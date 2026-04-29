/**
 * TS-AST → IR construction (pure path, value-position).
 *
 * Native IR construction is exhaustive: surface forms that don't
 * match any recognizer below return an `unsupported` result so
 * future milestones can target a concrete failure rather than a
 * hidden OpaqueExpr embedding.
 *
 * See `ir.ts` and CLAUDE.md §IR for the form table.
 */

import ts from "typescript";
import {
  type IRExpr,
  type IRExprEach,
  type IRFoldCombiner,
  irAppName,
  irBinop,
  irComb,
  irCond,
  irEach,
  irLitBool,
  irLitNat,
  irLitString,
  irVar,
} from "./ir.js";
import {
  buildL1MemberAccess,
  computedElementAccessUnsupportedReason,
  elementAccessLiteralKey,
  isArrayChainCall,
  type L1BuildContext,
  tryBuildL1Cardinality,
  tryBuildL1PureSubExpression,
  unwrapParens,
} from "./ir1-build.js";
import { lowerL1Expr } from "./ir1-lower.js";
import {
  type NullishTranslate,
  recognizeNullishForm,
  unwrapTransparentExpression,
} from "./nullish-recognizer.js";
import { isStaticallyBoolTyped } from "./purity.js";
import {
  allocComprehensionBinder,
  ambiguousFieldMsg,
  expressionReferencesNames,
  freshHygienicBinder,
  isNullableTsType,
  qualifyFieldAccess,
  type UniqueSupply,
} from "./translate-body.js";
import {
  cellRegisterMap,
  isMapType,
  isSetType,
  lookupMapKV,
  mapTsType,
  type NumericStrategy,
} from "./translate-types.js";

interface PendingIRComprehension {
  binder: string;
  src: IRExpr;
  guards: IRExpr[];
  proj: IRExpr;
}

interface IRBuildValue {
  expr: IRExpr;
  pendingComprehension?: PendingIRComprehension;
}

interface ReduceOpInfo {
  combiner: IRFoldCombiner;
  outer: "add" | "sub" | "mul" | "div" | "and" | "or";
  identityText: string | null;
  commutative: boolean;
  operandType: "number" | "boolean";
}

function binopToReduceInfo(kind: ts.SyntaxKind): ReduceOpInfo | null {
  switch (kind) {
    case ts.SyntaxKind.PlusToken:
      return {
        combiner: "add",
        outer: "add",
        identityText: "0",
        commutative: true,
        operandType: "number",
      };
    case ts.SyntaxKind.MinusToken:
      return {
        combiner: "add",
        outer: "sub",
        identityText: null,
        commutative: false,
        operandType: "number",
      };
    case ts.SyntaxKind.AsteriskToken:
      return {
        combiner: "mul",
        outer: "mul",
        identityText: "1",
        commutative: true,
        operandType: "number",
      };
    case ts.SyntaxKind.SlashToken:
      return {
        combiner: "mul",
        outer: "div",
        identityText: null,
        commutative: false,
        operandType: "number",
      };
    case ts.SyntaxKind.AmpersandAmpersandToken:
      return {
        combiner: "and",
        outer: "and",
        identityText: "true",
        commutative: true,
        operandType: "boolean",
      };
    case ts.SyntaxKind.BarBarToken:
      return {
        combiner: "or",
        outer: "or",
        identityText: "false",
        commutative: true,
        operandType: "boolean",
      };
    default:
      return null;
  }
}

function evaluateNumericLiteral(node: ts.Expression): number | null {
  node = unwrapParens(node) as ts.Expression;
  if (ts.isNumericLiteral(node)) {
    const n = Number(node.text);
    return Number.isFinite(n) ? n : null;
  }
  if (
    ts.isPrefixUnaryExpression(node) &&
    (node.operator === ts.SyntaxKind.PlusToken ||
      node.operator === ts.SyntaxKind.MinusToken) &&
    ts.isNumericLiteral(node.operand)
  ) {
    const n = Number(node.operand.text);
    if (!Number.isFinite(n)) {
      return null;
    }
    return node.operator === ts.SyntaxKind.MinusToken ? -n : n;
  }
  return null;
}

function isIdentityInit(node: ts.Expression, identityText: string): boolean {
  const inner = unwrapParens(node) as ts.Expression;
  if (identityText === "true") {
    return inner.kind === ts.SyntaxKind.TrueKeyword;
  }
  if (identityText === "false") {
    return inner.kind === ts.SyntaxKind.FalseKeyword;
  }
  const n = evaluateNumericLiteral(inner);
  return n !== null && n === Number(identityText);
}

function substituteIR(expr: IRExpr, name: string, replacement: IRExpr): IRExpr {
  switch (expr.kind) {
    case "var":
      return expr.name === name && !expr.primed ? replacement : expr;
    case "lit":
      return expr;
    case "app":
      return {
        ...expr,
        head:
          expr.head.kind === "expr"
            ? {
                kind: "expr",
                expr: substituteIR(expr.head.expr, name, replacement),
              }
            : expr.head,
        args: expr.args.map((arg) => substituteIR(arg, name, replacement)),
      };
    case "cond": {
      const arms: Array<[IRExpr, IRExpr]> = expr.arms.map(([g, v]) => [
        substituteIR(g, name, replacement),
        substituteIR(v, name, replacement),
      ]);
      const otherwise =
        expr.otherwise === undefined
          ? undefined
          : substituteIR(expr.otherwise, name, replacement);
      return irCond(arms, otherwise);
    }
    case "let":
      return {
        kind: "let",
        name: expr.name,
        value: substituteIR(expr.value, name, replacement),
        body:
          expr.name === name
            ? expr.body
            : substituteIR(expr.body, name, replacement),
      };
    case "each":
      return {
        ...expr,
        src: substituteIR(expr.src, name, replacement),
        guards:
          expr.binder === name
            ? expr.guards
            : expr.guards.map((guard) =>
                substituteIR(guard, name, replacement),
              ),
        proj:
          expr.binder === name
            ? expr.proj
            : substituteIR(expr.proj, name, replacement),
      };
    case "comb":
      if (expr.combiner === "min" || expr.combiner === "max") {
        return {
          kind: "comb",
          combiner: expr.combiner,
          each: substituteIR(expr.each, name, replacement) as IRExprEach,
        };
      }
      if ("init" in expr && expr.init !== undefined) {
        return {
          kind: "comb",
          combiner: expr.combiner,
          init: substituteIR(expr.init, name, replacement),
          each: substituteIR(expr.each, name, replacement) as IRExprEach,
        };
      }
      return {
        kind: "comb",
        combiner: expr.combiner,
        each: substituteIR(expr.each, name, replacement) as IRExprEach,
      };
    case "comb-typed":
      return expr.binder === name
        ? expr
        : {
            ...expr,
            guards: expr.guards.map((guard) =>
              substituteIR(guard, name, replacement),
            ),
            proj: substituteIR(expr.proj, name, replacement),
          };
    case "forall":
    case "exists":
      if (expr.binder === name) {
        return expr;
      }
      if (expr.guard !== undefined) {
        return {
          ...expr,
          guard: substituteIR(expr.guard, name, replacement),
          body: substituteIR(expr.body, name, replacement),
        };
      }
      return {
        ...expr,
        body: substituteIR(expr.body, name, replacement),
      };
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      throw new Error("unreachable: IRExpr");
    }
  }
}

function materializeBuildValue(value: IRBuildValue): IRExpr {
  if (value.pendingComprehension === undefined) {
    return value.expr;
  }
  const pending = value.pendingComprehension;
  return irEach(pending.binder, pending.src, pending.guards, pending.proj);
}

function extractArrowExpressionBody(
  fn: ts.Expression,
  expectedParams: number,
): ts.ArrowFunction | { unsupported: string } {
  if (!ts.isArrowFunction(fn)) {
    return { unsupported: "array callback must be an arrow function" };
  }
  if (fn.modifiers?.some((m) => m.kind === ts.SyntaxKind.AsyncKeyword)) {
    return { unsupported: "array callback must not be async" };
  }
  if (fn.parameters.length !== expectedParams) {
    return {
      unsupported:
        expectedParams === 1
          ? "filter/map callback must have exactly one identifier parameter"
          : "reduce callback must take exactly (acc, x)",
    };
  }
  for (const param of fn.parameters) {
    if (
      !ts.isIdentifier(param.name) ||
      param.initializer !== undefined ||
      param.dotDotDotToken !== undefined ||
      param.questionToken !== undefined
    ) {
      return {
        unsupported:
          "array callback parameters must be plain identifiers (no defaults/rest/optional)",
      };
    }
  }
  if (ts.isBlock(fn.body)) {
    const stmts = fn.body.statements;
    if (
      stmts.length !== 1 ||
      !ts.isReturnStatement(stmts[0]!) ||
      !stmts[0]!.expression
    ) {
      return {
        unsupported: "array callback block body must be a single return",
      };
    }
  }
  return fn;
}

function callbackBody(fn: ts.ArrowFunction): ts.Expression {
  if (ts.isBlock(fn.body)) {
    return (fn.body.statements[0] as ts.ReturnStatement).expression!;
  }
  return fn.body;
}

function getArrayElementTypesFromType(
  sourceType: ts.Type,
  checker: ts.TypeChecker,
): readonly ts.Type[] | null {
  if (sourceType.isUnion()) {
    const elementTypes: ts.Type[] = [];
    for (const member of sourceType.types) {
      const memberElementTypes = getArrayElementTypesFromType(member, checker);
      if (memberElementTypes === null) {
        return null;
      }
      elementTypes.push(...memberElementTypes);
    }
    return elementTypes.length > 0 ? elementTypes : null;
  }
  if (checker.isArrayType(sourceType) || checker.isTupleType(sourceType)) {
    const typeArgs = checker.getTypeArguments(sourceType as ts.TypeReference);
    return typeArgs.length > 0 ? typeArgs : null;
  }
  return null;
}

function getArrayElementTypes(
  tsExpr: ts.Expression,
  checker: ts.TypeChecker,
): readonly ts.Type[] | null {
  const sourceType = checker.getTypeAtLocation(tsExpr);
  return getArrayElementTypesFromType(sourceType, checker);
}

function arrayMethodCall(expr: ts.CallExpression): {
  methodName: "filter" | "map" | "reduce" | "reduceRight";
  receiver: ts.Expression;
} | null {
  const callee = expr.expression;
  if (ts.isPropertyAccessExpression(callee)) {
    const methodName = callee.name.text;
    if (
      methodName === "filter" ||
      methodName === "map" ||
      methodName === "reduce" ||
      methodName === "reduceRight"
    ) {
      return { methodName, receiver: callee.expression };
    }
    return null;
  }
  if (ts.isElementAccessExpression(callee)) {
    const arg = callee.argumentExpression;
    if (ts.isStringLiteral(arg) || ts.isNoSubstitutionTemplateLiteral(arg)) {
      const methodName = arg.text;
      if (
        methodName === "filter" ||
        methodName === "map" ||
        methodName === "reduce" ||
        methodName === "reduceRight"
      ) {
        return { methodName, receiver: callee.expression };
      }
    }
  }
  return null;
}

function isStaticallyNumberTyped(
  expr: ts.Expression,
  checker: ts.TypeChecker,
): boolean {
  const isAllNumber = (type: ts.Type): boolean => {
    if (type.isUnion()) {
      return type.types.every(isAllNumber);
    }
    if (type.isIntersection()) {
      return type.types.every(isAllNumber);
    }
    return (type.flags & ts.TypeFlags.NumberLike) !== 0;
  };
  return isAllNumber(checker.getTypeAtLocation(expr));
}

function reduceOperandsMatchCombiner(
  left: ts.Expression,
  right: ts.Expression,
  info: ReduceOpInfo,
  checker: ts.TypeChecker,
): boolean {
  if (info.operandType === "boolean") {
    return (
      isStaticallyBoolTyped(left, checker) &&
      isStaticallyBoolTyped(right, checker)
    );
  }
  return (
    isStaticallyNumberTyped(left, checker) &&
    isStaticallyNumberTyped(right, checker)
  );
}

function buildIRValue(
  expr: ts.Expression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  supply: UniqueSupply,
): IRBuildValue | { unsupported: string } {
  expr = unwrapParens(expr) as ts.Expression;
  if (ts.isCallExpression(expr)) {
    const arrayResult = tryBuildArrayMethodValue(
      expr,
      checker,
      strategy,
      paramNames,
      supply,
    );
    if (arrayResult !== null) {
      return arrayResult;
    }
  }

  const ir = buildIR(expr, checker, strategy, paramNames, supply);
  if (isBuildUnsupported(ir)) {
    return ir;
  }
  return { expr: ir };
}

/**
 * Build `Unop(card, <arrayChainReceiver>)` directly at the L2 IR layer
 * for `<arrayChain>.length` / `<arrayChain>["length"]` (and `.size`
 * variants on Set chains). The L1 cardinality dispatch (`Unop(card, …)`
 * over an `IR1Expr` receiver) cannot represent an array-chain receiver
 * because L1 has no `each` form — `each` is L2-only. Building at L2
 * sidesteps the layer mismatch.
 *
 * Returns `null` when the expression is not `<arrayChain>.length` /
 * `<arrayChain>.size`, leaving the L1 cardinality / Member dispatch
 * unaffected.
 */
function tryBuildArrayChainCardinality(
  expr: ts.PropertyAccessExpression | ts.ElementAccessExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  supply: UniqueSupply,
): IRExpr | null {
  if ((expr.flags & ts.NodeFlags.OptionalChain) !== 0) {
    return null;
  }
  const propName = ts.isPropertyAccessExpression(expr)
    ? expr.name.text
    : elementAccessLiteralKey(expr);
  if (propName !== "length" && propName !== "size") {
    return null;
  }
  const receiverNode = unwrapParens(expr.expression) as ts.Expression;
  if (!ts.isCallExpression(receiverNode)) {
    return null;
  }
  if (!isArrayChainCall(receiverNode, checker)) {
    return null;
  }
  const receiverIR = buildIR(
    receiverNode,
    checker,
    strategy,
    paramNames,
    supply,
  );
  if (isBuildUnsupported(receiverIR)) {
    return null;
  }
  // Sanity check: cardinality only applies when the receiver lowers to
  // something whose Pant type is a list-lifted form. The array-chain
  // recognizer guarantees this — `each(...)` produces `[T]` — so we
  // accept without re-checking the type.
  return {
    kind: "app",
    head: { kind: "unop", op: "card" },
    args: [receiverIR],
  };
}

function tryBuildArrayMethodValue(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  supply: UniqueSupply,
): IRBuildValue | { unsupported: string } | null {
  const arrayMethod = arrayMethodCall(expr);
  if (arrayMethod === null) {
    return null;
  }
  const { methodName, receiver } = arrayMethod;
  if (methodName === "filter" || methodName === "map") {
    const result = buildArrayMapFilter(
      methodName,
      receiver,
      expr,
      checker,
      strategy,
      paramNames,
      supply,
    );
    if (result !== null) {
      return result;
    }
  } else {
    const result = buildArrayReduce(
      methodName,
      receiver,
      expr,
      checker,
      strategy,
      paramNames,
      supply,
    );
    if (result !== null) {
      return result;
    }
  }
  return null;
}

function buildArrayMapFilter(
  methodName: "filter" | "map",
  tsReceiver: ts.Expression,
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  supply: UniqueSupply,
): IRBuildValue | { unsupported: string } | null {
  if (getArrayElementTypes(tsReceiver, checker) === null) {
    return null;
  }
  if (expr.arguments.length !== 1) {
    return {
      unsupported: `.${methodName} callback must have exactly one argument`,
    };
  }
  const receiver = buildIRValue(
    tsReceiver,
    checker,
    strategy,
    paramNames,
    supply,
  );
  if ("unsupported" in receiver) {
    return receiver;
  }

  const arrow = extractArrowExpressionBody(expr.arguments[0]!, 1);
  if ("unsupported" in arrow) {
    return arrow;
  }
  const pending = receiver.pendingComprehension;
  const isComposing = pending !== undefined;
  const sourceBinder = isComposing
    ? pending.binder
    : allocComprehensionBinder(supply, "x");
  const callbackBinder = isComposing
    ? freshHygienicBinder(supply)
    : sourceBinder;
  const arrowParams = new Map(paramNames);
  arrowParams.set(
    (arrow.parameters[0]!.name as ts.Identifier).text,
    callbackBinder,
  );

  const rawBody = buildIR(
    callbackBody(arrow),
    checker,
    strategy,
    arrowParams,
    supply,
  );
  if (isBuildUnsupported(rawBody)) {
    return rawBody;
  }
  const body = isComposing
    ? substituteIR(rawBody, callbackBinder, pending.proj)
    : rawBody;

  if (methodName === "filter") {
    if (!isStaticallyBoolTyped(callbackBody(arrow), checker)) {
      return {
        unsupported: ".filter callback must return a boolean predicate",
      };
    }
    if (isComposing) {
      return {
        expr: pending.proj,
        pendingComprehension: {
          binder: pending.binder,
          src: pending.src,
          guards: [...pending.guards, body],
          proj: pending.proj,
        },
      };
    }
    return {
      expr: irVar(sourceBinder),
      pendingComprehension: {
        binder: sourceBinder,
        src: receiver.expr,
        guards: [body],
        proj: irVar(sourceBinder),
      },
    };
  }

  if (isComposing) {
    return {
      expr: body,
      pendingComprehension: {
        ...pending,
        proj: body,
      },
    };
  }
  return {
    expr: body,
    pendingComprehension: {
      binder: sourceBinder,
      src: receiver.expr,
      guards: [],
      proj: body,
    },
  };
}

function buildArrayReduce(
  methodName: "reduce" | "reduceRight",
  tsReceiver: ts.Expression,
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  supply: UniqueSupply,
): IRBuildValue | { unsupported: string } | null {
  if (expr.arguments.length !== 2) {
    return { unsupported: `.${methodName} requires an explicit initial value` };
  }
  if (getArrayElementTypes(tsReceiver, checker) === null) {
    return null;
  }
  const receiver = buildIRValue(
    tsReceiver,
    checker,
    strategy,
    paramNames,
    supply,
  );
  if ("unsupported" in receiver) {
    return receiver;
  }
  const arrow = extractArrowExpressionBody(expr.arguments[0]!, 2);
  if ("unsupported" in arrow) {
    return arrow;
  }
  const accName = (arrow.parameters[0]!.name as ts.Identifier).text;
  const xName = (arrow.parameters[1]!.name as ts.Identifier).text;
  const body = unwrapParens(callbackBody(arrow)) as ts.Expression;
  if (!ts.isBinaryExpression(body)) {
    return {
      unsupported: `.${methodName} callback body must be 'acc OP f(x)'`,
    };
  }
  const info = binopToReduceInfo(body.operatorToken.kind);
  if (info === null) {
    return {
      unsupported: `.${methodName} operator ${ts.SyntaxKind[body.operatorToken.kind]} has no combiner`,
    };
  }
  if (methodName === "reduceRight" && !info.commutative) {
    return { unsupported: ".reduceRight with non-commutative operator" };
  }
  const leftExpr = unwrapParens(body.left) as ts.Expression;
  const rightExpr = unwrapParens(body.right) as ts.Expression;
  const leftIsAcc = ts.isIdentifier(leftExpr) && leftExpr.text === accName;
  const rightIsAcc = ts.isIdentifier(rightExpr) && rightExpr.text === accName;
  let innerExpr: ts.Expression;
  if (leftIsAcc && !rightIsAcc) {
    innerExpr = body.right;
  } else if (rightIsAcc && !leftIsAcc) {
    if (!info.commutative) {
      return {
        unsupported: `.${methodName} with acc on the right of a non-commutative operator`,
      };
    }
    innerExpr = body.left;
  } else {
    return {
      unsupported: `.${methodName} callback must reference acc exactly once`,
    };
  }
  if (!reduceOperandsMatchCombiner(body.left, body.right, info, checker)) {
    return {
      unsupported: `.${methodName} operator ${ts.SyntaxKind[body.operatorToken.kind]} operands do not match ${info.operandType} combiner`,
    };
  }
  if (expressionReferencesNames(innerExpr, new Set([accName]))) {
    return {
      unsupported: `.${methodName} inner expression must not reference acc`,
    };
  }

  const pending = receiver.pendingComprehension;
  const xBinder = pending
    ? freshHygienicBinder(supply)
    : allocComprehensionBinder(supply, "x");
  const arrowParams = new Map(paramNames);
  arrowParams.set(xName, xBinder);
  const inner = buildIR(innerExpr, checker, strategy, arrowParams, supply);
  if (isBuildUnsupported(inner)) {
    return inner;
  }
  const proj = pending ? substituteIR(inner, xBinder, pending.proj) : inner;
  const each = (
    pending
      ? irEach(pending.binder, pending.src, pending.guards, proj)
      : irEach(xBinder, receiver.expr, [], proj)
  ) as IRExprEach;
  let folded = irComb(info.combiner, each);

  const initNode = expr.arguments[1]!;
  if (
    info.identityText === null ||
    !isIdentityInit(initNode, info.identityText)
  ) {
    const init = buildIR(initNode, checker, strategy, paramNames, supply);
    if (isBuildUnsupported(init)) {
      return init;
    }
    folded =
      info.outer === info.combiner
        ? irComb(info.combiner, each, init)
        : irBinop(info.outer, init, folded);
  }
  return { expr: folded };
}

function buildCollectionMembershipCall(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  supply: UniqueSupply,
  l1Ctx: L1BuildContext,
): IRExpr | { unsupported: string } | null {
  if (
    !ts.isPropertyAccessExpression(expr.expression) ||
    expr.arguments.length !== 1
  ) {
    return null;
  }

  const methodName = expr.expression.name.text;
  if (methodName !== "includes" && methodName !== "has") {
    return null;
  }

  const tsReceiver = expr.expression.expression;
  const receiverType = checker.getTypeAtLocation(tsReceiver);

  // Probe-time `UniqueSupply` isolation: defer building `arg` and the
  // receiver until the receiver shape commits this probe to a known
  // membership lowering. Returning `null` from a non-matching probe
  // must not advance `supply`, or deterministic-name allocation drifts
  // for downstream callers.
  const buildArg = (): IRExpr | { unsupported: string } =>
    buildIR(expr.arguments[0]!, checker, strategy, paramNames, supply);

  if (methodName === "includes") {
    if (!checker.isArrayType(receiverType)) {
      return null;
    }
    const arg = buildArg();
    if (isBuildUnsupported(arg)) {
      return arg;
    }
    const receiver = buildIR(tsReceiver, checker, strategy, paramNames, supply);
    if (isBuildUnsupported(receiver)) {
      return receiver;
    }
    return irBinop("in", arg, receiver);
  }

  if (isSetType(receiverType)) {
    const arg = buildArg();
    if (isBuildUnsupported(arg)) {
      return arg;
    }
    const receiver = buildIR(tsReceiver, checker, strategy, paramNames, supply);
    if (isBuildUnsupported(receiver)) {
      return receiver;
    }
    return irBinop("in", arg, receiver);
  }

  if (!isMapType(receiverType)) {
    return null;
  }

  const normalizedReceiver = unwrapTransparentExpression(tsReceiver);
  const isMemberSurface =
    (ts.isPropertyAccessExpression(normalizedReceiver) &&
      (normalizedReceiver.flags & ts.NodeFlags.OptionalChain) === 0) ||
    (ts.isElementAccessExpression(normalizedReceiver) &&
      (normalizedReceiver.flags & ts.NodeFlags.OptionalChain) === 0 &&
      elementAccessLiteralKey(normalizedReceiver) !== null);
  if (isMemberSurface) {
    const arg = buildArg();
    if (isBuildUnsupported(arg)) {
      return arg;
    }
    const member = buildL1MemberAccess(
      normalizedReceiver as
        | ts.PropertyAccessExpression
        | ts.ElementAccessExpression,
      l1Ctx,
      { nativeReceiverLeaf: true },
    );
    if ("unsupported" in member) {
      return member;
    }
    if (member.kind !== "member") {
      return {
        unsupported: "Map .has receiver did not resolve to canonical L1 Member",
      };
    }
    return irAppName(`${member.name}-key`, [lowerL1Expr(member.receiver), arg]);
  }

  const typeArgs = checker.getTypeArguments(receiverType as ts.TypeReference);
  if (typeArgs.length !== 2) {
    return { unsupported: "Map with unexpected arity" };
  }
  const kType = mapTsType(typeArgs[0]!, checker, strategy, supply.synthCell);
  const vType = mapTsType(typeArgs[1]!, checker, strategy, supply.synthCell);
  let info = supply.synthCell
    ? lookupMapKV(supply.synthCell.synth, kType, vType)
    : undefined;
  if (!info && supply.synthCell) {
    cellRegisterMap(supply.synthCell, kType, vType);
    info = lookupMapKV(supply.synthCell.synth, kType, vType);
  }
  if (!info) {
    return {
      unsupported: `Map<${kType}, ${vType}>: key or value type cannot be mangled into a synthesized domain name`,
    };
  }
  const arg = buildArg();
  if (isBuildUnsupported(arg)) {
    return arg;
  }
  const receiver = buildIR(tsReceiver, checker, strategy, paramNames, supply);
  if (isBuildUnsupported(receiver)) {
    return receiver;
  }
  return irAppName(info.names.keyPred, [receiver, arg]);
}

/**
 * Translate a TypeScript expression to IR.
 *
 * Returns either an `IRExpr` (success) or `{ unsupported: string }` to
 * propagate translation rejections through the existing convention.
 *
 * Surface forms that don't match any recognizer below are reported as
 * `unsupported` — there is no legacy fallback into a wrapped
 * `OpaqueExpr` after M6.
 */
export function buildIR(
  expr: ts.Expression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  supply: UniqueSupply,
): IRExpr | { unsupported: string } {
  // Universal L1-layering: strip `(…)` wrappers at the build dispatch
  // entry so downstream recognizers see canonical shapes. Type-erasure
  // wrappers (`as T`, `!`, `<T>x`, `satisfies`) are NOT stripped — they
  // change the TS-checker type of the inner node and are load-bearing
  // for type-dependent dispatch like `qualifyFieldAccess`.
  expr = unwrapParens(expr) as ts.Expression;

  // L1 build context for sub-dispatchers (cardinality / Member). Pure
  // path has no symbolic state.
  const l1Ctx: L1BuildContext = {
    checker,
    strategy,
    paramNames,
    state: undefined,
    supply,
  };

  if (ts.isCallExpression(expr)) {
    const built = tryBuildArrayMethodValue(
      expr,
      checker,
      strategy,
      paramNames,
      supply,
    );
    if (built !== null) {
      if ("unsupported" in built) {
        return built;
      }
      return materializeBuildValue(built);
    }
  }

  if (ts.isCallExpression(expr)) {
    const membership = buildCollectionMembershipCall(
      expr,
      checker,
      strategy,
      paramNames,
      supply,
      l1Ctx,
    );
    if (membership !== null) {
      return membership;
    }
  }

  // Boolean keywords
  if (expr.kind === ts.SyntaxKind.TrueKeyword) {
    return irLitBool(true);
  }
  if (expr.kind === ts.SyntaxKind.FalseKeyword) {
    return irLitBool(false);
  }

  // Non-negative numeric literal: nat. Negative literals require
  // `Unop(Neg, ...)` lowering; they fall through to the unsupported
  // result below.
  if (ts.isNumericLiteral(expr)) {
    const n = Number(expr.text);
    if (Number.isFinite(n) && Number.isInteger(n) && n >= 0) {
      return irLitNat(n);
    }
  }

  // String literal
  if (ts.isStringLiteral(expr) || ts.isNoSubstitutionTemplateLiteral(expr)) {
    return irLitString(expr.text);
  }

  // Plain identifier — resolve through the param-name map (which renames
  // TS parameter names to Pant names) so a `Var(name)` in IR refers to
  // the Pant-side name. Mirrors the existing identifier-translation site
  // in translate-body.ts.
  if (ts.isIdentifier(expr)) {
    const renamed = paramNames.get(expr.text) ?? expr.text;
    return irVar(renamed);
  }

  // M5: cardinality dispatch (`.length` / `.size` → `Unop(card, x)`).
  // Pant's primitive for list cardinality is `#x`, not a `length` / `size`
  // rule application — routing through Member would produce a dangling
  // EUF function distinct from the actual cardinality. Fires before the
  // Member dispatch below for the six list-shaped TS types.
  if (ts.isPropertyAccessExpression(expr)) {
    const arrayChainCard = tryBuildArrayChainCardinality(
      expr,
      checker,
      strategy,
      paramNames,
      supply,
    );
    if (arrayChainCard !== null) {
      return arrayChainCard;
    }
    const card = tryBuildL1Cardinality(expr, l1Ctx, {
      nativeReceiverLeaf: true,
    });
    if (card !== null) {
      return lowerL1Expr(card);
    }
  }

  if (ts.isElementAccessExpression(expr)) {
    const inOptionalChain = (expr.flags & ts.NodeFlags.OptionalChain) !== 0;
    if (!inOptionalChain) {
      const arrayChainCard = tryBuildArrayChainCardinality(
        expr,
        checker,
        strategy,
        paramNames,
        supply,
      );
      if (arrayChainCard !== null) {
        return arrayChainCard;
      }
      const card = tryBuildL1Cardinality(expr, l1Ctx, {
        nativeReceiverLeaf: true,
      });
      if (card !== null) {
        return lowerL1Expr(card);
      }
    }
    if (inOptionalChain) {
      const prop = elementAccessLiteralKey(expr);
      if (prop === null) {
        return { unsupported: computedElementAccessUnsupportedReason };
      }
      let shouldLift = false;
      if (expr.questionDotToken !== undefined) {
        const receiverTsType = checker.getTypeAtLocation(expr.expression);
        shouldLift = isNullableTsType(receiverTsType);
      } else if (ts.isOptionalChain(expr.expression)) {
        shouldLift = true;
      }
      if (shouldLift) {
        const receiverTsType = checker.getTypeAtLocation(expr.expression);
        const ruleName = qualifyFieldAccess(
          receiverTsType,
          prop,
          checker,
          strategy,
          supply.synthCell,
        );
        if (ruleName === null) {
          return { unsupported: ambiguousFieldMsg(prop) };
        }
        const innerIR = buildIR(
          expr.expression,
          checker,
          strategy,
          paramNames,
          supply,
        );
        if (isBuildUnsupported(innerIR)) {
          return innerIR;
        }
        const binderName = allocComprehensionBinder(supply, "n");
        return irEach(
          binderName,
          innerIR,
          [],
          irAppName(ruleName, [irVar(binderName)]),
        );
      }
    }
  }

  if (
    (ts.isPropertyAccessExpression(expr) ||
      ts.isElementAccessExpression(expr)) &&
    (expr.flags & ts.NodeFlags.OptionalChain) === 0
  ) {
    const member = buildL1MemberAccess(expr, l1Ctx, {
      nativeReceiverLeaf: true,
    });
    if ("unsupported" in member) {
      return member;
    }
    return lowerL1Expr(member);
  }

  if (ts.isBinaryExpression(expr)) {
    const nullishTranslate: NullishTranslate = (sub) => {
      const subL1 = tryBuildL1PureSubExpression(sub, l1Ctx);
      if (subL1 === null) {
        return { unsupported: `unsupported nullish operand: ${sub.getText()}` };
      }
      return subL1;
    };
    const recognized = recognizeNullishForm(expr, checker, nullishTranslate);
    if (recognized !== null) {
      if ("unsupported" in recognized) {
        return { unsupported: recognized.unsupported };
      }
      return lowerL1Expr(recognized);
    }
  }

  const nativeL1 = tryBuildL1PureSubExpression(expr, l1Ctx);
  if (nativeL1 !== null) {
    if ("unsupported" in nativeL1) {
      return nativeL1;
    }
    return lowerL1Expr(nativeL1);
  }

  // Stage 2: Optional chain `x?.prop` (and tail of a chain marked by
  // NodeFlags.OptionalChain) under list-lift. With `x: [T]` on the Pant
  // side, the functor-lift encoding is `each $n in x | prop $n`, giving
  // `[U]` — empty when x is null/empty, singleton when x is present.
  // Chains compose as nested Each comprehensions: `x?.a.b` becomes
  // `Each($n', Each($n, x, [], a $n), [], b $n')`. When the receiver is
  // not nullable in TS, `?.` degenerates to `.` and falls through.
  if (ts.isPropertyAccessExpression(expr)) {
    const inOptionalChain = (expr.flags & ts.NodeFlags.OptionalChain) !== 0;
    if (inOptionalChain) {
      let shouldLift = false;
      if (expr.questionDotToken !== undefined) {
        const receiverTsType = checker.getTypeAtLocation(expr.expression);
        shouldLift = isNullableTsType(receiverTsType);
      } else if (ts.isOptionalChain(expr.expression)) {
        shouldLift = true;
      }
      if (shouldLift) {
        const prop = expr.name.text;
        const receiverTsType = checker.getTypeAtLocation(expr.expression);
        const ruleName = qualifyFieldAccess(
          receiverTsType,
          prop,
          checker,
          strategy,
          supply.synthCell,
        );
        if (ruleName === null) {
          return { unsupported: ambiguousFieldMsg(prop) };
        }
        const innerIR = buildIR(
          expr.expression,
          checker,
          strategy,
          paramNames,
          supply,
        );
        if (isBuildUnsupported(innerIR)) {
          return innerIR;
        }
        const binderName = allocComprehensionBinder(supply, "n");
        return irEach(
          binderName,
          innerIR,
          [],
          irAppName(ruleName, [irVar(binderName)]),
        );
      }
    }
  }

  // Native expression construction has been exhausted. Surface forms
  // that don't match any recognizer above are reported as `unsupported`
  // so future milestones can target a concrete failure rather than a
  // hidden OpaqueExpr embedding.
  return {
    unsupported: `unsupported pure expression form: ${ts.SyntaxKind[expr.kind]}`,
  };
}

export function isBuildUnsupported(
  r: IRExpr | { unsupported: string },
): r is { unsupported: string } {
  return "unsupported" in r;
}
