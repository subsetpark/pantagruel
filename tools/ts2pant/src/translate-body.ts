import type { SourceFile } from "ts-morph";
import ts from "typescript";
import {
  Apply,
  Binop,
  Cardinality,
  Comprehension,
  Cond,
  Equation,
  Membership,
  type PantExpr,
  type PantProp,
  PrimedApply,
  Unop,
  Unsupported,
  UnsupportedProp,
  Var,
} from "./pant-expr.js";
import {
  classifyFunction,
  findFunction,
  isAssertionCall,
  isFollowableGuardCall,
  isPureExpression,
  shortParamName,
  translateExpr,
  translateOperator,
} from "./translate-signature.js";
import { mapTsType, type NumericStrategy } from "./translate-types.js";
import type { PantDeclaration } from "./types.js";

/** Generate a binder name not already used by params. */
function freshBinder(paramNames: Map<string, string>): string {
  const used = new Set(paramNames.values());
  for (const candidate of ["x", "y", "z", "w", "v", "u", "t"]) {
    if (!used.has(candidate)) {
      return candidate;
    }
  }
  let i = 0;
  while (used.has(`x${i}`)) {
    i++;
  }
  return `x${i}`;
}

/** Replace every Var(name) in expr with replacement (for composing comprehension chains). */
function substituteBinder(
  expr: PantExpr,
  name: string,
  replacement: PantExpr,
): PantExpr {
  switch (expr.kind) {
    case "var":
      return expr.name === name ? replacement : expr;
    case "literal":
    case "unsupported":
      return expr;
    case "apply":
      return {
        ...expr,
        args: expr.args.map((a) => substituteBinder(a, name, replacement)),
      };
    case "primed-apply":
      return {
        ...expr,
        args: expr.args.map((a) => substituteBinder(a, name, replacement)),
      };
    case "binop":
      return {
        ...expr,
        left: substituteBinder(expr.left, name, replacement),
        right: substituteBinder(expr.right, name, replacement),
      };
    case "unop":
      return {
        ...expr,
        operand: substituteBinder(expr.operand, name, replacement),
      };
    case "cardinality":
      return { ...expr, expr: substituteBinder(expr.expr, name, replacement) };
    case "membership":
      return {
        ...expr,
        element: substituteBinder(expr.element, name, replacement),
        collection: substituteBinder(expr.collection, name, replacement),
      };
    case "cond":
      return {
        ...expr,
        arms: expr.arms.map((a) => ({
          guard: substituteBinder(a.guard, name, replacement),
          value: substituteBinder(a.value, name, replacement),
        })),
        fallback: substituteBinder(expr.fallback, name, replacement),
      };
    case "comprehension":
      if (expr.binder === name) {
        return expr;
      }
      return Comprehension(
        expr.binder,
        expr.type,
        substituteBinder(expr.body, name, replacement),
        expr.predicate
          ? substituteBinder(expr.predicate, name, replacement)
          : undefined,
      );
    default:
      throw new Error(`Unhandled expr kind: ${(expr as PantExpr).kind}`);
  }
}

export interface TranslateBodyOptions {
  sourceFile: SourceFile;
  functionName: string;
  strategy: NumericStrategy;
  /** Declarations in scope — used for frame condition generation. */
  declarations?: PantDeclaration[];
}

/**
 * Translate a TypeScript function body to Pantagruel propositions.
 *
 * Pure functions: return expression becomes `all params | f args = <expr>`.
 * Mutating functions: property assignments become primed propositions,
 * plus frame conditions for unmodified rules.
 */
export function translateBody(opts: TranslateBodyOptions): PantProp[] {
  const { sourceFile, functionName, strategy, declarations } = opts;
  const checker = sourceFile.getProject().getTypeChecker().compilerObject;
  const { node, className } = findFunction(sourceFile, functionName);
  // Strip class qualifier for use in Pantagruel identifiers
  const baseName = functionName.includes(".")
    ? functionName.split(".", 2)[1]!
    : functionName;
  const classification = classifyFunction(node, checker);

  // Build param name map (same logic as translateSignature)
  const paramNames = new Map<string, string>();
  const paramList: Array<{ name: string; type: string }> = [];

  const sig = checker.getSignatureFromDeclaration(node);

  if (className) {
    const existingParamNames = new Set(
      sig ? sig.getParameters().map((p) => p.name) : [],
    );
    const pName = shortParamName(className, existingParamNames);
    paramNames.set("this", pName);
    paramList.push({ name: pName, type: className });
  }

  if (sig) {
    for (const param of sig.getParameters()) {
      const paramType = checker.getTypeOfSymbol(param);
      const typeName = mapTsType(paramType, checker, strategy);
      paramNames.set(param.name, param.name);
      paramList.push({ name: param.name, type: typeName });
    }
  }

  if (!node.body) {
    return [];
  }

  if (classification === "pure") {
    return translatePureBody(
      node,
      baseName,
      paramList,
      checker,
      strategy,
      paramNames,
    );
  } else {
    return translateMutatingBody(
      node,
      checker,
      strategy,
      paramNames,
      declarations ?? [],
    );
  }
}

function translatePureBody(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  functionName: string,
  params: Array<{ name: string; type: string }>,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): PantProp[] {
  if (!node.body) {
    return [];
  }

  const returnExpr = extractReturnExpression(node.body, checker);
  if (!returnExpr) {
    const reason = describeRejectedBody(node.body, checker);
    return [UnsupportedProp(`${functionName} — ${reason}`)];
  }

  const body = translateBodyExpr(returnExpr, checker, strategy, paramNames);

  if (body.kind === "unsupported") {
    return [UnsupportedProp(body.reason)];
  }

  const argExprs = params.map((p) => Var(p.name));
  const lhs =
    argExprs.length > 0
      ? Apply(functionName, ...argExprs)
      : Apply(functionName);
  const quantifiers = params.map((p) => ({ name: p.name, type: p.type }));
  return [Equation(quantifiers, lhs, body)];
}

/**
 * Extract the return expression from a function body.
 * Handles:
 *   - Single return statement
 *   - if/else with returns in both branches (produces a synthetic conditional)
 */
function extractReturnExpression(
  body: ts.Block,
  checker: ts.TypeChecker,
): ts.Expression | ts.IfStatement | null {
  // Skip guard statements (if-throw patterns and assertion calls)
  const stmts = body.statements.filter((s) => !isGuardStatement(s, checker));

  if (stmts.length === 1) {
    const stmt = stmts[0]!;
    if (ts.isReturnStatement(stmt) && stmt.expression) {
      return stmt.expression;
    }
    // if/else with returns
    if (ts.isIfStatement(stmt) && stmt.elseStatement) {
      return stmt;
    }
  }

  // Multiple non-guard statements are not representable yet without
  // translating local bindings/control flow.
  if (stmts.length > 1) {
    return null;
  }

  return null;
}

function describeRejectedBody(body: ts.Block, checker: ts.TypeChecker): string {
  const stmts = body.statements.filter((s) => !isGuardStatement(s, checker));
  if (stmts.length === 0) {
    return "empty body";
  }
  if (stmts.length > 1) {
    return "local bindings or multiple statements before return";
  }
  const stmt = stmts[0]!;
  if (ts.isReturnStatement(stmt) && !stmt.expression) {
    return "return without expression";
  }
  return "non-translatable control flow";
}

function isGuardStatement(
  stmt: ts.Statement,
  checker: ts.TypeChecker,
): boolean {
  // Assertion call or followable guard call — must match the same purity
  // checks used by scanBodyForGuards in translate-signature.ts
  if (ts.isExpressionStatement(stmt) && ts.isCallExpression(stmt.expression)) {
    const call = stmt.expression;
    if (!isPureExpression(call.expression)) {
      return false;
    }
    if (!call.arguments.every(isPureExpression)) {
      return false;
    }
    if (isAssertionCall(checker, call) !== null) {
      return true;
    }
    if (isFollowableGuardCall(call, checker)) {
      return true;
    }
  }

  if (!ts.isIfStatement(stmt)) {
    return false;
  }
  // if (...) { throw } without else
  if (!stmt.elseStatement && blockThrows(stmt.thenStatement)) {
    return !expressionHasSideEffects(stmt.expression);
  }
  // if (...) { ... } else { throw }
  if (stmt.elseStatement && blockThrows(stmt.elseStatement)) {
    // Only a guard if the condition and then-block have no side effects.
    // A side-effectful condition like `if (audit(a)) { throw ... }` must not be
    // skipped. A mutating then-branch like `if (ok) { a.balance = 1; } else { throw e; }`
    // must NOT be classified as a guard — collectAssignments() needs to see it.
    return (
      !expressionHasSideEffects(stmt.expression) &&
      blockHasNoSideEffects(stmt.thenStatement) &&
      !blockReturns(stmt.thenStatement)
    );
  }
  return false;
}

function variableStatementHasNoSideEffects(
  stmt: ts.VariableStatement,
): boolean {
  return stmt.declarationList.declarations.every(
    (decl) => !decl.initializer || !expressionHasSideEffects(decl.initializer),
  );
}

function blockThrows(node: ts.Statement): boolean {
  if (ts.isThrowStatement(node)) {
    return true;
  }
  if (ts.isBlock(node)) {
    const stmts = node.statements;
    if (stmts.length === 0) {
      return false;
    }
    // Last statement must be a throw; all preceding must be side-effect-free
    // (variable declarations for building the error message, etc.)
    if (!ts.isThrowStatement(stmts[stmts.length - 1]!)) {
      return false;
    }
    return stmts
      .slice(0, -1)
      .every(
        (s) =>
          ts.isVariableStatement(s) && variableStatementHasNoSideEffects(s),
      );
  }
  return false;
}

function blockReturns(node: ts.Statement): boolean {
  if (ts.isBlock(node)) {
    return node.statements.some((s) => ts.isReturnStatement(s));
  }
  return ts.isReturnStatement(node);
}

/** Check that a statement/block contains no assignments or property writes. */
function blockHasNoSideEffects(node: ts.Statement): boolean {
  if (ts.isBlock(node)) {
    return node.statements.every((s) => blockHasNoSideEffects(s));
  }
  if (ts.isExpressionStatement(node)) {
    return !expressionHasSideEffects(node.expression);
  }
  // Variable declarations are fine only if initializers have no side effects
  if (ts.isVariableStatement(node)) {
    return variableStatementHasNoSideEffects(node);
  }
  // Return statements, throw statements are fine
  if (ts.isReturnStatement(node) || ts.isThrowStatement(node)) {
    return true;
  }
  // if/for/while/switch may contain mutations — treat as side-effectful
  return false;
}

/** Unwrap parentheses, type assertions, and non-null assertions to get the inner expression. */
function unwrapExpression(expr: ts.Expression): ts.Expression {
  while (
    ts.isParenthesizedExpression(expr) ||
    ts.isAsExpression(expr) ||
    ts.isSatisfiesExpression(expr) ||
    ts.isNonNullExpression(expr)
  ) {
    expr = expr.expression;
  }
  return expr;
}

function expressionHasSideEffects(expr: ts.Expression): boolean {
  expr = unwrapExpression(expr);

  if (ts.isDeleteExpression(expr)) {
    return true;
  }

  if (ts.isBinaryExpression(expr)) {
    // Any assignment operator
    return (
      (expr.operatorToken.kind >= ts.SyntaxKind.EqualsToken &&
        expr.operatorToken.kind <= ts.SyntaxKind.CaretEqualsToken) ||
      expressionHasSideEffects(expr.left) ||
      expressionHasSideEffects(expr.right)
    );
  }
  if (ts.isCallExpression(expr)) {
    return true;
  }
  if (ts.isNewExpression(expr) || ts.isAwaitExpression(expr)) {
    return true;
  }
  if (ts.isPrefixUnaryExpression(expr) || ts.isPostfixUnaryExpression(expr)) {
    const op = expr.operator;
    return (
      op === ts.SyntaxKind.PlusPlusToken ||
      op === ts.SyntaxKind.MinusMinusToken ||
      expressionHasSideEffects(expr.operand)
    );
  }
  return (
    ts.forEachChild(expr, (child) =>
      ts.isExpression(child) ? expressionHasSideEffects(child) : false,
    ) ?? false
  );
}

/**
 * Translate a TS expression to a PantExpr AST node, extending the base
 * translateExpr with support for ternary, array ops, and if/else as cond.
 */
export function translateBodyExpr(
  expr: ts.Expression | ts.Statement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): PantExpr {
  if (ts.isExpression(expr)) {
    expr = unwrapExpression(expr);
  }

  // if/else statement -> cond
  if (ts.isIfStatement(expr)) {
    return translateIfStatement(expr, checker, strategy, paramNames);
  }

  // Ternary: a ? b : c -> Cond([{guard: a, value: b}], c)
  if (ts.isConditionalExpression(expr)) {
    const cond = translateBodyExpr(
      expr.condition,
      checker,
      strategy,
      paramNames,
    );
    if (cond.kind === "unsupported") {
      return cond;
    }
    const whenTrue = translateBodyExpr(
      expr.whenTrue,
      checker,
      strategy,
      paramNames,
    );
    if (whenTrue.kind === "unsupported") {
      return whenTrue;
    }
    const whenFalse = translateBodyExpr(
      expr.whenFalse,
      checker,
      strategy,
      paramNames,
    );
    if (whenFalse.kind === "unsupported") {
      return whenFalse;
    }
    return Cond([{ guard: cond, value: whenTrue }], whenFalse);
  }

  // Property access with special array operations
  if (ts.isPropertyAccessExpression(expr)) {
    const prop = expr.name.text;
    const obj = translateBodyExpr(
      expr.expression,
      checker,
      strategy,
      paramNames,
    );
    if (obj.kind === "unsupported") {
      return obj;
    }
    // .length -> #obj (array only)
    if (prop === "length") {
      const receiverType = checker.getTypeAtLocation(expr.expression);
      if (checker.isArrayType(receiverType)) {
        return Cardinality(obj);
      }
    }
    // Regular property access: a.balance -> Apply("balance", obj)
    return Apply(prop, obj);
  }

  // Call expression: handle .includes(), .filter().map(), etc.
  if (ts.isCallExpression(expr)) {
    return translateCallExpr(expr, checker, strategy, paramNames);
  }

  // Prefix unary: !x -> Unop("~", x), -x -> Unop("-", x)
  if (ts.isPrefixUnaryExpression(expr)) {
    const operand = translateBodyExpr(
      expr.operand,
      checker,
      strategy,
      paramNames,
    );
    if (operand.kind === "unsupported") {
      return operand;
    }
    if (expr.operator === ts.SyntaxKind.ExclamationToken) {
      return Unop("~", operand);
    }
    if (expr.operator === ts.SyntaxKind.MinusToken) {
      return Unop("-", operand);
    }
  }

  // Binary expression
  if (ts.isBinaryExpression(expr)) {
    const op = translateOperator(expr.operatorToken.kind);
    if (op === "?") {
      return Unsupported(`operator ${ts.SyntaxKind[expr.operatorToken.kind]}`);
    }
    const left = translateBodyExpr(expr.left, checker, strategy, paramNames);
    if (left.kind === "unsupported") {
      return left;
    }
    const right = translateBodyExpr(expr.right, checker, strategy, paramNames);
    if (right.kind === "unsupported") {
      return right;
    }
    return Binop(op, left, right);
  }

  // Fall through to base translateExpr for identifiers, literals, this, etc.
  if (ts.isExpression(expr)) {
    return translateExpr(expr, checker, strategy, paramNames);
  }

  return Unsupported("non-expression statement");
}

function translateIfStatement(
  stmt: ts.IfStatement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): PantExpr {
  const cond = translateBodyExpr(
    stmt.expression,
    checker,
    strategy,
    paramNames,
  );
  if (cond.kind === "unsupported") {
    return cond;
  }
  const thenExpr = extractReturnFromBranch(stmt.thenStatement, checker);
  const elseExpr = stmt.elseStatement
    ? extractReturnFromBranch(stmt.elseStatement, checker)
    : null;

  if (thenExpr && elseExpr) {
    const thenVal = translateBodyExpr(thenExpr, checker, strategy, paramNames);
    if (thenVal.kind === "unsupported") {
      return thenVal;
    }
    const elseVal = translateBodyExpr(elseExpr, checker, strategy, paramNames);
    if (elseVal.kind === "unsupported") {
      return elseVal;
    }
    return Cond([{ guard: cond, value: thenVal }], elseVal);
  }

  return Unsupported("if statement without return in both branches");
}

function extractReturnFromBranch(
  stmt: ts.Statement,
  checker: ts.TypeChecker,
): ts.Expression | null {
  if (ts.isReturnStatement(stmt) && stmt.expression) {
    return stmt.expression;
  }
  if (ts.isBlock(stmt)) {
    // Apply the same rule as extractReturnExpression: only allow a single
    // return (after filtering guards). Blocks with local declarations or
    // multiple non-guard statements are rejected so we don't leak
    // branch-scoped bindings into the generated proposition.
    const nonGuard = stmt.statements.filter(
      (s) => !isGuardStatement(s, checker),
    );
    if (nonGuard.length === 1) {
      const s = nonGuard[0]!;
      if (ts.isReturnStatement(s) && s.expression) {
        return s.expression;
      }
    }
  }
  return null;
}

/** Get element type name for an array-typed TS expression, or null. */
function getArrayElementType(
  tsExpr: ts.Expression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
): string | null {
  const sourceType = checker.getTypeAtLocation(tsExpr);
  if (!checker.isArrayType(sourceType)) {
    return null;
  }
  const typeArgs = checker.getTypeArguments(sourceType as ts.TypeReference);
  return typeArgs.length === 1
    ? mapTsType(typeArgs[0]!, checker, strategy)
    : "?";
}

/**
 * Translate a .filter() or .map() call on an array to a comprehension,
 * composing with any existing comprehension from a prior chain step.
 */
function translateArrayMethod(
  methodName: "filter" | "map",
  tsReceiver: ts.Expression,
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): PantExpr | null {
  const elemType = getArrayElementType(tsReceiver, checker, strategy);
  if (!elemType) {
    return null;
  }

  const receiver = translateBodyExpr(tsReceiver, checker, strategy, paramNames);
  if (receiver.kind === "unsupported") {
    return receiver;
  }

  const isComposing = receiver.kind === "comprehension";
  const sourceBinder = isComposing ? receiver.binder : freshBinder(paramNames);
  // Use a fresh binder for the callback so it doesn't collide with sourceBinder
  const callbackBinder = isComposing
    ? freshBinder(new Map([...paramNames, [sourceBinder, sourceBinder]]))
    : sourceBinder;
  const extendedParams = new Map(paramNames);
  if (isComposing) {
    extendedParams.set(sourceBinder, sourceBinder);
  }
  extendedParams.set(callbackBinder, callbackBinder);

  const rawBody = extractArrowBody(
    expr.arguments[0]!,
    callbackBinder,
    extendedParams,
    checker,
    strategy,
  );
  if (!rawBody) {
    return Unsupported(expr.getText());
  }
  if (rawBody.kind === "unsupported") {
    return rawBody;
  }

  // When composing, substitute the callback's binder with the prior step's body
  // so that e.g. xs.map(f).map(g) becomes (each x: T | g(f(x))) not (each x: T | g(x))
  const body = isComposing
    ? substituteBinder(rawBody, callbackBinder, receiver.body)
    : rawBody;

  if (methodName === "filter") {
    if (isComposing) {
      const combined = receiver.predicate
        ? Binop("and", receiver.predicate, body)
        : body;
      return { ...receiver, predicate: combined };
    }
    return Comprehension(sourceBinder, elemType, Var(sourceBinder), body);
  } else {
    if (isComposing) {
      return { ...receiver, body };
    }
    return Comprehension(sourceBinder, elemType, body);
  }
}

function translateCallExpr(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): PantExpr {
  // Method calls: obj.method(args)
  if (ts.isPropertyAccessExpression(expr.expression)) {
    const methodName = expr.expression.name.text;
    const tsReceiver = expr.expression.expression;

    // .includes(x) -> x in obj (array only)
    if (methodName === "includes" && expr.arguments.length === 1) {
      const receiverType = checker.getTypeAtLocation(tsReceiver);
      if (!checker.isArrayType(receiverType)) {
        return Unsupported("non-array .includes()");
      }
      const arg = translateBodyExpr(
        expr.arguments[0]!,
        checker,
        strategy,
        paramNames,
      );
      if (arg.kind === "unsupported") {
        return arg;
      }
      const objExpr = translateBodyExpr(
        tsReceiver,
        checker,
        strategy,
        paramNames,
      );
      if (objExpr.kind === "unsupported") {
        return objExpr;
      }
      return Membership(arg, objExpr);
    }

    // .filter(pred) / .map(fn) — each independently produces or refines a comprehension
    if (
      (methodName === "filter" || methodName === "map") &&
      expr.arguments.length === 1
    ) {
      const result = translateArrayMethod(
        methodName,
        tsReceiver,
        expr,
        checker,
        strategy,
        paramNames,
      );
      if (result) {
        return result;
      }
    }
  }

  // Unsupported call
  return Unsupported(expr.getText());
}

function extractArrowBody(
  expr: ts.Expression,
  binderName: string,
  paramNames: Map<string, string>,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
): PantExpr | null {
  if (!ts.isArrowFunction(expr)) {
    return null;
  }
  if (
    expr.parameters.length !== 1 ||
    !ts.isIdentifier(expr.parameters[0]!.name)
  ) {
    return Unsupported(
      "filter/map callback must have exactly one identifier parameter",
    );
  }

  // Map arrow param to the fresh binder
  const param = expr.parameters[0]!;
  const arrowParams = new Map(paramNames);
  arrowParams.set((param.name as ts.Identifier).text, binderName);

  if (ts.isBlock(expr.body)) {
    // Only allow a single return (after filtering guards), same rule as
    // extractReturnExpression — blocks with locals or multiple statements
    // would introduce free variables in the generated comprehension.
    const nonGuard = expr.body.statements.filter(
      (s) => !isGuardStatement(s, checker),
    );
    if (nonGuard.length === 1) {
      const s = nonGuard[0]!;
      if (ts.isReturnStatement(s) && s.expression) {
        return translateBodyExpr(s.expression, checker, strategy, arrowParams);
      }
    }
    return null;
  }

  // Expression body
  return translateBodyExpr(expr.body, checker, strategy, arrowParams);
}

// --- Mutating function body translation ---

function translateMutatingBody(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  declarations: PantDeclaration[],
): PantProp[] {
  if (!node.body) {
    return [];
  }

  const propositions: PantProp[] = [];
  const modifiedRules = new Set<string>();

  // Collect property assignments
  const hasUnsupportedMutation = collectAssignments(
    node.body,
    checker,
    strategy,
    paramNames,
    propositions,
    modifiedRules,
  );

  // Only generate frame conditions when all mutation shapes were translatable;
  // unsupported control flow (if/loop/switch) makes frames unsound.
  if (!hasUnsupportedMutation) {
    const frames = generateFrameConditions(modifiedRules, declarations);
    propositions.push(...frames);
  }

  return propositions;
}

/**
 * Collect property assignments from a block. Returns true if any unsupported
 * mutating control flow (if/loop/switch) was encountered, signalling that
 * frame condition generation should be suppressed.
 */
function collectAssignments(
  body: ts.Block | ts.Statement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  propositions: PantProp[],
  modifiedRules: Set<string>,
): boolean {
  let hasUnsupportedMutation = false;
  const stmts = ts.isBlock(body) ? Array.from(body.statements) : [body];

  for (const stmt of stmts) {
    // Skip guard statements (if-throw patterns and assertion calls)
    if (isGuardStatement(stmt, checker)) {
      continue;
    }

    if (
      ts.isExpressionStatement(stmt) &&
      ts.isBinaryExpression(unwrapExpression(stmt.expression))
    ) {
      const bin = unwrapExpression(stmt.expression) as ts.BinaryExpression;
      if (
        bin.operatorToken.kind === ts.SyntaxKind.EqualsToken &&
        ts.isPropertyAccessExpression(bin.left)
      ) {
        const prop = bin.left.name.text;
        const obj = translateBodyExpr(
          bin.left.expression,
          checker,
          strategy,
          paramNames,
        );
        const val = translateBodyExpr(bin.right, checker, strategy, paramNames);
        if (obj.kind === "unsupported") {
          hasUnsupportedMutation = true;
          propositions.push(UnsupportedProp(obj.reason));
          continue;
        }
        if (val.kind === "unsupported") {
          hasUnsupportedMutation = true;
          propositions.push(UnsupportedProp(val.reason));
          continue;
        }
        propositions.push(Equation([], PrimedApply(prop, obj), val));
        modifiedRules.add(prop);
        continue;
      }
    }

    if (
      ts.isExpressionStatement(stmt) &&
      expressionHasSideEffects(stmt.expression)
    ) {
      propositions.push(UnsupportedProp("side-effectful expression"));
      hasUnsupportedMutation = true;
      continue;
    }

    if (
      ts.isVariableStatement(stmt) &&
      stmt.declarationList.declarations.some(
        (d) => d.initializer && expressionHasSideEffects(d.initializer),
      )
    ) {
      propositions.push(UnsupportedProp("side-effectful variable initializer"));
      hasUnsupportedMutation = true;
      continue;
    }

    if (
      (ts.isReturnStatement(stmt) || ts.isThrowStatement(stmt)) &&
      stmt.expression &&
      expressionHasSideEffects(stmt.expression)
    ) {
      propositions.push(
        UnsupportedProp("side-effectful control-flow expression"),
      );
      hasUnsupportedMutation = true;
      continue;
    }

    // Recurse into nested blocks
    if (ts.isBlock(stmt)) {
      if (
        collectAssignments(
          stmt,
          checker,
          strategy,
          paramNames,
          propositions,
          modifiedRules,
        )
      ) {
        hasUnsupportedMutation = true;
      }
    } else if (ts.isIfStatement(stmt)) {
      propositions.push(UnsupportedProp("conditional assignment (if/else)"));
      hasUnsupportedMutation = true;
    } else if (
      ts.isForStatement(stmt) ||
      ts.isForOfStatement(stmt) ||
      ts.isForInStatement(stmt) ||
      ts.isWhileStatement(stmt) ||
      ts.isDoStatement(stmt)
    ) {
      propositions.push(UnsupportedProp("loop assignment"));
      hasUnsupportedMutation = true;
    } else if (ts.isTryStatement(stmt)) {
      // try/catch branches are mutually exclusive; collecting from both would
      // produce contradictory conjunctions. Only the finally block executes
      // unconditionally.
      if (stmt.catchClause) {
        propositions.push(UnsupportedProp("try/catch assignment"));
        hasUnsupportedMutation = true;
      } else {
        if (
          collectAssignments(
            stmt.tryBlock,
            checker,
            strategy,
            paramNames,
            propositions,
            modifiedRules,
          )
        ) {
          hasUnsupportedMutation = true;
        }
      }
      if (stmt.finallyBlock) {
        if (
          collectAssignments(
            stmt.finallyBlock,
            checker,
            strategy,
            paramNames,
            propositions,
            modifiedRules,
          )
        ) {
          hasUnsupportedMutation = true;
        }
      }
    } else if (ts.isSwitchStatement(stmt)) {
      propositions.push(UnsupportedProp("switch assignment"));
      hasUnsupportedMutation = true;
    }
  }

  return hasUnsupportedMutation;
}

/**
 * Generate frame conditions: for each rule in declarations not explicitly
 * modified, emit `all x: T | rule' x = rule x`.
 */
function generateFrameConditions(
  modifiedRules: Set<string>,
  declarations: PantDeclaration[],
): PantProp[] {
  const frames: PantProp[] = [];

  for (const decl of declarations) {
    if (decl.kind !== "rule") {
      continue;
    }
    if (modifiedRules.has(decl.name)) {
      continue;
    }

    const paramArgs = decl.params.map((p) => Var(p.name));
    const lhs = PrimedApply(decl.name, ...paramArgs);
    const rhs =
      paramArgs.length > 0 ? Apply(decl.name, ...paramArgs) : Apply(decl.name);
    const quantifiers = decl.params.map((p) => ({
      name: p.name,
      type: p.type,
    }));
    frames.push(Equation(quantifiers, lhs, rhs));
  }

  return frames;
}
