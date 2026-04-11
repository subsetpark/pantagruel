import type { SourceFile } from "ts-morph";
import ts from "typescript";
import type { OpaqueExpr, OpaqueParam } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
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
import type { PantDeclaration, PropResult } from "./types.js";

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

/**
 * Result of translating a body expression. Either an opaque expression
 * (possibly tagged as a comprehension for chaining), or a failure.
 */
type BodyResult =
  | { unsupported: string }
  | { expr: OpaqueExpr; comprehensionBinder?: string };

/** Type guard for unsupported BodyResult. */
function isBodyUnsupported(r: BodyResult): r is { unsupported: string } {
  return "unsupported" in r;
}

/** Extract the OpaqueExpr from a successful BodyResult. */
function bodyExpr(r: BodyResult): OpaqueExpr {
  if ("unsupported" in r) {
    throw new Error(`bodyExpr called on unsupported: ${r.unsupported}`);
  }
  return r.expr;
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
export function translateBody(opts: TranslateBodyOptions): PropResult[] {
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
): PropResult[] {
  const ast = getAst();

  if (!node.body) {
    return [];
  }

  const extracted = extractReturnExpression(node.body, checker);
  if (!extracted) {
    const reason = describeRejectedBody(node.body, checker);
    return [{ kind: "unsupported", reason: `${functionName} — ${reason}` }];
  }

  // Translate const binding initializers for inline substitution
  const substitutions: Array<{ name: string; expr: OpaqueExpr }> = [];
  for (const binding of extracted.bindings) {
    const initResult = translateBodyExpr(binding.initializer, checker, strategy, paramNames);
    if (isBodyUnsupported(initResult)) {
      return [{ kind: "unsupported", reason: initResult.unsupported }];
    }
    let initExpr = bodyExpr(initResult);
    // Apply all prior substitutions to resolve chained references
    for (const prior of substitutions) {
      initExpr = ast.substituteBinder(initExpr, prior.name, prior.expr);
    }
    substitutions.push({ name: binding.name, expr: initExpr });
  }

  const body = translateBodyExpr(extracted.returnExpr, checker, strategy, paramNames);

  if (isBodyUnsupported(body)) {
    return [{ kind: "unsupported", reason: body.unsupported }];
  }

  // Apply const binding substitutions to the body expression
  let rhs = bodyExpr(body);
  for (const sub of substitutions) {
    rhs = ast.substituteBinder(rhs, sub.name, sub.expr);
  }

  const argExprs = params.map((p) => ast.var(p.name));
  const lhs = ast.app(ast.var(functionName), argExprs);
  return [
    {
      kind: "equation",
      quantifiers: [] as OpaqueParam[],
      lhs,
      rhs,
    },
  ];
}

interface ExtractedBody {
  bindings: Array<{ name: string; initializer: ts.Expression }>;
  returnExpr: ts.Expression;
}

/**
 * Extract the return expression from a function body, collecting any leading
 * const bindings with pure initializers for inline substitution.
 * Handles:
 *   - Single return statement
 *   - Leading const bindings + return statement
 *   - if/else with returns in both branches (produces a synthetic conditional)
 * Returns null if the body contains let/var bindings or effectful const initializers.
 */
function extractReturnExpression(
  body: ts.Block,
  checker: ts.TypeChecker,
): ExtractedBody | null {
  // Skip guard statements (if-throw patterns and assertion calls)
  const stmts = body.statements.filter((s) => !isGuardStatement(s, checker));

  if (stmts.length === 0) {
    return null;
  }

  const bindings: Array<{ name: string; initializer: ts.Expression }> = [];
  let i = 0;

  // Collect leading const bindings
  for (; i < stmts.length - 1; i++) {
    const stmt = stmts[i]!;
    if (!ts.isVariableStatement(stmt)) {
      break;
    }

    const declList = stmt.declarationList;
    // Reject let/var
    if (!(declList.flags & ts.NodeFlags.Const)) {
      return null;
    }

    for (const decl of declList.declarations) {
      // Must have a simple identifier name and an initializer
      if (!ts.isIdentifier(decl.name) || !decl.initializer) {
        return null;
      }
      // Reject effectful initializers
      if (expressionHasSideEffects(decl.initializer)) {
        return null;
      }
      bindings.push({ name: decl.name.text, initializer: decl.initializer });
    }
  }

  // The last statement must be a return or if/else-with-returns
  const last = stmts[i]!;
  // If we didn't consume all preceding statements as const bindings, reject
  if (i < stmts.length - 1) {
    return null;
  }

  if (ts.isReturnStatement(last) && last.expression) {
    return { bindings, returnExpr: last.expression };
  }
  if (ts.isIfStatement(last) && last.elseStatement) {
    return { bindings, returnExpr: last };
  }

  return null;
}

function describeRejectedBody(body: ts.Block, checker: ts.TypeChecker): string {
  const stmts = body.statements.filter((s) => !isGuardStatement(s, checker));
  if (stmts.length === 0) {
    return "empty body";
  }
  if (stmts.length > 1) {
    // Check for specific rejection reasons in leading statements
    for (const stmt of stmts) {
      if (ts.isVariableStatement(stmt)) {
        const declList = stmt.declarationList;
        if (!(declList.flags & ts.NodeFlags.Const)) {
          return "let/var bindings not supported";
        }
        for (const decl of declList.declarations) {
          if (decl.initializer && expressionHasSideEffects(decl.initializer)) {
            return "const binding with side-effectful initializer";
          }
        }
      }
    }
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
  // Condition must be pure (aligned with classifyGuardIf's isPureExpression check)
  if (!isPureExpression(stmt.expression)) {
    return false;
  }
  // if (...) { throw } without else
  if (!stmt.elseStatement && blockThrows(stmt.thenStatement)) {
    return true;
  }
  // if (...) { ... } else { throw }
  if (stmt.elseStatement && blockThrows(stmt.elseStatement)) {
    // Only a guard if the then-block has no side effects and doesn't return.
    // A mutating then-branch like `if (ok) { a.balance = 1; } else { throw e; }`
    // must NOT be classified as a guard — collectAssignments() needs to see it.
    return (
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
 * Translate a TS expression to an opaque Pantagruel AST node, extending the
 * base translateExpr with support for ternary, array ops, and if/else as cond.
 */
export function translateBodyExpr(
  expr: ts.Expression | ts.Statement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): BodyResult {
  const ast = getAst();

  if (ts.isExpression(expr)) {
    expr = unwrapExpression(expr);
  }

  // if/else statement -> cond
  if (ts.isIfStatement(expr)) {
    return translateIfStatement(expr, checker, strategy, paramNames);
  }

  // Ternary: a ? b : c -> cond([[a, b], [true, c]])
  if (ts.isConditionalExpression(expr)) {
    const cond = translateBodyExpr(
      expr.condition,
      checker,
      strategy,
      paramNames,
    );
    if (isBodyUnsupported(cond)) {
      return cond;
    }
    const whenTrue = translateBodyExpr(
      expr.whenTrue,
      checker,
      strategy,
      paramNames,
    );
    if (isBodyUnsupported(whenTrue)) {
      return whenTrue;
    }
    const whenFalse = translateBodyExpr(
      expr.whenFalse,
      checker,
      strategy,
      paramNames,
    );
    if (isBodyUnsupported(whenFalse)) {
      return whenFalse;
    }
    return {
      expr: ast.cond([
        [bodyExpr(cond), bodyExpr(whenTrue)],
        [ast.litBool(true), bodyExpr(whenFalse)],
      ]),
    };
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
    if (isBodyUnsupported(obj)) {
      return obj;
    }
    // .length -> #obj (array only)
    if (prop === "length") {
      const receiverType = checker.getTypeAtLocation(expr.expression);
      if (checker.isArrayType(receiverType)) {
        return { expr: ast.unop(ast.opCard(), bodyExpr(obj)) };
      }
    }
    // Regular property access: a.balance -> app(var("balance"), [obj])
    return { expr: ast.app(ast.var(prop), [bodyExpr(obj)]) };
  }

  // Call expression: handle .includes(), .filter().map(), etc.
  if (ts.isCallExpression(expr)) {
    return translateCallExpr(expr, checker, strategy, paramNames);
  }

  // Prefix unary: !x -> unop(opNot(), x), -x -> unop(opNeg(), x)
  if (ts.isPrefixUnaryExpression(expr)) {
    const operand = translateBodyExpr(
      expr.operand,
      checker,
      strategy,
      paramNames,
    );
    if (isBodyUnsupported(operand)) {
      return operand;
    }
    if (expr.operator === ts.SyntaxKind.ExclamationToken) {
      return { expr: ast.unop(ast.opNot(), bodyExpr(operand)) };
    }
    if (expr.operator === ts.SyntaxKind.MinusToken) {
      return { expr: ast.unop(ast.opNeg(), bodyExpr(operand)) };
    }
  }

  // Binary expression
  if (ts.isBinaryExpression(expr)) {
    const op = translateOperator(expr.operatorToken.kind);
    if (op === null) {
      return {
        unsupported: `operator ${ts.SyntaxKind[expr.operatorToken.kind]}`,
      };
    }
    const left = translateBodyExpr(expr.left, checker, strategy, paramNames);
    if (isBodyUnsupported(left)) {
      return left;
    }
    const right = translateBodyExpr(expr.right, checker, strategy, paramNames);
    if (isBodyUnsupported(right)) {
      return right;
    }
    return { expr: ast.binop(op, bodyExpr(left), bodyExpr(right)) };
  }

  // Fall through to base translateExpr for identifiers, literals, this, etc.
  if (ts.isExpression(expr)) {
    return { expr: translateExpr(expr, checker, strategy, paramNames) };
  }

  return { unsupported: "non-expression statement" };
}

function translateIfStatement(
  stmt: ts.IfStatement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): BodyResult {
  const ast = getAst();

  const cond = translateBodyExpr(
    stmt.expression,
    checker,
    strategy,
    paramNames,
  );
  if (isBodyUnsupported(cond)) {
    return cond;
  }
  const thenExpr = extractReturnFromBranch(stmt.thenStatement, checker);
  const elseExpr = stmt.elseStatement
    ? extractReturnFromBranch(stmt.elseStatement, checker)
    : null;

  if (thenExpr && elseExpr) {
    const thenVal = translateBodyExpr(thenExpr, checker, strategy, paramNames);
    if (isBodyUnsupported(thenVal)) {
      return thenVal;
    }
    const elseVal = translateBodyExpr(elseExpr, checker, strategy, paramNames);
    if (isBodyUnsupported(elseVal)) {
      return elseVal;
    }
    return {
      expr: ast.cond([
        [bodyExpr(cond), bodyExpr(thenVal)],
        [ast.litBool(true), bodyExpr(elseVal)],
      ]),
    };
  }

  return { unsupported: "if statement without return in both branches" };
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
): BodyResult | null {
  const ast = getAst();

  const elemType = getArrayElementType(tsReceiver, checker, strategy);
  if (!elemType) {
    return null;
  }

  const receiver = translateBodyExpr(tsReceiver, checker, strategy, paramNames);
  if (isBodyUnsupported(receiver)) {
    return receiver;
  }

  const isComposing = receiver.comprehensionBinder !== undefined;
  const sourceBinder = isComposing
    ? receiver.comprehensionBinder!
    : freshBinder(paramNames);
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
    return { unsupported: expr.getText() };
  }
  if (isBodyUnsupported(rawBody)) {
    return rawBody;
  }

  // When composing, substitute the callback's binder with the prior step's body
  // so that e.g. xs.map(f).map(g) becomes (each x: T | g(f(x))) not (each x: T | g(x))
  //
  // For composition, we need the inner comprehension's body expression. Since
  // we can't inspect opaque values, we use ast.substituteBinder on the raw
  // callback body, replacing the callback binder with a variable named after
  // the source binder. The outer comprehension will bind that variable.
  const bodyE = isComposing
    ? ast.substituteBinder(
        bodyExpr(rawBody),
        callbackBinder,
        ast.var(sourceBinder),
      )
    : bodyExpr(rawBody);

  if (methodName === "filter") {
    if (isComposing) {
      // Composing filter onto an existing comprehension: add a guard predicate.
      // We build a new comprehension with both the existing body and the new
      // filter predicate as a guard.
      // The existing comprehension's body becomes the new body, and the filter
      // predicate is added as an additional guard.
      //
      // We reconstruct the comprehension: each sourceBinder: elemType, guards + new guard | existingBody
      // Since we can't inspect the opaque comprehension, we track enough to rebuild.
      // For now, produce a standalone comprehension with the filter as a guard on the source binder variable.
      return {
        expr: ast.each(
          [ast.param(sourceBinder, ast.tName(elemType))],
          [ast.gExpr(bodyE)],
          ast.var(sourceBinder),
        ),
        comprehensionBinder: sourceBinder,
      };
    }
    return {
      expr: ast.each(
        [ast.param(sourceBinder, ast.tName(elemType))],
        [ast.gExpr(bodyE)],
        ast.var(sourceBinder),
      ),
      comprehensionBinder: sourceBinder,
    };
  } else {
    // map
    if (isComposing) {
      return {
        expr: ast.each(
          [ast.param(sourceBinder, ast.tName(elemType))],
          [],
          bodyE,
        ),
        comprehensionBinder: sourceBinder,
      };
    }
    return {
      expr: ast.each([ast.param(sourceBinder, ast.tName(elemType))], [], bodyE),
      comprehensionBinder: sourceBinder,
    };
  }
}

function translateCallExpr(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): BodyResult {
  const ast = getAst();

  // Method calls: obj.method(args)
  if (ts.isPropertyAccessExpression(expr.expression)) {
    const methodName = expr.expression.name.text;
    const tsReceiver = expr.expression.expression;

    // .includes(x) -> x in obj (array only)
    if (methodName === "includes" && expr.arguments.length === 1) {
      const receiverType = checker.getTypeAtLocation(tsReceiver);
      if (!checker.isArrayType(receiverType)) {
        return { unsupported: "non-array .includes()" };
      }
      const arg = translateBodyExpr(
        expr.arguments[0]!,
        checker,
        strategy,
        paramNames,
      );
      if (isBodyUnsupported(arg)) {
        return arg;
      }
      const objExpr = translateBodyExpr(
        tsReceiver,
        checker,
        strategy,
        paramNames,
      );
      if (isBodyUnsupported(objExpr)) {
        return objExpr;
      }
      return { expr: ast.binop(ast.opIn(), bodyExpr(arg), bodyExpr(objExpr)) };
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
  return { unsupported: expr.getText() };
}

function extractArrowBody(
  expr: ts.Expression,
  binderName: string,
  paramNames: Map<string, string>,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
): BodyResult | null {
  if (!ts.isArrowFunction(expr)) {
    return null;
  }
  if (
    expr.parameters.length !== 1 ||
    !ts.isIdentifier(expr.parameters[0]!.name)
  ) {
    return {
      unsupported:
        "filter/map callback must have exactly one identifier parameter",
    };
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
): PropResult[] {
  if (!node.body) {
    return [];
  }

  const propositions: PropResult[] = [];
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
  propositions: PropResult[],
  modifiedRules: Set<string>,
): boolean {
  const ast = getAst();
  let hasUnsupportedMutation = false;
  const constSubstitutions: Array<{ name: string; expr: OpaqueExpr }> = [];
  const stmts = ts.isBlock(body) ? Array.from(body.statements) : [body];

  for (const stmt of stmts) {
    // Skip guard statements (if-throw patterns and assertion calls)
    if (isGuardStatement(stmt, checker)) {
      continue;
    }

    // Handle const bindings: translate initializer and store for substitution
    if (ts.isVariableStatement(stmt)) {
      const declList = stmt.declarationList;
      if (declList.flags & ts.NodeFlags.Const) {
        let allPure = true;
        for (const decl of declList.declarations) {
          if (
            !ts.isIdentifier(decl.name) ||
            !decl.initializer ||
            expressionHasSideEffects(decl.initializer)
          ) {
            allPure = false;
            break;
          }
        }
        if (allPure) {
          for (const decl of declList.declarations) {
            const name = (decl.name as ts.Identifier).text;
            const initResult = translateBodyExpr(
              decl.initializer!,
              checker,
              strategy,
              paramNames,
            );
            if (isBodyUnsupported(initResult)) {
              hasUnsupportedMutation = true;
              propositions.push({
                kind: "unsupported",
                reason: initResult.unsupported,
              });
              continue;
            }
            let initExpr = bodyExpr(initResult);
            // Apply all prior const substitutions to resolve chained references
            for (const prior of constSubstitutions) {
              initExpr = ast.substituteBinder(
                initExpr,
                prior.name,
                prior.expr,
              );
            }
            constSubstitutions.push({ name, expr: initExpr });
          }
          continue;
        }
      }
      // let/var or effectful const — fall through to existing rejection
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
        if (isBodyUnsupported(obj)) {
          hasUnsupportedMutation = true;
          propositions.push({ kind: "unsupported", reason: obj.unsupported });
          continue;
        }
        if (isBodyUnsupported(val)) {
          hasUnsupportedMutation = true;
          propositions.push({ kind: "unsupported", reason: val.unsupported });
          continue;
        }
        // Apply const substitutions to assignment expressions
        let objExpr = bodyExpr(obj);
        let valExpr = bodyExpr(val);
        for (const sub of constSubstitutions) {
          objExpr = ast.substituteBinder(objExpr, sub.name, sub.expr);
          valExpr = ast.substituteBinder(valExpr, sub.name, sub.expr);
        }
        propositions.push({
          kind: "equation",
          quantifiers: [] as OpaqueParam[],
          lhs: ast.app(ast.primed(prop), [objExpr]),
          rhs: valExpr,
        });
        modifiedRules.add(prop);
        continue;
      }
    }

    if (
      ts.isExpressionStatement(stmt) &&
      expressionHasSideEffects(stmt.expression)
    ) {
      propositions.push({
        kind: "unsupported",
        reason: "side-effectful expression",
      });
      hasUnsupportedMutation = true;
      continue;
    }

    if (
      ts.isVariableStatement(stmt) &&
      stmt.declarationList.declarations.some(
        (d) => d.initializer && expressionHasSideEffects(d.initializer),
      )
    ) {
      propositions.push({
        kind: "unsupported",
        reason: "side-effectful variable initializer",
      });
      hasUnsupportedMutation = true;
      continue;
    }

    if (
      (ts.isReturnStatement(stmt) || ts.isThrowStatement(stmt)) &&
      stmt.expression &&
      expressionHasSideEffects(stmt.expression)
    ) {
      propositions.push({
        kind: "unsupported",
        reason: "side-effectful control-flow expression",
      });
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
      propositions.push({
        kind: "unsupported",
        reason: "conditional assignment (if/else)",
      });
      hasUnsupportedMutation = true;
    } else if (
      ts.isForStatement(stmt) ||
      ts.isForOfStatement(stmt) ||
      ts.isForInStatement(stmt) ||
      ts.isWhileStatement(stmt) ||
      ts.isDoStatement(stmt)
    ) {
      propositions.push({ kind: "unsupported", reason: "loop assignment" });
      hasUnsupportedMutation = true;
    } else if (ts.isTryStatement(stmt)) {
      // try/catch branches are mutually exclusive; collecting from both would
      // produce contradictory conjunctions. Only the finally block executes
      // unconditionally.
      if (stmt.catchClause) {
        propositions.push({
          kind: "unsupported",
          reason: "try/catch assignment",
        });
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
      propositions.push({ kind: "unsupported", reason: "switch assignment" });
      hasUnsupportedMutation = true;
    }
  }

  return hasUnsupportedMutation;
}

/**
 * Generate frame conditions: for each rule in declarations not explicitly
 * modified, emit `rule' x = rule x`. Variables are already in scope from
 * the rule declarations in the chapter head.
 */
function generateFrameConditions(
  modifiedRules: Set<string>,
  declarations: PantDeclaration[],
): PropResult[] {
  const ast = getAst();
  const frames: PropResult[] = [];

  for (const decl of declarations) {
    if (decl.kind !== "rule") {
      continue;
    }
    if (modifiedRules.has(decl.name)) {
      continue;
    }

    const paramArgs = decl.params.map((p) => ast.var(p.name));
    const lhs = ast.app(ast.primed(decl.name), paramArgs);
    const rhs = ast.app(ast.var(decl.name), paramArgs);
    frames.push({
      kind: "equation",
      quantifiers: [] as OpaqueParam[],
      lhs,
      rhs,
    });
  }

  return frames;
}
