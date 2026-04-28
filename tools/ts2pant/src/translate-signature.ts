import type { SourceFile } from "ts-morph";
import ts from "typescript";
import { irWrap } from "./ir.js";
import { lowerExpr } from "./ir-emit.js";
import { ir1FromL2 } from "./ir1.js";
import {
  type BuildL1MemberAccessOptions,
  buildL1MemberAccess,
  type L1BuildContext,
  tryBuildL1Cardinality,
  unwrapParens,
} from "./ir1-build.js";
import { lowerL1Expr } from "./ir1-lower.js";
import {
  type NullishTranslate,
  recognizeNullishForm,
} from "./nullish-recognizer.js";
import type { OpaqueBinop, OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import {
  cellIsUsed,
  cellRegisterName,
  isMapType,
  mapTsType,
  type NumericStrategy,
  type SynthCell,
  toPantTermName,
} from "./translate-types.js";
import type { PantAction, PantDeclaration, PantRule } from "./types.js";

export type Classification = "pure" | "mutating";

/**
 * Result of translating a TS expression to a Pant `OpaqueExpr`.
 * Either the translated expression, or an explicit `{ unsupported }`
 * marker for operators the conservative-refusal policy rejects (today:
 * loose equality `==` / `!=`). Distinct from `translateOperator`'s
 * `null` return — that signals "operator not handled" and falls through
 * to a raw-text fallback (acceptable for genuinely-unknown operators
 * that pant will fail to parse downstream). This `{ unsupported }`
 * signals "operator forbidden"; callers must check and bail cleanly
 * rather than emit raw source text as a Pant variable.
 *
 * Mirrors the `L1BuildResult` shape in `ir1-build.ts` (and the
 * `BodyResult` union in `translate-body.ts`) — the codebase convention
 * for unsupported translations is an explicit discriminated-union
 * result, never an exception.
 */
export type TranslateExprResult = OpaqueExpr | { unsupported: string };

export function isTranslateExprUnsupported(
  r: TranslateExprResult,
): r is { unsupported: string } {
  return typeof r === "object" && r !== null && "unsupported" in r;
}

export interface TranslatedSignature {
  declaration: PantDeclaration;
  classification: Classification;
  /** Map from TS parameter names to Pantagruel parameter names. */
  paramNameMap: Map<string, string>;
  /**
   * Synthesizer cell for `Map<K, V>` domains encountered during signature
   * translation (parameter and return types). Pass through to downstream
   * stages (`translateTypes`, `translateBody`) so they register and look up
   * Maps in the same table. Present only when a `synthCell` was supplied.
   */
  synthCell?: SynthCell | undefined;
}

/**
 * Find a function or method declaration by name in a source file.
 * Searches top-level functions and class methods.
 * Returns raw compiler nodes for downstream expression walking.
 */
export function findFunction(
  sourceFile: SourceFile,
  functionName: string,
): {
  node: ts.FunctionDeclaration | ts.MethodDeclaration;
  className?: string | undefined;
} {
  // Support "ClassName.methodName" for disambiguating methods
  const [classHint, memberName] = functionName.includes(".")
    ? (functionName.split(".", 2) as [string, string])
    : [undefined, functionName];

  // Search top-level functions (prefer one with body over overload signatures)
  if (!classHint) {
    const funcs = sourceFile
      .getFunctions()
      .filter((f) => f.getName() === memberName);
    const funcWithBody = funcs.find((f) => f.hasBody());
    const func = funcWithBody ?? funcs[0];
    if (func) {
      return { node: func.compilerNode as ts.FunctionDeclaration };
    }
  }

  // Search class methods
  for (const cls of sourceFile.getClasses()) {
    if (classHint && cls.getName() !== classHint) {
      continue;
    }
    const methods = cls.getMethods().filter((m) => m.getName() === memberName);
    const methodWithBody = methods.find((m) => m.hasBody());
    const method = methodWithBody ?? methods[0];
    if (method) {
      return {
        node: method.compilerNode as ts.MethodDeclaration,
        className: cls.getName(),
      };
    }
  }

  throw new Error(`Function not found: ${functionName}`);
}

/**
 * Classify a function as pure or mutating.
 * Mutating: void return type or has mutations in body (property assignments,
 * or `.set`/`.delete` calls on a Map receiver).
 * Pure: everything else.
 */
export function classifyFunction(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  checker: ts.TypeChecker,
): Classification {
  const sig = checker.getSignatureFromDeclaration(node);
  if (!sig) {
    throw new Error("Cannot get signature for classification");
  }

  const returnType = sig.getReturnType();
  if (returnType.flags & ts.TypeFlags.Void) {
    return "mutating";
  }

  if (node.body && hasMutation(node.body, checker)) {
    return "mutating";
  }

  return "pure";
}

function hasMutation(node: ts.Node, checker: ts.TypeChecker): boolean {
  let found = false;
  function visit(n: ts.Node) {
    if (found) {
      return;
    }
    // Don't recurse into nested functions or classes
    if (ts.isFunctionLike(n) && n !== node) {
      return;
    }
    if (ts.isClassDeclaration(n) || ts.isClassExpression(n)) {
      return;
    }
    if (
      ts.isBinaryExpression(n) &&
      n.operatorToken.kind === ts.SyntaxKind.EqualsToken &&
      ts.isPropertyAccessExpression(n.left)
    ) {
      found = true;
      return;
    }
    // `.set(k, v)` or `.delete(k)` on a Map-typed receiver is a mutation.
    if (
      ts.isCallExpression(n) &&
      ts.isPropertyAccessExpression(n.expression) &&
      (n.expression.name.text === "set" ||
        n.expression.name.text === "delete") &&
      isMapType(checker.getTypeAtLocation(n.expression.expression))
    ) {
      found = true;
      return;
    }
    ts.forEachChild(n, visit);
  }
  ts.forEachChild(node, visit);
  return found;
}

/**
 * Check if a call expression targets a function with an `asserts` return type.
 * Returns the assertion's parameter index if so (the parameter being asserted).
 */
export function isAssertionCall(
  checker: ts.TypeChecker,
  call: ts.CallExpression,
): number | null {
  if (!isPureExpression(call.expression)) {
    return null;
  }
  if (!call.arguments.every(isPureExpression)) {
    return null;
  }

  const sig = checker.getResolvedSignature(call);
  if (!sig) {
    return null;
  }
  const decl = sig.declaration;
  if (!decl || !ts.isFunctionLike(decl) || !decl.type) {
    return null;
  }
  if (!ts.isTypePredicateNode(decl.type)) {
    return null;
  }
  if (!decl.type.assertsModifier) {
    return null;
  }
  // Reject `asserts value is Type` narrowing predicates — only bare `asserts condition`
  if (decl.type.type) {
    return null;
  }

  // Find which parameter is being asserted
  const paramName = decl.type.parameterName;
  if (!ts.isIdentifier(paramName)) {
    return null;
  }

  const params = decl.parameters;
  for (let i = 0; i < params.length; i++) {
    const p = params[i]!;
    if (ts.isIdentifier(p.name) && p.name.text === paramName.text) {
      return i;
    }
  }
  return null;
}

/**
 * Extract the guard condition from an assertion call.
 * For `assert(amount > 0)` where assert has `asserts value`, returns "amount > 0".
 * For bare `asserts x` (truthiness), returns the argument expression.
 */
export function extractAssertionGuard(
  checker: ts.TypeChecker,
  call: ts.CallExpression,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  synthCell?: SynthCell,
): OpaqueExpr | undefined {
  const paramIndex = isAssertionCall(checker, call);
  if (paramIndex === null) {
    return undefined;
  }
  if (paramIndex >= call.arguments.length) {
    return undefined;
  }
  const arg = call.arguments[paramIndex]!;
  const result = translateExpr(arg, checker, strategy, paramNames, synthCell);
  if (isTranslateExprUnsupported(result)) {
    // Assertion contains an explicitly-unsupported operator (loose
    // equality). Skip the guard rather than emit raw text.
    return undefined;
  }
  return result;
}

/**
 * Resolve a call expression to a function body we can follow.
 * Returns the function-like node and its parameters, or null if we should bail.
 *
 * Bail conditions: node_modules, non-function declaration, dynamic dispatch,
 * higher-order calls (callee is a parameter).
 */
function resolveCallTarget(
  call: ts.CallExpression,
  checker: ts.TypeChecker,
): { body: ts.Block; params: ts.NodeArray<ts.ParameterDeclaration> } | null {
  // Bail on property-access/method calls — dynamic dispatch, `this` unsubstituted
  if (!ts.isIdentifier(call.expression)) {
    return null;
  }

  // Try getResolvedSignature first (works for direct calls, imports)
  const sig = checker.getResolvedSignature(call);
  let decl = sig?.declaration;

  // For const-bound functions, resolve via symbol
  if (!decl || !ts.isFunctionLike(decl)) {
    const symbol = checker.getSymbolAtLocation(call.expression);
    const vDecl = symbol?.valueDeclaration;
    if (vDecl && ts.isVariableDeclaration(vDecl) && vDecl.initializer) {
      const init = vDecl.initializer;
      if (ts.isFunctionExpression(init) || ts.isArrowFunction(init)) {
        decl = init;
      }
    }
  }

  if (!decl || !ts.isFunctionLike(decl)) {
    return null;
  }

  // Bail: declaration is in node_modules
  const sourceFile = decl.getSourceFile();
  if (sourceFile.fileName.includes("node_modules")) {
    return null;
  }

  // Get the body — must be a block (not expression-bodied arrow)
  let body: ts.Block | undefined;
  if (ts.isFunctionDeclaration(decl) || ts.isMethodDeclaration(decl)) {
    body = decl.body;
  } else if (ts.isFunctionExpression(decl)) {
    body = decl.body;
  } else if (ts.isArrowFunction(decl)) {
    body = ts.isBlock(decl.body) ? decl.body : undefined;
  }

  if (!body) {
    return null;
  }

  return { body, params: decl.parameters };
}

/**
 * Build a parameter substitution map for following a call into a function.
 * Maps formal parameter names to translated actual argument expressions.
 */
function buildSubstitutionMap(
  call: ts.CallExpression,
  targetParams: ts.NodeArray<ts.ParameterDeclaration>,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  callerParamNames: ReadonlyMap<string, string>,
  synthCell?: SynthCell,
): Map<string, string> | null {
  const ast = getAst();

  if (call.arguments.some(ts.isSpreadElement)) {
    return null;
  }

  const innerMap = new Map<string, string>();
  for (let i = 0; i < targetParams.length; i++) {
    const formal = targetParams[i]!;
    if (!ts.isIdentifier(formal.name)) {
      return null;
    }
    if (formal.dotDotDotToken || i >= call.arguments.length) {
      return null;
    }

    const actualResult = translateExpr(
      call.arguments[i]!,
      checker,
      strategy,
      callerParamNames,
      synthCell,
    );
    if (isTranslateExprUnsupported(actualResult)) {
      // Argument contains an explicitly-unsupported operator. Bail the
      // entire substitution rather than emit a partial map that would
      // inline raw source text into a guard.
      return null;
    }
    const actual = actualResult;
    const rendered = ast.strExpr(actual);
    // Heuristic: if the rendered string contains spaces, it's compound and needs parens
    const safeRendered = /\s/u.test(rendered) ? `(${rendered})` : rendered;
    innerMap.set(formal.name.text, safeRendered);
  }
  return innerMap;
}

/**
 * Follow a call expression into the called function to extract guards.
 * Conservative: bails on recursion, dynamic dispatch, node_modules, etc.
 */
function followGuards(
  call: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  callerParamNames: ReadonlyMap<string, string>,
  visited: Set<ts.Node>,
  synthCell?: SynthCell,
): OpaqueExpr[] {
  const target = resolveCallTarget(call, checker);
  if (!target) {
    return [];
  }

  // Bail: recursion
  if (visited.has(target.body)) {
    return [];
  }

  const innerParamNames = buildSubstitutionMap(
    call,
    target.params,
    checker,
    strategy,
    callerParamNames,
    synthCell,
  );
  if (!innerParamNames) {
    return [];
  }

  visited.add(target.body);
  const guards = scanBodyForGuards(
    target.body,
    checker,
    strategy,
    innerParamNames,
    visited,
    synthCell,
  );
  visited.delete(target.body);

  return guards;
}

const ASSIGNMENT_OPERATORS = new Set([
  ts.SyntaxKind.EqualsToken,
  ts.SyntaxKind.PlusEqualsToken,
  ts.SyntaxKind.MinusEqualsToken,
  ts.SyntaxKind.AsteriskEqualsToken,
  ts.SyntaxKind.SlashEqualsToken,
  ts.SyntaxKind.PercentEqualsToken,
  ts.SyntaxKind.AsteriskAsteriskEqualsToken,
  ts.SyntaxKind.LessThanLessThanEqualsToken,
  ts.SyntaxKind.GreaterThanGreaterThanEqualsToken,
  ts.SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken,
  ts.SyntaxKind.AmpersandEqualsToken,
  ts.SyntaxKind.BarEqualsToken,
  ts.SyntaxKind.CaretEqualsToken,
  ts.SyntaxKind.BarBarEqualsToken,
  ts.SyntaxKind.AmpersandAmpersandEqualsToken,
  ts.SyntaxKind.QuestionQuestionEqualsToken,
]);

/**
 * Walk an expression looking for operators that `translateExpr` /
 * `translateBodyExpr` explicitly reject (today: loose equality `==` and
 * `!=`). Used by guard-detection predicates to keep them aligned with
 * the actual translator — a guard whose condition contains a forbidden
 * operator must NOT be classified as a guard, otherwise the body filter
 * would silently drop the statement while signature extraction would
 * separately drop the guard, losing the runtime check on both sides.
 *
 * Mirrors the rejection list in `translateExpr`'s binary-expression
 * branch (translate-signature.ts) and `translateBodyExpr`'s
 * binary-expression branch (translate-body.ts). Adding a new operator
 * to those rejection lists must also add it here.
 */
export function containsUnsupportedOperator(expr: ts.Expression): boolean {
  if (ts.isBinaryExpression(expr)) {
    if (
      expr.operatorToken.kind === ts.SyntaxKind.EqualsEqualsToken ||
      expr.operatorToken.kind === ts.SyntaxKind.ExclamationEqualsToken
    ) {
      return true;
    }
    return (
      containsUnsupportedOperator(expr.left) ||
      containsUnsupportedOperator(expr.right)
    );
  }
  if (
    ts.isParenthesizedExpression(expr) ||
    ts.isAsExpression(expr) ||
    ts.isNonNullExpression(expr) ||
    ts.isSatisfiesExpression(expr)
  ) {
    return containsUnsupportedOperator(expr.expression);
  }
  if (ts.isPrefixUnaryExpression(expr) || ts.isPostfixUnaryExpression(expr)) {
    return containsUnsupportedOperator(expr.operand);
  }
  if (ts.isPropertyAccessExpression(expr)) {
    return containsUnsupportedOperator(expr.expression);
  }
  if (ts.isElementAccessExpression(expr)) {
    return (
      containsUnsupportedOperator(expr.expression) ||
      containsUnsupportedOperator(expr.argumentExpression)
    );
  }
  if (ts.isCallExpression(expr)) {
    return (
      containsUnsupportedOperator(expr.expression) ||
      expr.arguments.some(containsUnsupportedOperator)
    );
  }
  if (ts.isConditionalExpression(expr)) {
    return (
      containsUnsupportedOperator(expr.condition) ||
      containsUnsupportedOperator(expr.whenTrue) ||
      containsUnsupportedOperator(expr.whenFalse)
    );
  }
  return false;
}

/**
 * Check whether an expression is side-effect-free (identifiers, literals,
 * property access, operators — no calls, assignments, or increments).
 */
export function isPureExpression(expr: ts.Expression): boolean {
  if (
    ts.isIdentifier(expr) ||
    ts.isNumericLiteral(expr) ||
    ts.isStringLiteral(expr) ||
    ts.isNoSubstitutionTemplateLiteral(expr) ||
    expr.kind === ts.SyntaxKind.TrueKeyword ||
    expr.kind === ts.SyntaxKind.FalseKeyword ||
    expr.kind === ts.SyntaxKind.NullKeyword ||
    expr.kind === ts.SyntaxKind.ThisKeyword
  ) {
    return true;
  }
  if (
    ts.isParenthesizedExpression(expr) ||
    ts.isAsExpression(expr) ||
    ts.isNonNullExpression(expr)
  ) {
    return isPureExpression(expr.expression);
  }
  if (ts.isPropertyAccessExpression(expr)) {
    return isPureExpression(expr.expression);
  }
  if (ts.isElementAccessExpression(expr)) {
    return (
      isPureExpression(expr.expression) &&
      isPureExpression(expr.argumentExpression)
    );
  }
  if (ts.isPrefixUnaryExpression(expr)) {
    if (
      expr.operator === ts.SyntaxKind.PlusPlusToken ||
      expr.operator === ts.SyntaxKind.MinusMinusToken
    ) {
      return false;
    }
    return isPureExpression(expr.operand);
  }
  if (ts.isBinaryExpression(expr)) {
    if (ASSIGNMENT_OPERATORS.has(expr.operatorToken.kind)) {
      return false;
    }
    return isPureExpression(expr.left) && isPureExpression(expr.right);
  }
  // Calls, new, await, template expressions, etc. are not pure
  return false;
}

/**
 * Check whether a then-branch is side-effect-free and non-returning,
 * so extracting the condition as a guard won't silently discard
 * meaningful work.  Aligned with isGuardStatement in translate-body.ts.
 */
function isSafeGuardThenBranch(stmt: ts.Statement): boolean {
  return guardBranchHasNoSideEffects(stmt) && !guardBranchReturns(stmt);
}

function guardBranchReturns(node: ts.Statement): boolean {
  if (ts.isBlock(node)) {
    return node.statements.some((s) => ts.isReturnStatement(s));
  }
  return ts.isReturnStatement(node);
}

function guardBranchHasNoSideEffects(node: ts.Statement): boolean {
  if (ts.isBlock(node)) {
    return node.statements.every((s) => guardBranchHasNoSideEffects(s));
  }
  if (ts.isExpressionStatement(node)) {
    return isPureExpression(node.expression);
  }
  if (ts.isVariableStatement(node)) {
    return node.declarationList.declarations.every(
      (d) => !d.initializer || isPureExpression(d.initializer),
    );
  }
  if (ts.isReturnStatement(node) || ts.isThrowStatement(node)) {
    return true;
  }
  if (ts.isEmptyStatement(node)) {
    return true;
  }
  return false;
}

/**
 * Check if an if-statement matches a guard pattern (pure condition + throw branch).
 * Returns "positive" for if/side-effect-free-then/else-throw,
 * "negative" for if-throw (no else), or null if the pattern doesn't match.
 *
 * The then-branch acceptance must stay aligned with isGuardStatement in
 * translate-body.ts — both accept side-effect-free, non-returning then-blocks.
 */
function classifyGuardIf(stmt: ts.IfStatement): "positive" | "negative" | null {
  if (!isPureExpression(stmt.expression)) {
    return null;
  }
  // M4 P3: a guard whose condition contains a forbidden operator (loose
  // equality) cannot be translated. If we classified it as a guard, the
  // body filter would drop the statement and signature extraction would
  // separately drop the guard — losing the runtime check on both sides.
  // Refuse the classification so the statement stays in the body, where
  // the body translator will surface a specific `unsupported` reason.
  if (containsUnsupportedOperator(stmt.expression)) {
    return null;
  }
  if (
    stmt.elseStatement &&
    isSafeGuardThenBranch(stmt.thenStatement) &&
    blockThrows(stmt.elseStatement)
  ) {
    return "positive";
  }
  if (!stmt.elseStatement && blockThrows(stmt.thenStatement)) {
    return "negative";
  }
  return null;
}

/**
 * Scan leading statements of a function body for guard patterns.
 * Extracts guards from if-throw patterns, assertion calls, and
 * recursively follows direct calls to extract their guards.
 */
function scanBodyForGuards(
  body: ts.Block,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  visited: Set<ts.Node>,
  synthCell?: SynthCell,
): OpaqueExpr[] {
  const ast = getAst();
  const guards: OpaqueExpr[] = [];

  for (const stmt of body.statements) {
    // Pattern: call expression (assertion or followable call)
    if (
      ts.isExpressionStatement(stmt) &&
      ts.isCallExpression(stmt.expression)
    ) {
      // Skip extraction if callee or arguments contain side effects
      // (e.g. makeValidator().assertPositive(x) or assert(loadAmount() > 0))
      if (!isPureExpression(stmt.expression.expression)) {
        break;
      }
      const argsArePure = stmt.expression.arguments.every(isPureExpression);
      if (!argsArePure) {
        break;
      }

      // Try assertion call first
      const g = extractAssertionGuard(
        checker,
        stmt.expression,
        strategy,
        paramNames,
        synthCell,
      );
      if (g !== undefined) {
        guards.push(g);
        continue;
      }

      // Try following the call
      const followed = followGuards(
        stmt.expression,
        checker,
        strategy,
        paramNames,
        visited,
        synthCell,
      );
      if (followed.length > 0) {
        guards.push(...followed);
        continue;
      }

      // Non-followable call — stop scanning (side-effectful statement)
      break;
    }

    // Only extract if-throw guards from leading precondition checks
    if (!ts.isIfStatement(stmt)) {
      break;
    }

    const guardKind = classifyGuardIf(stmt);
    if (guardKind === "positive") {
      const g = tryTranslateGuardExpr(
        stmt.expression,
        checker,
        strategy,
        paramNames,
        synthCell,
      );
      if (g === null) {
        // Guard contains explicitly-unsupported operator — stop scanning
        // rather than emit a partial guard set with raw source text.
        break;
      }
      guards.push(g);
      continue;
    }
    if (guardKind === "negative") {
      if (
        ts.isPrefixUnaryExpression(stmt.expression) &&
        stmt.expression.operator === ts.SyntaxKind.ExclamationToken
      ) {
        const g = tryTranslateGuardExpr(
          stmt.expression.operand,
          checker,
          strategy,
          paramNames,
          synthCell,
        );
        if (g === null) {
          break;
        }
        guards.push(g);
        continue;
      }
      const g = tryTranslateGuardExpr(
        stmt.expression,
        checker,
        strategy,
        paramNames,
        synthCell,
      );
      if (g === null) {
        break;
      }
      guards.push(ast.unop(ast.opNot(), g));
      continue;
    }

    // If-statement doesn't match a guard pattern — stop scanning
    break;
  }

  return guards;
}

/**
 * Translate a guard expression, returning `null` when the expression
 * contains an explicitly-unsupported operator (today: loose equality).
 * Mirrors the bail behavior in `extractAssertionGuard` and
 * `buildSubstitutionMap` — one helper unifies the union check so all
 * three guard-extraction paths reject identically.
 */
function tryTranslateGuardExpr(
  expr: ts.Expression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  synthCell: SynthCell | undefined,
): OpaqueExpr | null {
  const result = translateExpr(expr, checker, strategy, paramNames, synthCell);
  if (isTranslateExprUnsupported(result)) {
    return null;
  }
  return result;
}

/**
 * Check if a call expression resolves to a function that is a "pure guard" —
 * every statement in its body is a guard pattern, with no non-guard side effects.
 * Used by isGuardStatement in translate-body.ts to filter followed calls.
 */
export function isFollowableGuardCall(
  call: ts.CallExpression,
  checker: ts.TypeChecker,
  visited: Set<ts.Node> = new Set(),
): boolean {
  const target = resolveCallTarget(call, checker);
  if (!target) {
    return false;
  }
  // Reject spread arguments — buildSubstitutionMap cannot handle them.
  if (call.arguments.some(ts.isSpreadElement)) {
    return false;
  }
  // Validate parameter compatibility (mirrors buildSubstitutionMap checks).
  for (let i = 0; i < target.params.length; i++) {
    const formal = target.params[i]!;
    if (!ts.isIdentifier(formal.name)) {
      return false;
    }
    if (formal.dotDotDotToken || i >= call.arguments.length) {
      return false;
    }
  }
  if (visited.has(target.body)) {
    return false;
  }
  if (target.body.statements.length === 0) {
    return false;
  }

  visited.add(target.body);
  const stmts = target.body.statements;
  const result = stmts.every((stmt, i) => {
    if (
      ts.isExpressionStatement(stmt) &&
      ts.isCallExpression(stmt.expression)
    ) {
      if (!isPureExpression(stmt.expression.expression)) {
        return false;
      }
      if (!stmt.expression.arguments.every(isPureExpression)) {
        return false;
      }
      // M4 P3: an assertion call whose argument contains a forbidden
      // operator (loose equality) cannot be translated. Reject the
      // whole helper rather than silently dropping the guard during
      // signature extraction. The body translator will then surface
      // the rejection on the helper call itself.
      if (stmt.expression.arguments.some(containsUnsupportedOperator)) {
        return false;
      }
      return (
        isAssertionCall(checker, stmt.expression) !== null ||
        isFollowableGuardCall(stmt.expression, checker, visited)
      );
    }
    // Accept a trailing empty return (e.g. `assertPositive(x); return;`)
    if (
      ts.isReturnStatement(stmt) &&
      !stmt.expression &&
      i === stmts.length - 1
    ) {
      return true;
    }
    if (!ts.isIfStatement(stmt)) {
      return false;
    }
    // classifyGuardIf already rejects untranslatable conditions —
    // this is the same check scanBodyForGuards relies on.
    return classifyGuardIf(stmt) !== null;
  });
  visited.delete(target.body);
  return result;
}

/**
 * Detect guard patterns in a function body.
 * Patterns:
 *   if (cond) { body } else { throw ... }
 *   if (!cond) { throw ... } (early return guard)
 *   assert(cond) — calls with `asserts` return type
 *   validateX(args) — direct calls to functions containing only guards
 */
export function detectGuard(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  synthCell?: SynthCell,
): OpaqueExpr | undefined {
  const ast = getAst();

  if (!node.body) {
    return undefined;
  }

  const visited = new Set<ts.Node>();
  visited.add(node.body);
  const guards = scanBodyForGuards(
    node.body,
    checker,
    strategy,
    paramNames,
    visited,
    synthCell,
  );

  if (guards.length === 0) {
    return undefined;
  }
  return guards.reduce((acc, g) => ast.binop(ast.opAnd(), acc, g));
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
    // variable declarations (e.g. building the error message).
    if (!ts.isThrowStatement(stmts.at(-1)!)) {
      return false;
    }
    return stmts
      .slice(0, -1)
      .every(
        (s) =>
          ts.isVariableStatement(s) &&
          s.declarationList.declarations.every(
            (d) => !d.initializer || isPureExpression(d.initializer),
          ),
      );
  }
  return false;
}

/**
 * Translate a TypeScript expression to an opaque Pantagruel AST node
 * (best-effort), or return `{ unsupported: reason }` for operators the
 * conservative-refusal policy rejects (today: loose `==`/`!=`).
 *
 * Callers must check the result via `isTranslateExprUnsupported` and
 * bail cleanly. This mirrors the `BodyResult` / `L1BuildResult`
 * convention — the codebase signals unsupported translations through
 * explicit discriminated-union results, never exceptions.
 */
export function translateExpr(
  expr: ts.Expression,
  checker: ts.TypeChecker,
  _strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  synthCell?: SynthCell,
): TranslateExprResult {
  const ast = getAst();

  // M5: plain (non-optional) property access dispatches cardinality
  // first (`xs.length` / `m.size` on list-shaped TS types → L1
  // `Unop(card, x)`, lowered to `#x`), then falls through to L1
  // Member. Pre-cutover the signature path emitted `length xs` /
  // `size m` as opaque EUF rule applications — solver couldn't see
  // them as cardinality. Routing through `tryBuildL1Cardinality`
  // makes signature guards see the same `#x` primitive that body and
  // pure paths now produce, satisfying spec invariant 8 universally.
  // Optional-chain access and ElementAccess fall through to the rest
  // of the dispatch.
  if (
    ts.isPropertyAccessExpression(expr) &&
    (expr.flags & ts.NodeFlags.OptionalChain) === 0
  ) {
    const l1Ctx: L1BuildContext = {
      checker,
      strategy: _strategy,
      paramNames,
      state: undefined,
      supply: { n: 0, synthCell },
    };
    // Signature-side options shared between cardinality and Member:
    // - `ambiguousOwnerFallback: "bare-kebab"` keeps optional analyses
    //   (guard extraction, call-following) from bailing on a single
    //   ambiguous union/intersection accessor; `translateExpr`'s
    //   callers treat unsupported as a hard bail.
    // - `translateReceiverLeaf` routes the non-property leaf through
    //   `translateExpr` (signature-side) so the receiver inherits
    //   signature-only operator surface — loose-eq rejection,
    //   transparent `as`/`!`/`satisfies` unwrapping — and skips
    //   body-only branches (chain fusion, Map/Set effects,
    //   optional-chain functor lift) inappropriate inside guards.
    const sigOptions: BuildL1MemberAccessOptions = {
      ambiguousOwnerFallback: "bare-kebab",
      translateReceiverLeaf: (e) => {
        const r = translateExpr(e, checker, _strategy, paramNames, synthCell);
        if (isTranslateExprUnsupported(r)) {
          return { kind: "unsupported", reason: r.unsupported };
        }
        return { kind: "expr", expr: r };
      },
    };
    const card = tryBuildL1Cardinality(expr, l1Ctx, sigOptions);
    if (card !== null) {
      return lowerExpr(lowerL1Expr(card));
    }
    const member = buildL1MemberAccess(expr, l1Ctx, sigOptions);
    if ("unsupported" in member) {
      return member;
    }
    return lowerExpr(lowerL1Expr(member));
  }
  // Defensively unwrap parens for the paren-stripping invariant — a
  // paren-wrapped property access (`(a).b`) routes through the same
  // Member dispatch as `a.b`.
  const stripped = unwrapParens(expr) as ts.Expression;
  if (
    stripped !== expr &&
    ts.isPropertyAccessExpression(stripped) &&
    (stripped.flags & ts.NodeFlags.OptionalChain) === 0
  ) {
    return translateExpr(stripped, checker, _strategy, paramNames, synthCell);
  }

  // Binary expression: a >= b -> binop(opGe(), a, b)
  if (ts.isBinaryExpression(expr)) {
    // M4 P2: recognize nullish surface forms (`x == null`,
    // `x === undefined`, the long form, `typeof x === 'undefined'`,
    // …) and collapse them to L1 `IsNullish`. Runs *before* the
    // loose-eq rejection below so `x == null` (a loose-eq nullish)
    // gets folded, not rejected. Mirrors the body-side recognition
    // site in `translate-body.ts`.
    //
    // The translate callback unwraps `translateExpr`'s discriminated-
    // union result (post-P3 `TranslateExprResult`); an upstream
    // `{ unsupported }` propagates through the recognizer and is
    // surfaced by the entry-point caller.
    //
    // The recognizer takes the checker for two soundness gates:
    // (1) TS-type nullability — only fold when the operand's
    // declared type includes `null`/`undefined`/`void`; (2)
    // resolved-`undefined` — verify the `undefined` identifier
    // resolves to the global symbol, not a shadowed local.
    const translate: NullishTranslate = (sub) => {
      const subResult = translateExpr(
        sub,
        checker,
        _strategy,
        paramNames,
        synthCell,
      );
      if (isTranslateExprUnsupported(subResult)) {
        return subResult;
      }
      return ir1FromL2(irWrap(subResult));
    };
    const recognized = recognizeNullishForm(expr, checker, translate);
    if (recognized !== null && !("unsupported" in recognized)) {
      return lowerExpr(lowerL1Expr(recognized));
    }
    // M4 P3: reject any surviving loose equality (== / !=). The
    // nullish recognizer above consumes loose-eq nullish forms; what
    // reaches here is `==` / `!=` against non-nullish operands, which
    // is inherently ambiguous in TS coercion semantics. Falling
    // through to the `ast.var(expr.getText())` path below would emit
    // raw source text as a Pant variable name, violating
    // conservative-refusal policy 3(b). Return an explicit `unsupported`
    // marker — entry-point callers (extractAssertionGuard,
    // buildSubstitutionMap, scanBodyForGuards) propagate it and bail.
    if (
      expr.operatorToken.kind === ts.SyntaxKind.EqualsEqualsToken ||
      expr.operatorToken.kind === ts.SyntaxKind.ExclamationEqualsToken
    ) {
      return {
        unsupported: "loose equality (== / !=) is unsupported; use === / !==",
      };
    }
    const left = translateExpr(
      expr.left,
      checker,
      _strategy,
      paramNames,
      synthCell,
    );
    if (isTranslateExprUnsupported(left)) {
      return left;
    }
    const right = translateExpr(
      expr.right,
      checker,
      _strategy,
      paramNames,
      synthCell,
    );
    if (isTranslateExprUnsupported(right)) {
      return right;
    }
    const op = translateOperator(expr.operatorToken.kind);
    if (op === null) {
      return ast.var(expr.getText());
    }
    return ast.binop(op, left, right);
  }

  // Prefix unary: !x -> unop(opNot(), x), -x -> unop(opNeg(), x)
  if (ts.isPrefixUnaryExpression(expr)) {
    if (expr.operator === ts.SyntaxKind.ExclamationToken) {
      const operand = translateExpr(
        expr.operand,
        checker,
        _strategy,
        paramNames,
        synthCell,
      );
      if (isTranslateExprUnsupported(operand)) {
        return operand;
      }
      return ast.unop(ast.opNot(), operand);
    }
    if (expr.operator === ts.SyntaxKind.MinusToken) {
      const operand = translateExpr(
        expr.operand,
        checker,
        _strategy,
        paramNames,
        synthCell,
      );
      if (isTranslateExprUnsupported(operand)) {
        return operand;
      }
      return ast.unop(ast.opNeg(), operand);
    }
  }

  // Transparent wrappers — unwrap before any translation, including
  // before falling through to the raw-text fallback at the end of this
  // function. Without this, `(x == 0) as boolean` and `(x == 0)!` would
  // bypass the loose-equality rejection above and emit raw source text
  // as a Pant variable name. Mirrors `unwrapExpression` in
  // translate-body.ts and the wrapper handling in `isPureExpression` /
  // `containsUnsupportedOperator`.
  if (
    ts.isParenthesizedExpression(expr) ||
    ts.isAsExpression(expr) ||
    ts.isNonNullExpression(expr) ||
    ts.isSatisfiesExpression(expr)
  ) {
    return translateExpr(
      expr.expression,
      checker,
      _strategy,
      paramNames,
      synthCell,
    );
  }

  // `this` keyword
  if (expr.kind === ts.SyntaxKind.ThisKeyword) {
    return ast.var(paramNames.get("this") ?? "this");
  }

  // Identifier — use param name mapping if available
  if (ts.isIdentifier(expr)) {
    return ast.var(paramNames.get(expr.text) ?? expr.text);
  }

  // Numeric literal
  if (ts.isNumericLiteral(expr)) {
    return ast.litNat(Number(expr.text));
  }

  // String literal
  if (ts.isStringLiteral(expr)) {
    return ast.litString(expr.text);
  }

  // Fallback
  return ast.var(expr.getText());
}

export function translateOperator(kind: ts.SyntaxKind): OpaqueBinop | null {
  const ast = getAst();
  switch (kind) {
    case ts.SyntaxKind.GreaterThanEqualsToken:
      return ast.opGe();
    case ts.SyntaxKind.LessThanEqualsToken:
      return ast.opLe();
    case ts.SyntaxKind.GreaterThanToken:
      return ast.opGt();
    case ts.SyntaxKind.LessThanToken:
      return ast.opLt();
    case ts.SyntaxKind.EqualsEqualsEqualsToken:
      return ast.opEq();
    case ts.SyntaxKind.ExclamationEqualsEqualsToken:
      return ast.opNeq();
    // M4 P3: loose equality (`==` / `!=`) is unsupported. Patch 2's
    // nullish recognizer consumes `x == null` / `x != null` before
    // this dispatcher is reached on the pure path; any `==`/`!=`
    // here has ambiguous intent and we reject (return null) to steer
    // the programmer toward `===`/`!==`.
    case ts.SyntaxKind.EqualsEqualsToken:
    case ts.SyntaxKind.ExclamationEqualsToken:
      return null;
    case ts.SyntaxKind.AmpersandAmpersandToken:
      return ast.opAnd();
    case ts.SyntaxKind.BarBarToken:
      return ast.opOr();
    case ts.SyntaxKind.PlusToken:
      return ast.opAdd();
    case ts.SyntaxKind.MinusToken:
      return ast.opSub();
    case ts.SyntaxKind.AsteriskToken:
      return ast.opMul();
    case ts.SyntaxKind.SlashToken:
      return ast.opDiv();
    default:
      return null;
  }
}

function capitalize(s: string): string {
  return s.charAt(0).toUpperCase() + s.slice(1);
}

export function shortParamName(
  typeName: string,
  existingNames: Set<string>,
  synthCell?: SynthCell,
): string {
  let name = typeName[0]!.toLowerCase();
  let suffix = 1;
  while (
    existingNames.has(name) ||
    (synthCell ? cellIsUsed(synthCell, name) : false)
  ) {
    name = typeName[0]!.toLowerCase() + suffix;
    suffix++;
  }
  if (synthCell) {
    cellRegisterName(synthCell, name);
  }
  return name;
}

/**
 * Translate a TypeScript function signature to a Pantagruel declaration.
 */
export function translateSignature(
  sourceFile: SourceFile,
  functionName: string,
  strategy: NumericStrategy,
  synthCell?: SynthCell,
  overrides?: Map<string, string>,
): TranslatedSignature {
  const checker = sourceFile.getProject().getTypeChecker().compilerObject;
  const { node, className } = findFunction(sourceFile, functionName);
  // Strip class qualifier for use in Pantagruel identifiers
  const baseName = toPantTermName(
    functionName.includes(".") ? functionName.split(".", 2)[1]! : functionName,
  );
  const classification = classifyFunction(node, checker);
  const sig = checker.getSignatureFromDeclaration(node);
  if (!sig) {
    throw new Error(`Cannot get signature for: ${functionName}`);
  }

  // Build params, prepending `this` for class methods
  const params: Array<{ name: string; type: string }> = [];
  const paramNameMap = new Map<string, string>();

  if (className) {
    const existingParamNames = new Set(sig.getParameters().map((p) => p.name));
    const pName = shortParamName(className, existingParamNames, synthCell);
    params.push({ name: pName, type: className });
    paramNameMap.set("this", pName);
  }

  for (const param of sig.getParameters()) {
    const symbolType = checker.getTypeOfSymbol(param);
    const defaultType = mapTsType(symbolType, checker, strategy, synthCell);
    const paramType = overrides?.get(param.name) ?? defaultType;
    const pantName = synthCell
      ? cellRegisterName(synthCell, toPantTermName(param.name))
      : toPantTermName(param.name);
    params.push({ name: pantName, type: paramType });
    paramNameMap.set(param.name, pantName);
  }

  const guard = detectGuard(node, checker, strategy, paramNameMap, synthCell);

  if (classification === "pure") {
    const returnType = mapTsType(
      sig.getReturnType(),
      checker,
      strategy,
      synthCell,
    );
    const decl: PantRule = {
      kind: "rule",
      name: baseName,
      params,
      returnType,
    };
    if (guard) {
      decl.guard = guard;
    }
    return { declaration: decl, classification, paramNameMap, synthCell };
  } else {
    const decl: PantAction = {
      kind: "action",
      label: capitalize(baseName),
      params,
    };
    if (guard) {
      decl.guard = guard;
    }
    return { declaration: decl, classification, paramNameMap, synthCell };
  }
}
