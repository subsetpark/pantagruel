import type { SourceFile } from "ts-morph";
import ts from "typescript";
import {
  Apply,
  Binop,
  Lit,
  type PantExpr,
  renderExpr,
  Unop,
  Var,
} from "./pant-expr.js";
import { mapTsType, type NumericStrategy } from "./translate-types.js";
import type { PantAction, PantDeclaration, PantRule } from "./types.js";

export type Classification = "pure" | "mutating";

export interface TranslatedSignature {
  declaration: PantDeclaration;
  classification: Classification;
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
 * Mutating: void return type or has property assignments in body.
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

  if (node.body && hasPropertyAssignment(node.body)) {
    return "mutating";
  }

  return "pure";
}

function hasPropertyAssignment(node: ts.Node): boolean {
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
  paramNames: Map<string, string>,
): PantExpr | undefined {
  const paramIndex = isAssertionCall(checker, call);
  if (paramIndex === null) {
    return undefined;
  }
  if (paramIndex >= call.arguments.length) {
    return undefined;
  }
  const arg = call.arguments[paramIndex]!;
  return translateExpr(arg, checker, strategy, paramNames);
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
  callerParamNames: Map<string, string>,
): Map<string, string> | null {
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

    const actual = translateExpr(
      call.arguments[i]!,
      checker,
      strategy,
      callerParamNames,
    );
    const rendered = renderExpr(actual);
    const safeRendered =
      actual.kind === "var" || actual.kind === "literal"
        ? rendered
        : `(${rendered})`;
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
  callerParamNames: Map<string, string>,
  visited: Set<ts.Node>,
): PantExpr[] {
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
 * Check whether a then-branch is effectively a no-op (empty block or
 * empty statement), so extracting the condition as a guard won't
 * silently discard meaningful work.
 */
function isNoopThenBranch(stmt: ts.Statement): boolean {
  return (
    ts.isEmptyStatement(stmt) ||
    (ts.isBlock(stmt) && stmt.statements.length === 0)
  );
}

/**
 * Check if an if-statement matches a guard pattern (pure condition + throw branch).
 * Returns "positive" for if/no-op/else-throw, "negative" for if-throw (no else),
 * or null if the pattern doesn't match.
 */
function classifyGuardIf(stmt: ts.IfStatement): "positive" | "negative" | null {
  if (!isPureExpression(stmt.expression)) {
    return null;
  }
  if (
    stmt.elseStatement &&
    isNoopThenBranch(stmt.thenStatement) &&
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
  paramNames: Map<string, string>,
  visited: Set<ts.Node>,
): PantExpr[] {
  const guards: PantExpr[] = [];

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
      guards.push(
        translateExpr(stmt.expression, checker, strategy, paramNames),
      );
      continue;
    }
    if (guardKind === "negative") {
      if (
        ts.isPrefixUnaryExpression(stmt.expression) &&
        stmt.expression.operator === ts.SyntaxKind.ExclamationToken
      ) {
        guards.push(
          translateExpr(stmt.expression.operand, checker, strategy, paramNames),
        );
        continue;
      }
      guards.push(
        Unop(
          "~",
          translateExpr(stmt.expression, checker, strategy, paramNames),
        ),
      );
      continue;
    }

    // If-statement doesn't match a guard pattern — stop scanning
    break;
  }

  return guards;
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
  paramNames: Map<string, string>,
): PantExpr | undefined {
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
  );

  if (guards.length === 0) {
    return undefined;
  }
  return guards.reduce((acc, g) => Binop("and", acc, g));
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
 * Translate a TypeScript expression to a PantExpr AST node (best-effort).
 */
export function translateExpr(
  expr: ts.Expression,
  _checker: ts.TypeChecker,
  _strategy: NumericStrategy,
  paramNames: Map<string, string>,
): PantExpr {
  // Property access: a.balance -> Apply("balance", obj)
  if (ts.isPropertyAccessExpression(expr)) {
    const obj = translateExpr(expr.expression, _checker, _strategy, paramNames);
    return Apply(expr.name.text, obj);
  }

  // Binary expression: a >= b -> Binop(">=", a, b)
  if (ts.isBinaryExpression(expr)) {
    const left = translateExpr(expr.left, _checker, _strategy, paramNames);
    const right = translateExpr(expr.right, _checker, _strategy, paramNames);
    const op = translateOperator(expr.operatorToken.kind);
    if (op === "?") {
      return Lit(expr.getText());
    }
    return Binop(op, left, right);
  }

  // Prefix unary: !x -> Unop("~", x), -x -> Unop("-", x)
  if (ts.isPrefixUnaryExpression(expr)) {
    if (expr.operator === ts.SyntaxKind.ExclamationToken) {
      return Unop(
        "~",
        translateExpr(expr.operand, _checker, _strategy, paramNames),
      );
    }
    if (expr.operator === ts.SyntaxKind.MinusToken) {
      return Unop(
        "-",
        translateExpr(expr.operand, _checker, _strategy, paramNames),
      );
    }
  }

  // Parenthesized — unwrap (parens are a rendering concern)
  if (ts.isParenthesizedExpression(expr)) {
    return translateExpr(expr.expression, _checker, _strategy, paramNames);
  }

  // `this` keyword
  if (expr.kind === ts.SyntaxKind.ThisKeyword) {
    return Var(paramNames.get("this") ?? "this");
  }

  // Identifier — use param name mapping if available
  if (ts.isIdentifier(expr)) {
    return Var(paramNames.get(expr.text) ?? expr.text);
  }

  // Numeric literal
  if (ts.isNumericLiteral(expr)) {
    return Lit(expr.text);
  }

  // String literal
  if (ts.isStringLiteral(expr)) {
    return Lit(`"${expr.text.replace(/\\/gu, "\\\\").replace(/"/gu, '\\"')}"`);
  }

  // Fallback
  return Lit(expr.getText());
}

export function translateOperator(kind: ts.SyntaxKind): string {
  switch (kind) {
    case ts.SyntaxKind.GreaterThanEqualsToken:
      return ">=";
    case ts.SyntaxKind.LessThanEqualsToken:
      return "<=";
    case ts.SyntaxKind.GreaterThanToken:
      return ">";
    case ts.SyntaxKind.LessThanToken:
      return "<";
    case ts.SyntaxKind.EqualsEqualsEqualsToken:
    case ts.SyntaxKind.EqualsEqualsToken:
      return "=";
    case ts.SyntaxKind.ExclamationEqualsEqualsToken:
    case ts.SyntaxKind.ExclamationEqualsToken:
      return "~=";
    case ts.SyntaxKind.AmpersandAmpersandToken:
      return "and";
    case ts.SyntaxKind.BarBarToken:
      return "or";
    case ts.SyntaxKind.PlusToken:
      return "+";
    case ts.SyntaxKind.MinusToken:
      return "-";
    case ts.SyntaxKind.AsteriskToken:
      return "*";
    case ts.SyntaxKind.SlashToken:
      return "/";
    default:
      return "?";
  }
}

function capitalize(s: string): string {
  return s.charAt(0).toUpperCase() + s.slice(1);
}

export function shortParamName(
  typeName: string,
  existingNames: Set<string>,
): string {
  let name = typeName[0]!.toLowerCase();
  let suffix = 1;
  while (existingNames.has(name)) {
    name = typeName[0]!.toLowerCase() + suffix;
    suffix++;
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
): TranslatedSignature {
  const checker = sourceFile.getProject().getTypeChecker().compilerObject;
  const { node, className } = findFunction(sourceFile, functionName);
  // Strip class qualifier for use in Pantagruel identifiers
  const baseName = functionName.includes(".")
    ? functionName.split(".", 2)[1]!
    : functionName;
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
    const pName = shortParamName(className, existingParamNames);
    params.push({ name: pName, type: className });
    paramNameMap.set("this", pName);
  }

  for (const param of sig.getParameters()) {
    const paramType = mapTsType(
      checker.getTypeOfSymbol(param),
      checker,
      strategy,
    );
    params.push({ name: param.name, type: paramType });
    paramNameMap.set(param.name, param.name);
  }

  const guard = detectGuard(node, checker, strategy, paramNameMap);

  if (classification === "pure") {
    const returnType = mapTsType(sig.getReturnType(), checker, strategy);
    const decl: PantRule = {
      kind: "rule",
      name: baseName,
      params,
      returnType,
    };
    if (guard) {
      decl.guard = guard;
    }
    return { declaration: decl, classification };
  } else {
    const decl: PantAction = {
      kind: "action",
      label: capitalize(baseName),
      params,
    };
    if (guard) {
      decl.guard = guard;
    }
    return { declaration: decl, classification };
  }
}
