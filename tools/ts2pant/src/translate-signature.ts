import ts from "typescript";
import { Apply, Binop, Lit, type PantExpr, Unop, Var } from "./pant-expr.js";
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
 */
export function findFunction(
  program: ts.Program,
  fileName: string,
  functionName: string,
): { node: ts.FunctionDeclaration | ts.MethodDeclaration; className?: string } {
  const sourceFile = program.getSourceFile(fileName);
  if (!sourceFile) {
    throw new Error(`Source file not found: ${fileName}`);
  }

  let match:
    | {
        node: ts.FunctionDeclaration | ts.MethodDeclaration;
        className?: string;
      }
    | undefined;

  // Search top-level functions
  for (const stmt of sourceFile.statements) {
    if (ts.isFunctionDeclaration(stmt) && stmt.name?.text === functionName) {
      if (stmt.body) {
        return { node: stmt };
      }
      match ??= { node: stmt };
    }
  }

  // Search class methods
  for (const stmt of sourceFile.statements) {
    if (ts.isClassDeclaration(stmt) && stmt.name) {
      for (const member of stmt.members) {
        if (
          ts.isMethodDeclaration(member) &&
          ts.isIdentifier(member.name) &&
          member.name.text === functionName
        ) {
          if (member.body) {
            return { node: member, className: stmt.name.text };
          }
          match ??= { node: member, className: stmt.name.text };
        }
      }
    }
  }

  if (match) {
    return match;
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
 * Detect guard patterns in a function body.
 * Patterns:
 *   if (cond) { body } else { throw ... }
 *   if (!cond) { throw ... } (early return guard)
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

  for (const stmt of node.body.statements) {
    // Only extract guards from leading precondition checks
    if (!ts.isIfStatement(stmt)) {
      break;
    }

    // Pattern 1: if (cond) { ... } else { throw }
    if (stmt.elseStatement && blockThrows(stmt.elseStatement)) {
      return translateExpr(stmt.expression, checker, strategy, paramNames);
    }

    // Pattern 2: if (!cond) { throw }  =>  guard is cond
    if (
      stmt.thenStatement &&
      blockThrows(stmt.thenStatement) &&
      !stmt.elseStatement
    ) {
      // Negate: if the condition is a prefix !, strip it
      if (
        ts.isPrefixUnaryExpression(stmt.expression) &&
        stmt.expression.operator === ts.SyntaxKind.ExclamationToken
      ) {
        return translateExpr(
          stmt.expression.operand,
          checker,
          strategy,
          paramNames,
        );
      }
      // Otherwise negate the whole expression
      return Unop(
        "~",
        translateExpr(stmt.expression, checker, strategy, paramNames),
      );
    }

    // If it's an if-statement but doesn't match a guard pattern, stop scanning
    break;
  }

  return undefined;
}

function blockThrows(node: ts.Statement): boolean {
  if (ts.isBlock(node)) {
    return node.statements.some((s) => ts.isThrowStatement(s));
  }
  return ts.isThrowStatement(node);
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
  program: ts.Program,
  fileName: string,
  functionName: string,
  strategy: NumericStrategy,
): TranslatedSignature {
  const checker = program.getTypeChecker();
  const { node, className } = findFunction(program, fileName, functionName);
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
      name: functionName,
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
      label: capitalize(functionName),
      params,
    };
    if (guard) {
      decl.guard = guard;
    }
    return { declaration: decl, classification };
  }
}
