import ts from "typescript";
import { isEffectFree } from "./purity.js";
import { isTsNullish } from "./translate-types.js";

export interface EffectErrorMode {
  name: string;
  type: ts.Type;
}

export interface ErrorYield {
  errorName: string;
  expression: ts.NewExpression;
}

export interface ErrorModeGuard {
  mode: EffectErrorMode;
  condition: ts.Expression;
}

/**
 * Extract the recoverable E-channel modes from an Effect<A, E, R> type.
 *
 * Returns null for non-Effect types, a never E-channel, nullish-only unions,
 * or unnameable error members. Unions are enumerated member-by-member after
 * stripping nullish members, matching the discriminated-union helper's
 * conservative union traversal.
 */
export function extractEffectErrorModes(
  returnType: ts.Type,
  checker: ts.TypeChecker,
): EffectErrorMode[] | null {
  const typeArgs = effectTypeArguments(returnType);
  if (!typeArgs || typeArgs.length < 2) {
    return null;
  }

  const errorType = typeArgs[1]!;
  if (isNeverType(errorType)) {
    return null;
  }

  const members = errorType.isUnion()
    ? errorType.types.filter((member) => !isTsNullish(member))
    : [errorType];
  if (members.length === 0 || members.some(isNeverType)) {
    return null;
  }

  const modes: EffectErrorMode[] = [];
  const seen = new Set<string>();
  for (const member of members) {
    const name = errorModeName(member, checker);
    if (!name || seen.has(name)) {
      return null;
    }
    seen.add(name);
    modes.push({ name, type: member });
  }
  return modes.length > 0 ? modes : null;
}

/**
 * Recognize the Effect.gen typed-error idiom `yield* new ErrorClass(...)`.
 */
export function recognizeErrorYield(node: ts.Node): ErrorYield | null {
  const expr = ts.isExpressionStatement(node) ? node.expression : node;
  if (!ts.isYieldExpression(expr) || expr.asteriskToken === undefined) {
    return null;
  }
  if (!expr.expression || !ts.isNewExpression(expr.expression)) {
    return null;
  }
  const errorName = constructorName(expr.expression.expression);
  return errorName === null ? null : { errorName, expression: expr.expression };
}

/**
 * Recover `cond` from `if (cond) { yield* new ErrorClass(...) }`.
 *
 * The condition must be side-effect-free and the yielded constructor must
 * match one of the enumerated E-channel modes; otherwise this helper bails.
 */
export function recoverErrorModeGuard(
  stmt: ts.IfStatement,
  modes: readonly EffectErrorMode[],
  checker: ts.TypeChecker,
): ErrorModeGuard | null {
  if (stmt.elseStatement || !isEffectFree(stmt.expression, checker)) {
    return null;
  }

  const yielded = recognizeErrorYield(singleThenStatement(stmt.thenStatement));
  if (!yielded) {
    return null;
  }

  const mode = modes.find((candidate) => candidate.name === yielded.errorName);
  return mode ? { mode, condition: stmt.expression } : null;
}

function effectTypeArguments(type: ts.Type): readonly ts.Type[] | null {
  const ref = type as ts.TypeReference;
  const target = ref.target;
  if (!target || !isEffectTypeTarget(target)) {
    return null;
  }
  return ref.typeArguments ?? [];
}

function isEffectTypeTarget(target: ts.Type): boolean {
  const symbol = target.aliasSymbol ?? target.symbol;
  if (symbol?.getName() !== "Effect") {
    return false;
  }
  return (
    symbol
      .getDeclarations()
      ?.some((decl) =>
        /node_modules\/effect\/(?:dist\/dts|src)\/Effect\.(?:d\.ts|ts)$/u.test(
          decl.getSourceFile().fileName,
        ),
      ) ?? false
  );
}

function isNeverType(type: ts.Type): boolean {
  return (type.flags & ts.TypeFlags.Never) !== 0;
}

function errorModeName(type: ts.Type, checker: ts.TypeChecker): string | null {
  const symbol = type.aliasSymbol ?? type.symbol;
  const name = symbol?.getName();
  if (name && name !== "__type") {
    return name;
  }

  const rendered = checker
    .typeToString(type)
    .replace(/^import\("[^"]+"\)\./u, "");
  return /^[A-Za-z_$][A-Za-z0-9_$]*$/u.test(rendered) ? rendered : null;
}

function constructorName(expr: ts.Expression): string | null {
  if (ts.isIdentifier(expr)) {
    return expr.text;
  }
  if (ts.isPropertyAccessExpression(expr)) {
    return expr.name.text;
  }
  return null;
}

function singleThenStatement(stmt: ts.Statement): ts.Statement {
  if (ts.isBlock(stmt) && stmt.statements.length === 1) {
    return stmt.statements[0]!;
  }
  return stmt;
}
