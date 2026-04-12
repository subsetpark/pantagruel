/**
 * Conservative purity oracle for TypeScript call expressions.
 *
 * Three-tier analysis (trivial instance of abstract interpretation over
 * a {pure, effectful} lattice — Cousot & Cousot, POPL 1977):
 *
 *   Tier 1a: Known-pure builtin allowlist (Math.*, non-mutating String/Array methods)
 *   Tier 1b: Effect-TS awareness (Effect<A,E,R> construction is always pure)
 *   Tier 1c: Conservative default (unknown = effectful)
 *
 * Ref: Lucassen & Gifford, Polymorphic Effect Systems, POPL 1988.
 */
import ts from "typescript";

// ---------------------------------------------------------------------------
// Tier 1a — Known-pure builtin allowlists
// ---------------------------------------------------------------------------

/** Namespaces where ALL methods are pure (e.g. Math.max, Math.abs). */
const PURE_NAMESPACES: ReadonlySet<string> = new Set(["Math"]);

/** Pure methods by primitive receiver type. */
const PURE_METHODS_BY_TYPE: ReadonlyMap<string, ReadonlySet<string>> = new Map([
  [
    "string",
    new Set([
      "indexOf",
      "slice",
      "substring",
      "includes",
      "startsWith",
      "endsWith",
      "trim",
      "toLowerCase",
      "toUpperCase",
      "charAt",
      "charCodeAt",
      "split",
      "replace",
      "replaceAll",
      "repeat",
      "padStart",
      "padEnd",
      "match",
      "search",
      "concat",
      "normalize",
    ]),
  ],
  [
    "number",
    new Set([
      "toFixed",
      "toPrecision",
      "toString",
      "toExponential",
      "toLocaleString",
    ]),
  ],
]);

/** Non-mutating array methods that take no callback. */
const PURE_ARRAY_METHODS: ReadonlySet<string> = new Set([
  "at",
  "concat",
  "flat",
  "includes",
  "indexOf",
  "join",
  "keys",
  "lastIndexOf",
  "slice",
  "toString",
  "values",
]);

/** Array methods that are pure if their callback argument is side-effect-free. */
const HO_PURE_ARRAY_METHODS: ReadonlySet<string> = new Set([
  "every",
  "filter",
  "find",
  "findIndex",
  "flatMap",
  "map",
  "reduce",
  "reduceRight",
  "some",
]);

// ---------------------------------------------------------------------------
// Tier 1b — Effect-TS awareness
// ---------------------------------------------------------------------------

/** Known-pure Effect-TS combinators (from effect/Function). */
const EFFECT_PURE_COMBINATORS: ReadonlySet<string> = new Set([
  "pipe",
  "flow",
  "identity",
]);

/** Known-impure Effect-TS runners — excluded even though they return Effect. */
const EFFECT_IMPURE_RUNNERS: ReadonlySet<string> = new Set([
  "runSync",
  "runPromise",
  "runSyncExit",
  "runPromiseExit",
  "runFork",
]);

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/**
 * Determine whether a call expression is known to be pure (no side effects).
 *
 * Returns `true` only when there is positive evidence of purity.
 * Returns `false` (conservative) for any unknown call.
 */
export function isKnownPureCall(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
): boolean {
  // --- Tier 1a: builtin allowlist ---

  if (ts.isPropertyAccessExpression(expr.expression)) {
    const methodName = expr.expression.name.text;
    const receiver = expr.expression.expression;

    // Pure namespace: Math.max(...), Math.abs(...), etc.
    if (ts.isIdentifier(receiver) && PURE_NAMESPACES.has(receiver.text)) {
      return true;
    }

    // Method on a typed receiver
    const receiverType = checker.getTypeAtLocation(receiver);

    // String methods
    if (receiverType.flags & ts.TypeFlags.StringLike) {
      const pureMethods = PURE_METHODS_BY_TYPE.get("string");
      if (pureMethods?.has(methodName)) {
        return true;
      }
    }

    // Number methods
    if (receiverType.flags & ts.TypeFlags.NumberLike) {
      const pureMethods = PURE_METHODS_BY_TYPE.get("number");
      if (pureMethods?.has(methodName)) {
        return true;
      }
    }

    // Array methods
    if (checker.isArrayType(receiverType)) {
      if (PURE_ARRAY_METHODS.has(methodName)) {
        return true;
      }

      // Higher-order array methods: pure if callback is side-effect-free
      if (HO_PURE_ARRAY_METHODS.has(methodName) && expr.arguments.length >= 1) {
        const callback = expr.arguments[0]!;
        if (isArrowPure(callback, checker)) {
          return true;
        }
        // Non-arrow callback (function expression, identifier) → conservative
        return false;
      }
    }

    // Tier 1b: Effect-TS impure runners (check before return-type detection)
    if (EFFECT_IMPURE_RUNNERS.has(methodName)) {
      return false;
    }
  }

  // Tier 1a: bare identifier calls — check if it's a pure combinator
  if (ts.isIdentifier(expr.expression)) {
    if (EFFECT_PURE_COMBINATORS.has(expr.expression.text)) {
      return true;
    }
  }

  // --- Tier 1b: Effect-TS return type detection ---
  if (isEffectReturningCall(expr, checker)) {
    return true;
  }

  // --- Tier 1c: conservative default ---
  return false;
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/**
 * Check whether an arrow function callback is side-effect-free.
 *
 * For expression-bodied arrows: check the expression.
 * For block-bodied arrows with a single return: check the return expression.
 * All other shapes are conservatively treated as impure.
 */
function isArrowPure(
  callback: ts.Expression,
  checker: ts.TypeChecker,
): boolean {
  if (!ts.isArrowFunction(callback)) {
    return false;
  }

  if (ts.isBlock(callback.body)) {
    // Block body: only pure if it's a single return statement
    const stmts = callback.body.statements;
    if (
      stmts.length === 1 &&
      ts.isReturnStatement(stmts[0]!) &&
      stmts[0]!.expression
    ) {
      return expressionIsPure(stmts[0]!.expression, checker);
    }
    return false;
  }

  // Expression body
  return expressionIsPure(callback.body, checker);
}

/**
 * Recursively check whether an expression is free of side effects.
 *
 * This mirrors the logic of expressionHasSideEffects in translate-body.ts,
 * but inverted: known-pure calls return true, unknown calls return false.
 */
function expressionIsPure(
  expr: ts.Expression,
  checker: ts.TypeChecker,
): boolean {
  // Unwrap parentheses, type assertions, non-null assertions
  while (
    ts.isParenthesizedExpression(expr) ||
    ts.isAsExpression(expr) ||
    ts.isSatisfiesExpression(expr) ||
    ts.isNonNullExpression(expr)
  ) {
    expr = expr.expression;
  }

  // Literals, identifiers, property access are pure
  if (
    ts.isIdentifier(expr) ||
    ts.isNumericLiteral(expr) ||
    ts.isStringLiteral(expr) ||
    ts.isNoSubstitutionTemplateLiteral(expr) ||
    expr.kind === ts.SyntaxKind.TrueKeyword ||
    expr.kind === ts.SyntaxKind.FalseKeyword ||
    expr.kind === ts.SyntaxKind.NullKeyword ||
    expr.kind === ts.SyntaxKind.UndefinedKeyword
  ) {
    return true;
  }

  if (ts.isPropertyAccessExpression(expr) || ts.isElementAccessExpression(expr)) {
    return true;
  }

  if (ts.isBinaryExpression(expr)) {
    // Assignment operators are side-effectful
    if (
      expr.operatorToken.kind >= ts.SyntaxKind.EqualsToken &&
      expr.operatorToken.kind <= ts.SyntaxKind.CaretEqualsToken
    ) {
      return false;
    }
    return (
      expressionIsPure(expr.left, checker) &&
      expressionIsPure(expr.right, checker)
    );
  }

  if (ts.isPrefixUnaryExpression(expr) || ts.isPostfixUnaryExpression(expr)) {
    if (
      expr.operator === ts.SyntaxKind.PlusPlusToken ||
      expr.operator === ts.SyntaxKind.MinusMinusToken
    ) {
      return false;
    }
    return expressionIsPure(expr.operand, checker);
  }

  if (ts.isConditionalExpression(expr)) {
    return (
      expressionIsPure(expr.condition, checker) &&
      expressionIsPure(expr.whenTrue, checker) &&
      expressionIsPure(expr.whenFalse, checker)
    );
  }

  if (ts.isCallExpression(expr)) {
    return isKnownPureCall(expr, checker);
  }

  if (ts.isArrayLiteralExpression(expr)) {
    return expr.elements.every((el) => expressionIsPure(el, checker));
  }

  if (ts.isTemplateExpression(expr)) {
    return expr.templateSpans.every((span) =>
      expressionIsPure(span.expression, checker),
    );
  }

  // Side-effectful by default
  if (
    ts.isDeleteExpression(expr) ||
    ts.isNewExpression(expr) ||
    ts.isAwaitExpression(expr)
  ) {
    return false;
  }

  // Unknown expression kind → conservative
  return false;
}

/**
 * Tier 1b: Check if a call returns an Effect<A,E,R> value.
 *
 * Detection: the return type has the EffectTypeId branded property.
 * This is structural and version-resilient — avoids source-path heuristics.
 *
 * Known-impure runners (runSync, runPromise, etc.) are excluded upstream.
 */
function isEffectReturningCall(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
): boolean {
  try {
    const signature = checker.getResolvedSignature(expr);
    if (!signature) return false;

    const returnType = checker.getReturnTypeOfSignature(signature);
    return hasEffectTypeId(returnType);
  } catch {
    return false;
  }
}

/**
 * Check whether a type has the EffectTypeId branded property,
 * which is the structural marker for Effect<A,E,R>.
 */
function hasEffectTypeId(type: ts.Type): boolean {
  // Check direct properties for EffectTypeId
  const props = type.getProperties();
  for (const prop of props) {
    if (prop.name === "_tag" || prop.name === "EffectTypeId" || prop.name === "_op") {
      // Heuristic: if it has EffectTypeId, it's an Effect type
      if (prop.name === "EffectTypeId") {
        return true;
      }
    }
  }

  // Check alias: Effect types might be aliased
  if (type.aliasSymbol?.name === "Effect") {
    return true;
  }

  // Check symbol name
  if (type.symbol?.name === "Effect") {
    return true;
  }

  return false;
}
