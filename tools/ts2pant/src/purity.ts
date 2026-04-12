/**
 * Conservative purity oracle for TypeScript call expressions.
 *
 * Standard name: Allowlist-based must-analysis over a {pure, effectful}
 * abstract domain (trivial two-point lattice).
 *
 * Framework: Abstract interpretation (Cousot & Cousot, POPL 1977).
 *   - Domain: {pure, effectful} with pure ⊑ effectful.
 *   - Default: effectful (⊤). Only positive evidence yields pure (⊥).
 *   - This is a must-analysis: returns "pure" only when provably so.
 *
 * Allowlist approach: Equivalent to the stub annotation technique used by
 * the Checker Framework (@Pure/@SideEffectFree, Dietl et al. ICSE 2011)
 * and Closure Compiler externs. The allowlist encodes the effect signature
 * of known library functions without requiring whole-program inference.
 *
 * Higher-order functions: Call-site callback specialization per
 * Lucassen & Gifford (POPL 1988). The effect of arr.map(f) depends on
 * the effect of f at each call site, not on a fixed effect for map.
 *
 * Pragmatic assumptions (shared with Webpack, Rollup, Closure Compiler):
 *   - Property access does not trigger effectful getters.
 *   - Implicit toString()/valueOf() coercions in template literals are pure.
 *   - Combinator names (pipe, flow, identity) match by identifier text,
 *     not by import source. Argument purity checking provides a safety net.
 *
 * Tiers:
 *   1a: Known-pure builtin allowlist (Math methods, String/Array methods)
 *   1b: Effect-TS combinator allowlist (pipe, flow, identity)
 *   1c: Conservative default (unknown = effectful)
 *
 * Ref: Cousot & Cousot, "Abstract Interpretation", POPL 1977.
 * Ref: Lucassen & Gifford, "Polymorphic Effect Systems", POPL 1988.
 * Ref: Dietl et al., "Building and Using Pluggable Type-Checkers", ICSE 2011.
 * Ref: Talpin & Jouvelot, "The Type and Effect Discipline", I&C 1994.
 */
import ts from "typescript";

// ---------------------------------------------------------------------------
// Tier 1a — Known-pure builtin allowlists
// ---------------------------------------------------------------------------

/**
 * Pure Math methods (enumerated, not blanket namespace).
 * Math.random() is excluded — it is non-deterministic.
 */
const PURE_MATH_METHODS: ReadonlySet<string> = new Set([
  "abs",
  "acos",
  "acosh",
  "asin",
  "asinh",
  "atan",
  "atan2",
  "atanh",
  "cbrt",
  "ceil",
  "clz32",
  "cos",
  "cosh",
  "exp",
  "expm1",
  "floor",
  "fround",
  "hypot",
  "imul",
  "log",
  "log10",
  "log1p",
  "log2",
  "max",
  "min",
  "pow",
  "round",
  "sign",
  "sin",
  "sinh",
  "sqrt",
  "tan",
  "tanh",
  "trunc",
]);

/**
 * Pure methods by primitive receiver type.
 *
 * String methods that accept RegExp or replacement callbacks are excluded
 * (split, replace, replaceAll, match, search) — these can execute user code
 * via RegExp Symbol.replace/Symbol.match hooks.
 */
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
      "repeat",
      "padStart",
      "padEnd",
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

    // Pure Math methods: Math.max(...), Math.abs(...), etc.
    // Math.random() is excluded (non-deterministic).
    if (
      ts.isIdentifier(receiver) &&
      receiver.text === "Math" &&
      PURE_MATH_METHODS.has(methodName)
    ) {
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

      // Higher-order array methods: pure if callback AND all eagerly-evaluated
      // args (thisArg, initialValue) are side-effect-free.
      if (HO_PURE_ARRAY_METHODS.has(methodName) && expr.arguments.length >= 1) {
        const [callback, ...restArgs] = expr.arguments;
        return (
          callback !== undefined &&
          isArrowPure(callback, checker) &&
          restArgs.every((arg) => expressionIsPure(arg, checker))
        );
      }
    }

    // Tier 1b: Effect-TS impure runners
    if (EFFECT_IMPURE_RUNNERS.has(methodName)) {
      return false;
    }
  }

  // Tier 1b: bare identifier calls — pure combinators with pure arguments.
  // Arguments must be checked: identity(sideEffect()) is impure.
  if (ts.isIdentifier(expr.expression)) {
    if (EFFECT_PURE_COMBINATORS.has(expr.expression.text)) {
      return expr.arguments.every((arg) => expressionIsPure(arg, checker));
    }
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
 * Compositional purity analysis over the expression AST.
 *
 * Standard name: Structural induction over expression forms, classifying
 * each into the {pure, effectful} lattice. This is the standard recursive
 * descent approach described in Nielson, Nielson & Hankin, "Principles of
 * Program Analysis" (Springer 1999), Ch. 2 — instantiated for a trivial
 * two-point effect domain rather than a full type-and-effect system.
 *
 * Soundness invariant: returns true only when the expression is provably
 * side-effect-free. Returns false (conservative) for any unknown form.
 *
 * Pragmatic assumption: property access is assumed pure (no effectful
 * getters). This matches the universal assumption in JavaScript bundler
 * tree-shaking (Webpack, Rollup, Closure Compiler). Getter-bearing types
 * in specification-relevant code are out of scope for ts2pant.
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

  // Property access: recurse into receiver. Assumes no effectful getters
  // (see module-level pragmatic assumptions documentation).
  if (ts.isPropertyAccessExpression(expr)) {
    return expressionIsPure(expr.expression, checker);
  }

  if (ts.isElementAccessExpression(expr)) {
    return (
      expressionIsPure(expr.expression, checker) &&
      expr.argumentExpression !== undefined &&
      expressionIsPure(expr.argumentExpression, checker)
    );
  }

  // Arrow functions and function expressions are pure value-creating
  // expressions — the function body is not executed at evaluation time.
  if (ts.isArrowFunction(expr) || ts.isFunctionExpression(expr)) {
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
    return expr.elements.every((el) => {
      // Spread elements: [...arr] is pure if arr is pure (array iteration
      // is pure for built-in arrays). Per Talpin & Jouvelot 1994,
      // allocation effects are maskable — the fresh array is local.
      if (ts.isSpreadElement(el)) {
        return expressionIsPure(el.expression, checker);
      }
      return expressionIsPure(el, checker);
    });
  }

  // Object literals: { a: expr } is pure if all property values are pure.
  // Allocation is a maskable effect (Talpin & Jouvelot, I&C 1994) — the
  // fresh object is local and does not escape to observable state.
  if (ts.isObjectLiteralExpression(expr)) {
    return expr.properties.every((prop) => {
      if (ts.isPropertyAssignment(prop)) {
        return expressionIsPure(prop.initializer, checker);
      }
      if (ts.isShorthandPropertyAssignment(prop)) {
        return true; // { x } just reads a variable
      }
      if (ts.isSpreadAssignment(prop)) {
        return expressionIsPure(prop.expression, checker);
      }
      // Computed property names, method declarations, accessors → conservative
      return false;
    });
  }

  if (ts.isTemplateExpression(expr)) {
    // Assumes implicit toString() on interpolated values is pure
    // (see module-level pragmatic assumptions documentation).
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
