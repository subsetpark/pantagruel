/**
 * Structural equality for TypeScript expression nodes.
 *
 * Compares two `ts.Expression` ASTs node-by-node by `kind` plus the
 * structural children that participate in identity (identifier text,
 * call arguments, etc.). The comparison is intentionally narrower than
 * a general AST equality — only the shapes that arise as nullish-test
 * operands in M4 are handled. Anything outside the supported shape
 * returns `false` (conservative).
 *
 * **Why not `node.getText()`?** Surface-text equality is sensitive to
 * whitespace, comments, quote style, and parenthesization (`a.b` vs
 * `a /* note *​/.b` would compare unequal). Those false non-matches
 * would let the long-form nullish recognizer drop a perfectly good
 * pair, falling through to a generic `||`/`&&` translation that emits
 * broken Pant for the surrounding shape. Walking the AST avoids the
 * trap. Pays forward into M5's `Member`-path normalization, which
 * needs the same predicate.
 *
 * **Supported node kinds:**
 * - `Identifier` — compare `.text`
 * - `PropertyAccessExpression` — recurse on `.expression`, compare `.name.text`
 * - `ElementAccessExpression` — recurse on `.expression` and `.argumentExpression`
 * - `CallExpression` — recurse on `.expression`, recurse pairwise on `.arguments`
 *   (spread arguments cause the comparison to bail conservatively)
 * - `ParenthesizedExpression`, `AsExpression`, `NonNullExpression`,
 *   `SatisfiesExpression` — unwrapped via `unwrapTransparentExpression`
 *   to mirror `translate-body.ts`'s `unwrapExpression`. So
 *   `(x as T) === null || x === undefined` matches operand identity
 *   and folds to a single `IsNullish(x)`.
 * - `ThisExpression` — always equal
 * - `StringLiteral` — compare `.text` (so quote-style differences don't matter)
 * - `NumericLiteral` — compare `.text`
 *
 * Anything else returns `false`. There is no fallback to `getText` —
 * conservative-refusal here means "not provably equal, treat as
 * different and don't fold" which preserves the legacy translation
 * for the surrounding shape.
 */

import ts from "typescript";

/**
 * Strip transparent wrappers — parentheses, `as` casts, non-null
 * assertions, and `satisfies` — to expose the underlying expression
 * for structural comparison. Mirrors `unwrapExpression` in
 * `translate-body.ts`: those wrappers don't change runtime value, so
 * `(x as T) === null || x === undefined` should fold to one
 * `IsNullish(x)` rather than miss the operand-identity match. The
 * helper is recursive, peeling nested wrappers in any order
 * (`(x as T)!`, `(x!) as T`, etc.).
 */
function unwrapTransparentExpression(e: ts.Expression): ts.Expression {
  let cur = e;
  while (
    ts.isParenthesizedExpression(cur) ||
    ts.isAsExpression(cur) ||
    ts.isNonNullExpression(cur) ||
    ts.isSatisfiesExpression(cur)
  ) {
    cur = cur.expression;
  }
  return cur;
}

export function structurallyEqualExpression(
  a: ts.Expression,
  b: ts.Expression,
): boolean {
  const ua = unwrapTransparentExpression(a);
  const ub = unwrapTransparentExpression(b);

  if (ua.kind !== ub.kind) {
    return false;
  }

  if (ts.isIdentifier(ua) && ts.isIdentifier(ub)) {
    return ua.text === ub.text;
  }

  if (ts.isPropertyAccessExpression(ua) && ts.isPropertyAccessExpression(ub)) {
    return (
      ua.name.text === ub.name.text &&
      structurallyEqualExpression(ua.expression, ub.expression)
    );
  }

  if (ts.isElementAccessExpression(ua) && ts.isElementAccessExpression(ub)) {
    return (
      structurallyEqualExpression(ua.expression, ub.expression) &&
      structurallyEqualExpression(ua.argumentExpression, ub.argumentExpression)
    );
  }

  if (ts.isCallExpression(ua) && ts.isCallExpression(ub)) {
    // Bail on spread arguments — `f(...arr)` and `f(a, b)` may have
    // matching `.arguments.length` (1 vs 1) but the spread expands at
    // runtime to an unknown number of values, so structural equality
    // here would be unsound. Pairwise comparison alone happens to
    // catch most cases (a SpreadElement compares unequal to a non-
    // spread Expression because the kind check fails), but be
    // explicit so a future SpreadElement match doesn't silently
    // accept `f(...a)` ≡ `f(...a)` as if the runtime expansions
    // matched — the operand identity needed for nullish folding is
    // about value identity, which spread expansions cannot guarantee.
    if (
      ua.arguments.some(ts.isSpreadElement) ||
      ub.arguments.some(ts.isSpreadElement)
    ) {
      return false;
    }
    if (!structurallyEqualExpression(ua.expression, ub.expression)) {
      return false;
    }
    if (ua.arguments.length !== ub.arguments.length) {
      return false;
    }
    for (let i = 0; i < ua.arguments.length; i++) {
      if (
        !structurallyEqualExpression(
          ua.arguments[i] as ts.Expression,
          ub.arguments[i] as ts.Expression,
        )
      ) {
        return false;
      }
    }
    return true;
  }

  if (
    ua.kind === ts.SyntaxKind.ThisKeyword &&
    ub.kind === ts.SyntaxKind.ThisKeyword
  ) {
    return true;
  }

  if (ts.isStringLiteral(ua) && ts.isStringLiteral(ub)) {
    return ua.text === ub.text;
  }

  if (ts.isNumericLiteral(ua) && ts.isNumericLiteral(ub)) {
    return ua.text === ub.text;
  }

  return false;
}
