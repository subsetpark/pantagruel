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
 * - `ParenthesizedExpression` — unwrap and recurse
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

function unwrapParens(e: ts.Expression): ts.Expression {
  let cur = e;
  while (ts.isParenthesizedExpression(cur)) {
    cur = cur.expression;
  }
  return cur;
}

export function structurallyEqualExpression(
  a: ts.Expression,
  b: ts.Expression,
): boolean {
  const ua = unwrapParens(a);
  const ub = unwrapParens(b);

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
