/**
 * Unit tests for `structurallyEqualExpression` (M4 helper).
 *
 * Verifies that equality is structural — same kind, same identifier
 * texts, same recursive children — and that surface-text differences
 * (whitespace, comments, quote style, parenthesization) don't change
 * the verdict. The latter is exactly what `getText`-based comparison
 * would get wrong.
 */

import assert from "node:assert/strict";
import { describe, it } from "node:test";
import ts from "typescript";
import { structurallyEqualExpression } from "../src/ast-equal.js";
import { createSourceFileFromSource } from "../src/extract.js";

/**
 * Parse a TS expression. Wraps the source in `const _ = (<expr>);`
 * so we can pull a single typed Expression node back out without
 * fighting the statement-level parser.
 */
function parseExpr(source: string): ts.Expression {
  const sf = createSourceFileFromSource(`const _ = (${source});`);
  const stmt = sf.compilerNode.statements[0];
  if (!stmt || !ts.isVariableStatement(stmt)) {
    throw new Error("test helper: expected a variable statement");
  }
  const decl = stmt.declarationList.declarations[0];
  if (!decl || !decl.initializer) {
    throw new Error("test helper: expected an initializer");
  }
  // Unwrap the outer parens we added.
  let expr: ts.Expression = decl.initializer;
  while (ts.isParenthesizedExpression(expr)) {
    expr = expr.expression;
  }
  return expr;
}

describe("ast-equal", () => {
  it("identifiers compare structurally", () => {
    assert.equal(structurallyEqualExpression(parseExpr("a"), parseExpr("a")), true);
    assert.equal(
      structurallyEqualExpression(parseExpr("a"), parseExpr("b")),
      false,
    );
  });

  it("dotted paths compare structurally", () => {
    assert.equal(
      structurallyEqualExpression(parseExpr("a.b"), parseExpr("a.b")),
      true,
    );
    assert.equal(
      structurallyEqualExpression(parseExpr("a.b"), parseExpr("a.c")),
      false,
    );
    assert.equal(
      structurallyEqualExpression(parseExpr("a.b.c"), parseExpr("a.b.c")),
      true,
    );
    assert.equal(
      structurallyEqualExpression(parseExpr("a.b.c"), parseExpr("a.x.c")),
      false,
    );
  });

  it("indexed access compares structurally", () => {
    assert.equal(
      structurallyEqualExpression(parseExpr('a["k"]'), parseExpr('a["k"]')),
      true,
    );
    assert.equal(
      structurallyEqualExpression(parseExpr('a["k"]'), parseExpr('a["j"]')),
      false,
    );
    // Mixed dotted-and-indexed paths
    assert.equal(
      structurallyEqualExpression(parseExpr('a.b["k"]'), parseExpr('a.b["k"]')),
      true,
    );
  });

  it("spread arguments cause calls to compare unequal (conservative)", () => {
    // f(...arr) vs f(...arr) — same source text but spread expansion
    // is runtime-determined; we cannot assert operand identity.
    assert.equal(
      structurallyEqualExpression(parseExpr("f(...a)"), parseExpr("f(...a)")),
      false,
    );
    // f(...arr) vs f(a, b) — different argument shapes.
    assert.equal(
      structurallyEqualExpression(parseExpr("f(...a)"), parseExpr("f(a, b)")),
      false,
    );
  });

  it("calls compare structurally including arguments", () => {
    assert.equal(
      structurallyEqualExpression(parseExpr("f(x)"), parseExpr("f(x)")),
      true,
    );
    assert.equal(
      structurallyEqualExpression(parseExpr("f(x)"), parseExpr("f(y)")),
      false,
    );
    assert.equal(
      structurallyEqualExpression(parseExpr("f(x, y)"), parseExpr("f(x, y)")),
      true,
    );
    assert.equal(
      structurallyEqualExpression(parseExpr("f(x, y)"), parseExpr("f(x, z)")),
      false,
    );
    // Arity differs
    assert.equal(
      structurallyEqualExpression(parseExpr("f(x)"), parseExpr("f(x, y)")),
      false,
    );
    // Different callee
    assert.equal(
      structurallyEqualExpression(parseExpr("f(x)"), parseExpr("g(x)")),
      false,
    );
  });

  it("transparent wrappers (as / !  / satisfies) are unwrapped", () => {
    // Match `unwrapExpression` in translate-body.ts — these wrappers
    // don't change runtime value, so two operands wrapped differently
    // should still match for the long-form nullish recognizer to
    // fold them.
    assert.equal(
      structurallyEqualExpression(parseExpr("(x as number)"), parseExpr("x")),
      true,
    );
    assert.equal(
      structurallyEqualExpression(parseExpr("x!"), parseExpr("x")),
      true,
    );
    assert.equal(
      structurallyEqualExpression(
        parseExpr("x satisfies number"),
        parseExpr("x"),
      ),
      true,
    );
    // Composed wrappers (paren around as, etc.).
    assert.equal(
      structurallyEqualExpression(
        parseExpr("((x as number)!)"),
        parseExpr("x"),
      ),
      true,
    );
  });

  it("parenthesized wrappers are unwrapped", () => {
    assert.equal(
      structurallyEqualExpression(parseExpr("(a.b)"), parseExpr("a.b")),
      true,
    );
    assert.equal(
      structurallyEqualExpression(
        parseExpr("((a.b))"),
        parseExpr("a.b"),
      ),
      true,
    );
    assert.equal(
      structurallyEqualExpression(
        parseExpr("(f(x))"),
        parseExpr("f((x))"),
      ),
      true,
    );
  });

  it("comments inside nodes are ignored", () => {
    // Comments live as trivia, not as AST children — structural equality
    // walks AST children only, so a comment between nodes is naturally
    // invisible. `getText`-based comparison would see them.
    assert.equal(
      structurallyEqualExpression(
        parseExpr("a /* note */.b"),
        parseExpr("a.b"),
      ),
      true,
    );
    assert.equal(
      structurallyEqualExpression(
        parseExpr("f(/*1*/ x /*2*/)"),
        parseExpr("f(x)"),
      ),
      true,
    );
  });

  it("quote-style differences ignored on string element-access keys", () => {
    // `obj['k']` and `obj["k"]` parse to identical ElementAccess+StringLiteral
    // structure (the literal's `.text` strips the surrounding quotes).
    assert.equal(
      structurallyEqualExpression(parseExpr("a['k']"), parseExpr('a["k"]')),
      true,
    );
  });

  it("different operand shapes return false", () => {
    // Identifier vs PropertyAccess
    assert.equal(
      structurallyEqualExpression(parseExpr("a"), parseExpr("a.b")),
      false,
    );
    // Call vs PropertyAccess
    assert.equal(
      structurallyEqualExpression(parseExpr("f(x)"), parseExpr("f.x")),
      false,
    );
    // ElementAccess vs PropertyAccess (even with same effective key)
    assert.equal(
      structurallyEqualExpression(parseExpr('a["b"]'), parseExpr("a.b")),
      false,
    );
  });

  it("optional-chain access is distinct from plain access", () => {
    // `a?.b` vs `a.b` must compare unequal so the nullish recognizer's
    // operand-identity dedup doesn't fold a pair like
    // `a?.b === null || a.b === undefined` into one `IsNullish`.
    assert.equal(
      structurallyEqualExpression(parseExpr("a?.b"), parseExpr("a.b")),
      false,
    );
    assert.equal(
      structurallyEqualExpression(parseExpr("a.b"), parseExpr("a?.b")),
      false,
    );
    // Tail members of a chain (`a?.b.c`'s outer `.c`) carry
    // NodeFlags.OptionalChain even without a `?.` token; that flag must
    // also distinguish them from plain `.c` accesses.
    assert.equal(
      structurallyEqualExpression(parseExpr("a?.b.c"), parseExpr("a.b.c")),
      false,
    );
    // Same-shape optional chains compare equal.
    assert.equal(
      structurallyEqualExpression(parseExpr("a?.b"), parseExpr("a?.b")),
      true,
    );
  });

  it("`this` always equal to `this`", () => {
    assert.equal(
      structurallyEqualExpression(parseExpr("this"), parseExpr("this")),
      true,
    );
    assert.equal(
      structurallyEqualExpression(parseExpr("this.x"), parseExpr("this.x")),
      true,
    );
  });

  it("unsupported node kinds (e.g., binary expressions) return false", () => {
    // `a + b` is not in the supported shape vocabulary; conservative-refusal
    // returns false rather than recursively comparing.
    assert.equal(
      structurallyEqualExpression(parseExpr("a + b"), parseExpr("a + b")),
      false,
    );
  });
});
