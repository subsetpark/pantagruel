/**
 * Unit tests for the M4 nullish recognizer (`recognizeNullishForm`).
 *
 * Each test parses a TS expression and asserts the recognizer
 * produces the expected L1 shape:
 *
 * - `IsNullish(operand)` for positive forms
 * - `Unop(not, IsNullish(operand))` for negated forms
 * - `BinOp(or | and, IsNullish(...), other)` when a long-form chain
 *   has residue
 * - `null` when no nullish form is recognized (fall-through)
 *
 * The recognizer is invoked through a synthetic translation callback
 * that builds opaque `var` references so we can compare structure
 * without translating actual TS bindings.
 */

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import { irWrap } from "../src/ir.js";
import { type IR1Expr, ir1FromL2 } from "../src/ir1.js";
import { createSourceFileFromSource } from "../src/extract.js";
import {
  type NullishTranslate,
  recognizeNullishForm,
} from "../src/nullish-recognizer.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

before(async () => {
  await loadAst();
});

/**
 * Parse `expr` as a single binary expression. Wraps in `const _ = (<expr>);`
 * to give the parser a statement context, then unwraps the outer parens.
 */
function parseBinExpr(source: string): ts.BinaryExpression {
  const sf = createSourceFileFromSource(`const _ = (${source});`);
  const stmt = sf.compilerNode.statements[0];
  if (!stmt || !ts.isVariableStatement(stmt)) {
    throw new Error("test helper: expected a variable statement");
  }
  const decl = stmt.declarationList.declarations[0];
  if (!decl || !decl.initializer) {
    throw new Error("test helper: expected an initializer");
  }
  let expr: ts.Expression = decl.initializer;
  while (ts.isParenthesizedExpression(expr)) {
    expr = expr.expression;
  }
  if (!ts.isBinaryExpression(expr)) {
    throw new Error(
      `test helper: expected a binary expression, got ${ts.SyntaxKind[expr.kind]}`,
    );
  }
  return expr;
}

/**
 * Synthetic translator: turns each TS expression into a `from-l2` that
 * wraps an opaque `var` named after the expression's source text. This
 * makes the structural assertions readable without a real binding
 * environment.
 */
function makeTranslate(): NullishTranslate {
  const ast = getAst();
  return (e) => ir1FromL2(irWrap(ast.var(e.getText())));
}

function asL1(
  result: IR1Expr | { unsupported: string } | null,
): IR1Expr {
  if (result === null) {
    throw new Error("expected an L1 expression, got null (no match)");
  }
  if ("unsupported" in result) {
    throw new Error(`expected an L1 expression, got unsupported: ${result.unsupported}`);
  }
  return result;
}

function expectIsNullish(l1: IR1Expr): IR1Expr {
  if (l1.kind !== "is-nullish") {
    throw new Error(
      `expected is-nullish at top level, got ${l1.kind}`,
    );
  }
  return l1.operand;
}

function expectNotIsNullish(l1: IR1Expr): IR1Expr {
  if (l1.kind !== "unop" || l1.op !== "not") {
    throw new Error(`expected unop(not, …) at top level, got ${l1.kind}`);
  }
  return expectIsNullish(l1.arg);
}

describe("ir1-build-nullish", () => {
  it("x == null builds is-nullish", () => {
    const expr = parseBinExpr("x == null");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    expectIsNullish(l1);
  });

  it("x === null builds is-nullish", () => {
    const expr = parseBinExpr("x === null");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    expectIsNullish(l1);
  });

  it("x === undefined builds is-nullish", () => {
    const expr = parseBinExpr("x === undefined");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    expectIsNullish(l1);
  });

  it("x === null || x === undefined builds is-nullish (operand identity)", () => {
    const expr = parseBinExpr("x === null || x === undefined");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    // The recognizer folds the pair into one `is-nullish` (not nested
    // inside `or` — that's the point of the long-form recognizer).
    expectIsNullish(l1);
  });

  it("typeof x === 'undefined' builds is-nullish", () => {
    const expr = parseBinExpr("typeof x === 'undefined'");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    expectIsNullish(l1);
  });

  it("x != null builds not(is-nullish)", () => {
    const expr = parseBinExpr("x != null");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    expectNotIsNullish(l1);
  });

  it("x !== null builds not(is-nullish)", () => {
    const expr = parseBinExpr("x !== null");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    expectNotIsNullish(l1);
  });

  it("x !== null && x !== undefined builds not(is-nullish)", () => {
    const expr = parseBinExpr("x !== null && x !== undefined");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    expectNotIsNullish(l1);
  });

  it("typeof x !== 'undefined' builds not(is-nullish)", () => {
    const expr = parseBinExpr("typeof x !== 'undefined'");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    expectNotIsNullish(l1);
  });

  it("a === null || b === undefined falls through (operand mismatch)", () => {
    const expr = parseBinExpr("a === null || b === undefined");
    const l1 = recognizeNullishForm(expr, makeTranslate());
    // No fold-eligible pair (different operands), so the long-form
    // recognizer returns null and the caller falls through to its
    // normal `||` handling. Each leaf would still be recognized
    // individually if the caller re-translates them.
    assert.equal(l1, null);
  });

  it("x === null || x === undefined || other extracts is-nullish prefix", () => {
    const expr = parseBinExpr("x === null || x === undefined || other");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    // Expect: or(is-nullish(x), other)
    assert.equal(l1.kind, "binop");
    if (l1.kind === "binop") {
      assert.equal(l1.op, "or");
      assert.equal(l1.lhs.kind, "is-nullish");
    }
  });

  it("other || x === null || x === undefined extracts is-nullish suffix", () => {
    const expr = parseBinExpr("other || x === null || x === undefined");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    // Expect: or(other, is-nullish(x))
    assert.equal(l1.kind, "binop");
    if (l1.kind === "binop") {
      assert.equal(l1.op, "or");
      assert.equal(l1.rhs.kind, "is-nullish");
    }
  });

  it("other && x !== null && x !== undefined extracts not-is-nullish", () => {
    const expr = parseBinExpr("other && x !== null && x !== undefined");
    const l1 = asL1(recognizeNullishForm(expr, makeTranslate()));
    // Expect: and(other, not(is-nullish(x)))
    assert.equal(l1.kind, "binop");
    if (l1.kind === "binop") {
      assert.equal(l1.op, "and");
      assert.equal(l1.rhs.kind, "unop");
      if (l1.rhs.kind === "unop") {
        assert.equal(l1.rhs.op, "not");
        assert.equal(l1.rhs.arg.kind, "is-nullish");
      }
    }
  });

  it("non-nullish equality (x === 5) falls through", () => {
    const expr = parseBinExpr("x === 5");
    const l1 = recognizeNullishForm(expr, makeTranslate());
    assert.equal(l1, null);
  });

  it("non-nullish disjunction (x > 0 || y < 0) falls through", () => {
    const expr = parseBinExpr("x > 0 || y < 0");
    const l1 = recognizeNullishForm(expr, makeTranslate());
    assert.equal(l1, null);
  });

  it("loose-eq with non-null literal (x == 5) falls through", () => {
    const expr = parseBinExpr("x == 5");
    const l1 = recognizeNullishForm(expr, makeTranslate());
    assert.equal(l1, null);
  });

  it("loose-eq with undefined (x == undefined) falls through (only null is recognized for ==)", () => {
    const expr = parseBinExpr("x == undefined");
    const l1 = recognizeNullishForm(expr, makeTranslate());
    assert.equal(l1, null);
  });
});
