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
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import {
  type NullishTranslate,
  recognizeNullishForm,
} from "../src/nullish-recognizer.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

before(async () => {
  await loadAst();
});

/**
 * Parse `expr` as a single binary expression. Declares ambient
 * bindings for the names commonly used in tests (`x`, `y`, `a`, `b`,
 * `other`) so the type checker has a real type at each operand
 * location — the recognizer's nullability gate consults
 * `checker.getTypeAtLocation` and would reject an operand whose type
 * resolves to error/`any`.
 *
 * Pass an `overrides` map to override types for specific names — used
 * by the negative tests (e.g. `x: number` to assert non-nullable
 * operands fall through).
 */
function parseBinExpr(
  source: string,
  overrides: Record<string, string> = {},
): { expr: ts.BinaryExpression; checker: ts.TypeChecker } {
  const types: Record<string, string> = {
    x: "number | null | undefined",
    y: "number | null | undefined",
    a: "number | null | undefined",
    b: "number | null | undefined",
    other: "boolean",
    ...overrides,
  };
  const decls = Object.entries(types)
    .map(([name, ty]) => `declare const ${name}: ${ty};`)
    .join("\n");
  const wrapper = `${decls}\nconst _ = (${source});`;
  const sf = createSourceFileFromSource(wrapper);
  const checker = getChecker(sf);
  const stmts = sf.compilerNode.statements;
  const last = stmts[stmts.length - 1];
  if (!last || !ts.isVariableStatement(last)) {
    throw new Error("test helper: expected a trailing variable statement");
  }
  const decl = last.declarationList.declarations[0];
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
  return { expr, checker };
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

/**
 * Verify that `operand` is the result of translating a TS expression
 * with text `expectedText`. The synthetic translator builds
 * `ir1FromL2(irWrap(ast.var(e.getText())))`, so the operand should be
 * a `from-l2` wrapping a `var`-headed OpaqueExpr of that text.
 */
function expectOperandText(operand: IR1Expr, expectedText: string): void {
  assert.equal(operand.kind, "from-l2");
  if (operand.kind === "from-l2") {
    assert.equal(operand.expr.kind, "ir-wrap");
    if (operand.expr.kind === "ir-wrap") {
      const ast = getAst();
      assert.equal(ast.strExpr(operand.expr.expr), expectedText);
    }
  }
}

describe("ir1-build-nullish", () => {
  it("x == null builds is-nullish", () => {
    const { expr, checker } = parseBinExpr("x == null");
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    const operand = expectIsNullish(l1);
    expectOperandText(operand, "x");
  });

  it("x === null builds is-nullish", () => {
    const { expr, checker } = parseBinExpr("x === null");
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    const operand = expectIsNullish(l1);
    expectOperandText(operand, "x");
  });

  it("x === undefined builds is-nullish", () => {
    const { expr, checker } = parseBinExpr("x === undefined");
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    const operand = expectIsNullish(l1);
    expectOperandText(operand, "x");
  });

  it("x === null || x === undefined builds is-nullish (operand identity)", () => {
    const { expr, checker } = parseBinExpr(
      "x === null || x === undefined",
    );
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    // The recognizer folds the pair into one `is-nullish` (not nested
    // inside `or` — that's the point of the long-form recognizer).
    const operand = expectIsNullish(l1);
    expectOperandText(operand, "x");
  });

  it("typeof x === 'undefined' builds is-nullish", () => {
    const { expr, checker } = parseBinExpr("typeof x === 'undefined'");
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    const operand = expectIsNullish(l1);
    expectOperandText(operand, "x");
  });

  it("x != null builds not(is-nullish)", () => {
    const { expr, checker } = parseBinExpr("x != null");
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    const operand = expectNotIsNullish(l1);
    expectOperandText(operand, "x");
  });

  it("x !== null builds not(is-nullish)", () => {
    const { expr, checker } = parseBinExpr("x !== null");
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    const operand = expectNotIsNullish(l1);
    expectOperandText(operand, "x");
  });

  it("x !== null && x !== undefined builds not(is-nullish)", () => {
    const { expr, checker } = parseBinExpr(
      "x !== null && x !== undefined",
    );
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    const operand = expectNotIsNullish(l1);
    expectOperandText(operand, "x");
  });

  it("typeof x !== 'undefined' builds not(is-nullish)", () => {
    const { expr, checker } = parseBinExpr("typeof x !== 'undefined'");
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    const operand = expectNotIsNullish(l1);
    expectOperandText(operand, "x");
  });

  it("a === null || b === undefined falls through (operand mismatch)", () => {
    const { expr, checker } = parseBinExpr("a === null || b === undefined");
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    // No fold-eligible pair (different operands), so the long-form
    // recognizer returns null and the caller falls through to its
    // normal `||` handling. Each leaf would still be recognized
    // individually if the caller re-translates them.
    assert.equal(l1, null);
  });

  it("x === null || x === undefined || other extracts is-nullish prefix", () => {
    const { expr, checker } = parseBinExpr(
      "x === null || x === undefined || other",
    );
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    // Expect: or(is-nullish(x), other)
    assert.equal(l1.kind, "binop");
    if (l1.kind === "binop") {
      assert.equal(l1.op, "or");
      assert.equal(l1.lhs.kind, "is-nullish");
      if (l1.lhs.kind === "is-nullish") {
        // Verify the folded operand is the translated `x`, not e.g.
        // the `other` clause that incidentally has the right kind.
        expectOperandText(l1.lhs.operand, "x");
      }
    }
  });

  it("other || x === null || x === undefined extracts is-nullish suffix", () => {
    const { expr, checker } = parseBinExpr(
      "other || x === null || x === undefined",
    );
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    // Expect: or(other, is-nullish(x))
    assert.equal(l1.kind, "binop");
    if (l1.kind === "binop") {
      assert.equal(l1.op, "or");
      assert.equal(l1.rhs.kind, "is-nullish");
      if (l1.rhs.kind === "is-nullish") {
        expectOperandText(l1.rhs.operand, "x");
      }
    }
  });

  it("other && x !== null && x !== undefined extracts not-is-nullish", () => {
    const { expr, checker } = parseBinExpr(
      "other && x !== null && x !== undefined",
    );
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    // Expect: and(other, not(is-nullish(x)))
    assert.equal(l1.kind, "binop");
    if (l1.kind === "binop") {
      assert.equal(l1.op, "and");
      assert.equal(l1.rhs.kind, "unop");
      if (l1.rhs.kind === "unop") {
        assert.equal(l1.rhs.op, "not");
        assert.equal(l1.rhs.arg.kind, "is-nullish");
        if (l1.rhs.arg.kind === "is-nullish") {
          expectOperandText(l1.rhs.arg.operand, "x");
        }
      }
    }
  });

  it("non-nullish equality (x === 5) falls through", () => {
    const { expr, checker } = parseBinExpr("x === 5");
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });

  it("non-nullish disjunction (x > 0 || y < 0) falls through", () => {
    const { expr, checker } = parseBinExpr("x > 0 || y < 0");
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });

  it("loose-eq with non-null literal (x == 5) falls through", () => {
    const { expr, checker } = parseBinExpr("x == 5");
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });

  it("loose-eq with undefined (x == undefined) falls through (only null is recognized for ==)", () => {
    const { expr, checker } = parseBinExpr("x == undefined");
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });

  it("non-nullable operand (n: number) falls through (nullability gate)", () => {
    // Non-nullable `n` would lower to a scalar, not `[T]`, so `#n = 0`
    // would be ill-typed. The recognizer must refuse to fold here.
    const { expr, checker } = parseBinExpr("n === null", {
      n: "number",
    });
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });

  it("shadowed `undefined` falls through (shadowing gate)", () => {
    // A locally bound `undefined: number` doesn't resolve to the
    // global undefined value. The recognizer's symbol-resolution
    // gate keeps `x === undefined` from matching here.
    const { expr, checker } = parseBinExpr("x === undefined", {
      undefined: "number",
    });
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });
});
