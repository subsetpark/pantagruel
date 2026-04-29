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
  isL1Unsupported,
  tryBuildL1PureSubExpression,
  type L1BuildContext,
} from "../src/ir1-build.js";
import {
  type NullishTranslate,
  recognizeNullishForm,
} from "../src/nullish-recognizer.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import type { UniqueSupply } from "../src/translate-body.js";
import { IntStrategy, newSynthCell } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

/**
 * Parse `expr` as a single binary expression. Declares ambient
 * bindings for the names commonly used in tests (`x`, `y`, `a`, `b`,
 * `other`) so the type checker has a real type at each operand
 * location — the recognizer's nullability gate uses the *declared*
 * type from the symbol's declaration (via
 * `getOperandDeclaredType` / `checker.getTypeOfSymbolAtLocation`),
 * not the flow-narrowed type at the use site, so leaves later in a
 * long-form chain still see the parameter's full nullable union.
 * Without ambient bindings the operand's symbol resolution would
 * fall back to error/`any` and reject the fold.
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

function parseNarrowedNullishReturn(source: string): {
  expr: ts.Expression;
  ctx: L1BuildContext;
} {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements.find(
    (stmt): stmt is ts.FunctionDeclaration =>
      ts.isFunctionDeclaration(stmt) && stmt.body !== undefined,
  );
  if (!fn) {
    throw new Error("test helper: expected function body");
  }
  const first = fn.body.statements[0];
  if (!first || !ts.isIfStatement(first)) {
    throw new Error("test helper: expected leading if statement");
  }
  const thenStmt = ts.isBlock(first.thenStatement)
    ? first.thenStatement.statements[0]
    : first.thenStatement;
  if (!thenStmt || !ts.isReturnStatement(thenStmt) || !thenStmt.expression) {
    throw new Error("test helper: expected return inside if");
  }
  const paramNames = new Map<string, string>();
  for (const param of fn.parameters) {
    if (ts.isIdentifier(param.name)) {
      paramNames.set(param.name.text, param.name.text);
    }
  }
  const supply: UniqueSupply = { n: 0, synthCell: newSynthCell() };
  return {
    expr: thenStmt.expression,
    ctx: {
      checker,
      strategy: IntStrategy,
      paramNames,
      state: undefined,
      supply,
    },
  };
}

function asL1(result: IR1Expr | { unsupported: string } | null): IR1Expr {
  if (result === null) {
    throw new Error("expected an L1 expression, got null (no match)");
  }
  if ("unsupported" in result) {
    throw new Error(
      `expected an L1 expression, got unsupported: ${result.unsupported}`,
    );
  }
  return result;
}

function expectIsNullish(l1: IR1Expr): IR1Expr {
  if (l1.kind !== "is-nullish") {
    throw new Error(`expected is-nullish at top level, got ${l1.kind}`);
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
    const { expr, checker } = parseBinExpr("x === null || x === undefined");
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    // The recognizer folds the pair into one `is-nullish` (not nested
    // inside `or` — that's the point of the long-form recognizer).
    const operand = expectIsNullish(l1);
    expectOperandText(operand, "x");
  });

  it("x: number | null still folds long-form nullish pair (declared-type gate)", () => {
    // Regression for the declared-type vs location-type distinction:
    // TS narrows `x` to non-nullable in later clauses of a long-form
    // chain after prior negative comparisons. Without using
    // `getOperandDeclaredType`, the third leaf would see `x: never`
    // and the gate would reject the fold. Even when the declared
    // type only includes `null` (no `undefined`), the recognizer
    // should still fold the pair — comparing a `number | null` value
    // against `undefined` is an always-false comparison in TS, but
    // syntactically it's still part of the nullish pattern, and
    // collapsing to `IsNullish(x)` lowers to a sound `#x = 0` test.
    const { expr, checker } = parseBinExpr("x === null || x === undefined", {
      x: "number | null",
    });
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
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
    const { expr, checker } = parseBinExpr("x !== null && x !== undefined");
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

  it("non-Bool leftover clause falls through (Bool-typed gate)", () => {
    // Mixed-type long form: `x === null || x === undefined || n` where
    // `n: number` is not Bool. Folding would rebuild as
    // `IsNullish(x) or n`, mixing a Bool with a number — changes JS's
    // truthy short-circuit semantics. Refuse to fold.
    const { expr, checker } = parseBinExpr(
      "x === null || x === undefined || n",
      { n: "number" },
    );
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });

  it("Bool leftover clause still folds (Bool-typed gate)", () => {
    // Sanity: the Bool gate doesn't reject the legitimate residue
    // case where `other: boolean` — same shape as the existing
    // prefix-extraction test, but tests the gate path explicitly.
    const { expr, checker } = parseBinExpr(
      "x === null || x === undefined || other",
    );
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    assert.equal(l1.kind, "binop");
    if (l1.kind === "binop") {
      assert.equal(l1.lhs.kind, "is-nullish");
    }
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

  // ─── Sentinel-vs-sentinel rejection ─────────────────────────────────

  it("`null === undefined` falls through (sentinel-vs-sentinel)", () => {
    // The "operand" would itself be a nullish sentinel — folding to
    // `IsNullish(undefined)` lowers to `#undefined = 0`, ill-typed.
    const { expr, checker } = parseBinExpr("null === undefined");
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });

  it("`undefined !== null` falls through (sentinel-vs-sentinel, negated)", () => {
    const { expr, checker } = parseBinExpr("undefined !== null");
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });

  it("`null === null` falls through (same-kind sentinel)", () => {
    // Always-true; folding to `IsNullish(null)` is ill-typed.
    const { expr, checker } = parseBinExpr("null === null");
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });

  it("`undefined === undefined` falls through (same-kind sentinel)", () => {
    const { expr, checker } = parseBinExpr("undefined === undefined");
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });

  it("`typeof null === 'undefined'` falls through (sentinel inside typeof)", () => {
    // `typeof null` is "object", so this is always false. Folding to
    // `IsNullish(null)` is wrong both semantically and as Pant shape.
    const { expr, checker } = parseBinExpr("typeof null === 'undefined'");
    const l1 = recognizeNullishForm(expr, checker, makeTranslate());
    assert.equal(l1, null);
  });

  // ─── Duplicate-fold dedup ───────────────────────────────────────────

  it("duplicate-operand long form collapses to a single is-nullish", () => {
    // `(P or P) ≡ P`: chains with multiple fold pairs sharing the
    // same operand should produce ONE IsNullish, not two joined by
    // `or`. Also avoids translating the same operand twice through
    // the shared UniqueSupply (which could diverge fresh-binder
    // counters for chain-typed operands).
    const { expr, checker } = parseBinExpr(
      "x === null || x === undefined || x === null || x === undefined",
    );
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    // Top-level should be a single `is-nullish`, not `binop(or, …)`.
    const operand = expectIsNullish(l1);
    expectOperandText(operand, "x");
  });

  it("two distinct duplicate folds keep both is-nullish (different operands)", () => {
    // Sanity: dedup is by operand identity, not by fold count.
    // `x..., x..., y..., y...` should produce
    // `IsNullish(x) or IsNullish(y)`, not collapse to one.
    const { expr, checker } = parseBinExpr(
      "x === null || x === undefined || y === null || y === undefined",
    );
    const l1 = asL1(recognizeNullishForm(expr, checker, makeTranslate()));
    assert.equal(l1.kind, "binop");
    if (l1.kind === "binop") {
      assert.equal(l1.op, "or");
      assert.equal(l1.lhs.kind, "is-nullish");
      assert.equal(l1.rhs.kind, "is-nullish");
      if (l1.lhs.kind === "is-nullish") {
        expectOperandText(l1.lhs.operand, "x");
      }
      if (l1.rhs.kind === "is-nullish") {
        expectOperandText(l1.rhs.operand, "y");
      }
    }
  });
});

describe("ir1-build nullish coalescing", () => {
  it("uses declared left nullability when the left operand is flow-narrowed", () => {
    const { expr, ctx } = parseNarrowedNullishReturn(`
      function f(x: number | null, y: number): number {
        if (x !== null) {
          return x ?? y;
        }
        return y;
      }
    `);
    const l1 = tryBuildL1PureSubExpression(expr, ctx);
    assert.notEqual(l1, null);
    if (l1 === null || isL1Unsupported(l1)) {
      throw new Error("expected nullish coalescing to build");
    }
    assert.equal(l1.kind, "cond");
    if (l1.kind === "cond") {
      assert.equal(l1.otherwise.kind, "app");
    }
  });

  it("uses declared right nullability when the right operand is flow-narrowed", () => {
    const { expr, ctx } = parseNarrowedNullishReturn(`
      function f(x: number | null, y: number | null): number | null {
        if (y !== null) {
          return x ?? y;
        }
        return x;
      }
    `);
    const l1 = tryBuildL1PureSubExpression(expr, ctx);
    assert.notEqual(l1, null);
    if (l1 === null || isL1Unsupported(l1)) {
      throw new Error("expected nullish coalescing to build");
    }
    assert.equal(l1.kind, "cond");
    if (l1.kind === "cond") {
      assert.equal(l1.otherwise.kind, "var");
    }
  });
});
