/**
 * Unit tests for `buildL1IncrementStep` (workstream M2).
 *
 * Exercises the increment surface-form normalizer in isolation: takes a
 * TS expression-statement body, recognizes one of the increment shapes
 * on the named counter, and produces canonical L1 `Assign(Var(c),
 * BinOp(<op>, Var(c), <k>))`.
 *
 * The five `+1` spellings (`i++`, `++i`, `i += 1`, `i = i + 1`,
 * `i = 1 + i`) MUST produce identical L1 output. Other increment shapes
 * (compound, non-commutative, unary `--`) build to non-canonical Assign
 * variants; the L1 μ-search recognizer at lowering time rejects those.
 */

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import {
  buildL1IncrementStep,
  isL1StmtUnsupported,
} from "../src/ir1-build.js";
import {
  ir1Assign,
  ir1Binop,
  ir1LitNat,
  ir1Var,
} from "../src/ir1.js";
import { loadAst } from "../src/pant-wasm.js";
import {
  type UniqueSupply,
  freshHygienicBinder,
} from "../src/translate-body.js";
import { IntStrategy } from "../src/translate-types.js";
import ts from "typescript";

before(async () => {
  await loadAst();
});

/**
 * Build a minimal `L1BuildContext` and pull the body of `function f(i:
 * number) { <step>; }` so we can run `buildL1IncrementStep` directly on
 * the underlying expression. Returns the increment expression and a
 * context suitable for building it.
 */
function setup(stepSource: string) {
  const source = `function f(i: number) { ${stepSource}; }`;
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements[0]!;
  if (!ts.isFunctionDeclaration(fn) || !fn.body) {
    throw new Error("test helper: expected a function declaration");
  }
  const stmt = fn.body.statements[0]!;
  if (!ts.isExpressionStatement(stmt)) {
    throw new Error("test helper: expected an expression-statement body");
  }
  const supply: UniqueSupply = { n: 0 };
  // freshHygienicBinder is a no-op here; we just want a valid supply.
  void freshHygienicBinder;
  const ctx = {
    checker,
    strategy: IntStrategy,
    paramNames: new Map<string, string>([["i", "i"]]),
    state: undefined,
    supply,
  };
  return { expr: stmt.expression, ctx };
}

const CANONICAL_PLUS_ONE = ir1Assign(
  ir1Var("i"),
  ir1Binop("add", ir1Var("i"), ir1LitNat(1)),
);

// ---------------------------------------------------------------------------
// The five canonical `+1` spellings — all produce identical Assign
// ---------------------------------------------------------------------------

describe("buildL1IncrementStep: +1 spellings collapse to one canonical Assign", () => {
  it("`i++` → Assign(i, i + 1)", () => {
    const { expr, ctx } = setup("i++");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (isL1StmtUnsupported(result)) {
      assert.fail(`expected L1 statement, got: ${result.unsupported}`);
    }
    assert.deepEqual(result, CANONICAL_PLUS_ONE);
  });

  it("`++i` → Assign(i, i + 1)", () => {
    const { expr, ctx } = setup("++i");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (isL1StmtUnsupported(result)) {
      assert.fail(`expected L1 statement, got: ${result.unsupported}`);
    }
    assert.deepEqual(result, CANONICAL_PLUS_ONE);
  });

  it("`i += 1` → Assign(i, i + 1)", () => {
    const { expr, ctx } = setup("i += 1");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (isL1StmtUnsupported(result)) {
      assert.fail(`expected L1 statement, got: ${result.unsupported}`);
    }
    assert.deepEqual(result, CANONICAL_PLUS_ONE);
  });

  it("`i = i + 1` → Assign(i, i + 1)", () => {
    const { expr, ctx } = setup("i = i + 1");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (isL1StmtUnsupported(result)) {
      assert.fail(`expected L1 statement, got: ${result.unsupported}`);
    }
    assert.deepEqual(result, CANONICAL_PLUS_ONE);
  });

  it("`i = 1 + i` → Assign(i, i + 1)  (commutative-rewrite)", () => {
    const { expr, ctx } = setup("i = 1 + i");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (isL1StmtUnsupported(result)) {
      assert.fail(`expected L1 statement, got: ${result.unsupported}`);
    }
    assert.deepEqual(result, CANONICAL_PLUS_ONE);
  });
});

// ---------------------------------------------------------------------------
// Non-`+1` increments build successfully (L1 recognizer rejects later)
// ---------------------------------------------------------------------------

describe("buildL1IncrementStep: non-+1 increments build to L1", () => {
  it("`i--` → Assign(i, i - 1)", () => {
    const { expr, ctx } = setup("i--");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (isL1StmtUnsupported(result)) {
      assert.fail(`expected L1 statement, got: ${result.unsupported}`);
    }
    assert.deepEqual(
      result,
      ir1Assign(ir1Var("i"), ir1Binop("sub", ir1Var("i"), ir1LitNat(1))),
    );
  });

  it("`i += 2` → Assign(i, i + 2)", () => {
    const { expr, ctx } = setup("i += 2");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (isL1StmtUnsupported(result)) {
      assert.fail(`expected L1 statement, got: ${result.unsupported}`);
    }
    if (!isL1StmtUnsupported(result) && result.kind === "assign") {
      assert.equal(result.value.kind, "binop");
    }
  });

  it("`i = i * 2` → Assign(i, i * 2)", () => {
    const { expr, ctx } = setup("i = i * 2");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (isL1StmtUnsupported(result)) {
      assert.fail(`expected L1 statement, got: ${result.unsupported}`);
    }
    if (!isL1StmtUnsupported(result) && result.kind === "assign") {
      assert.equal(result.value.kind, "binop");
      if (result.value.kind === "binop") {
        assert.equal(result.value.op, "mul");
      }
    }
  });
});

// ---------------------------------------------------------------------------
// Rejection cases
// ---------------------------------------------------------------------------

describe("buildL1IncrementStep: rejection cases", () => {
  it("rejects assignment to a non-counter variable", () => {
    const { expr, ctx } = setup("j = j + 1");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (!isL1StmtUnsupported(result)) {
      assert.fail("expected unsupported, got an L1 statement");
    }
    assert.match(result.unsupported, /not the counter/);
  });

  it("rejects compound-assignment to a non-counter variable", () => {
    const { expr, ctx } = setup("j += 1");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (!isL1StmtUnsupported(result)) {
      assert.fail("expected unsupported, got an L1 statement");
    }
  });

  it("rejects unary increment on a non-counter variable", () => {
    const { expr, ctx } = setup("j++");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (!isL1StmtUnsupported(result)) {
      assert.fail("expected unsupported, got an L1 statement");
    }
  });

  it("rejects RHS that does not reference counter (`i = 5`)", () => {
    const { expr, ctx } = setup("i = 5");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (!isL1StmtUnsupported(result)) {
      assert.fail("expected unsupported, got an L1 statement");
    }
    assert.match(result.unsupported, /not a binary expression/);
  });

  it("rejects `i = k - i` (counter on right of non-commutative op)", () => {
    const { expr, ctx } = setup("i = 5 - i");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (!isL1StmtUnsupported(result)) {
      assert.fail("expected unsupported, got an L1 statement");
    }
    assert.match(result.unsupported, /non-commutative/);
  });

  it("rejects RHS without counter (`i = j + 1`)", () => {
    const { expr, ctx } = setup("i = j + 1");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (!isL1StmtUnsupported(result)) {
      assert.fail("expected unsupported, got an L1 statement");
    }
    assert.match(result.unsupported, /reference counter/);
  });

  it("rejects bare expression that's not an assignment or increment", () => {
    const { expr, ctx } = setup("i + 1");
    const result = buildL1IncrementStep(expr, "i", ctx);
    if (!isL1StmtUnsupported(result)) {
      assert.fail("expected unsupported, got an L1 statement");
    }
  });
});
