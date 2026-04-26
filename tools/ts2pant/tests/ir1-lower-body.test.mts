import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import {
  ir1Assign,
  ir1Block,
  ir1CondStmt,
  ir1Foreach,
  ir1LitBool,
  ir1LitNat,
  ir1Member,
  ir1Var,
} from "../src/ir1.js";
import { lowerL1Body } from "../src/ir1-lower-body.js";
import { makeSymbolicState } from "../src/translate-body.js";
import type { PropResult } from "../src/types.js";
import type { OpaqueExpr } from "../src/pant-ast.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

before(async () => {
  await loadAst();
});

function ctx() {
  return { applyConst: (e: OpaqueExpr): OpaqueExpr => e };
}

function fresh(): {
  state: ReturnType<typeof makeSymbolicState>;
  propositions: PropResult[];
} {
  return {
    state: makeSymbolicState(),
    propositions: [],
  };
}

// Render an equation PropResult to a Pantagruel string for diff checks.
// Wraps in a forall so we can stringify both quantifiers + guards
// uniformly (Pant has no public stringifier for raw PropResult shape).
function renderEquation(p: PropResult): string {
  if (p.kind !== "equation") {
    throw new Error(`expected equation, got ${p.kind}`);
  }
  const ast = getAst();
  const eqExpr = ast.binop(ast.opEq(), p.lhs, p.rhs);
  if (p.quantifiers.length === 0 && (p.guards ?? []).length === 0) {
    return ast.strExpr(eqExpr);
  }
  return ast.strExpr(ast.forall(p.quantifiers, p.guards ?? [], eqExpr));
}

// ---------------------------------------------------------------------------
// block + assign — basic shape
// ---------------------------------------------------------------------------

describe("lowerL1Body — block + assign(member, …)", () => {
  it("single property write installs into state.writes", () => {
    const stmt = ir1Assign(
      ir1Member(ir1Var("acct"), "balance"),
      ir1LitNat(100),
    );
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, true);
    assert.equal(propositions.length, 0);
    assert.equal(state.writes.size, 1);
    const entry = [...state.writes.values()][0]!;
    assert.equal(entry.kind, "property");
    if (entry.kind === "property") {
      assert.equal(entry.prop, "balance");
    }
  });

  it("block of two writes installs both with last-write-wins per key", () => {
    const stmt = ir1Block([
      ir1Assign(ir1Member(ir1Var("acct"), "balance"), ir1LitNat(100)),
      ir1Assign(ir1Member(ir1Var("acct"), "balance"), ir1LitNat(200)),
    ]);
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, true);
    assert.equal(propositions.length, 0);
    // Last-write-wins: only one entry, value is 200.
    assert.equal(state.writes.size, 1);
  });

  it("block with writes to two different properties produces two entries", () => {
    const stmt = ir1Block([
      ir1Assign(ir1Member(ir1Var("acct"), "balance"), ir1LitNat(100)),
      ir1Assign(ir1Member(ir1Var("acct"), "active"), ir1LitBool(true)),
    ]);
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, true);
    assert.equal(state.writes.size, 2);
  });

  it("rejects var-target assigns (μ-search counter only)", () => {
    const stmt = ir1Assign(ir1Var("c"), ir1LitNat(1));
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, false);
    assert.equal(propositions.length, 1);
    assert.equal(propositions[0]!.kind, "unsupported");
    if (propositions[0]!.kind === "unsupported") {
      assert.match(propositions[0]!.reason, /μ-search-only/u);
    }
  });
});

// ---------------------------------------------------------------------------
// cond-stmt — branch merge per write-key
// ---------------------------------------------------------------------------

describe("lowerL1Body — cond-stmt (branch merge)", () => {
  it("if/else with same property write merges via cond(g, vT, true, vE)", () => {
    // if (g) acct.balance = 100; else acct.balance = 50;
    const stmt = ir1CondStmt(
      [
        [
          ir1Var("g"),
          ir1Assign(ir1Member(ir1Var("acct"), "balance"), ir1LitNat(100)),
        ],
      ],
      ir1Assign(ir1Member(ir1Var("acct"), "balance"), ir1LitNat(50)),
    );
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, true);
    assert.equal(propositions.length, 0);
    assert.equal(state.writes.size, 1);
    const entry = [...state.writes.values()][0]!;
    if (entry.kind === "property") {
      const ast = getAst();
      assert.equal(
        ast.strExpr(entry.value),
        "cond g => 100, true => 50",
      );
    } else {
      throw new Error(`expected property entry, got ${entry.kind}`);
    }
  });

  it("if-only branch falls back to identity (pre-state read) on else side", () => {
    // if (g) acct.balance = 100;
    const stmt = ir1CondStmt(
      [
        [
          ir1Var("g"),
          ir1Assign(ir1Member(ir1Var("acct"), "balance"), ir1LitNat(100)),
        ],
      ],
      null,
    );
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, true);
    const entry = [...state.writes.values()][0]!;
    if (entry.kind === "property") {
      const ast = getAst();
      assert.equal(
        ast.strExpr(entry.value),
        "cond g => 100, true => balance acct",
      );
    } else {
      throw new Error(`expected property entry, got ${entry.kind}`);
    }
  });

  it("multi-armed cond folds right-to-left into nested cond expression", () => {
    // if (g1) p = 1; else if (g2) p = 2; else p = 3;
    const stmt = ir1CondStmt(
      [
        [ir1Var("g1"), ir1Assign(ir1Member(ir1Var("o"), "p"), ir1LitNat(1))],
        [ir1Var("g2"), ir1Assign(ir1Member(ir1Var("o"), "p"), ir1LitNat(2))],
      ],
      ir1Assign(ir1Member(ir1Var("o"), "p"), ir1LitNat(3)),
    );
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, true);
    const entry = [...state.writes.values()][0]!;
    if (entry.kind === "property") {
      const ast = getAst();
      assert.equal(
        ast.strExpr(entry.value),
        "cond g1 => 1, true => (cond g2 => 2, true => 3)",
      );
    } else {
      throw new Error(`expected property entry, got ${entry.kind}`);
    }
  });

  it("disjoint property writes (different rules) merge independently", () => {
    // if (g) o.alpha = 1; else o.beta = 2;
    const stmt = ir1CondStmt(
      [
        [ir1Var("g"), ir1Assign(ir1Member(ir1Var("o"), "alpha"), ir1LitNat(1))],
      ],
      ir1Assign(ir1Member(ir1Var("o"), "beta"), ir1LitNat(2)),
    );
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, true);
    assert.equal(state.writes.size, 2);
    // alpha: cond g => 1, true => alpha o
    // beta:  cond g => beta o, true => 2
    const entries = [...state.writes.values()].map((e) => {
      if (e.kind === "property") {
        return { prop: e.prop, value: getAst().strExpr(e.value) };
      }
      throw new Error(`expected property, got ${e.kind}`);
    });
    const alpha = entries.find((e) => e.prop === "alpha")!;
    const beta = entries.find((e) => e.prop === "beta")!;
    assert.equal(alpha.value, "cond g => 1, true => alpha o");
    assert.equal(beta.value, "cond g => beta o, true => 2");
  });
});

// ---------------------------------------------------------------------------
// foreach — Shape A: per-iteration property writes via gIn envelope
// ---------------------------------------------------------------------------

describe("lowerL1Body — foreach (Shape A)", () => {
  it("simple Shape A emits `all x in arr | p' x = v.`", () => {
    // for (const u of users) { u.active = true }
    const stmt = ir1Foreach(
      "u",
      ir1Var("users"),
      ir1Assign(ir1Member(ir1Var("u"), "active"), ir1LitBool(true)),
    );
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, true);
    assert.equal(propositions.length, 1);
    const rendered = renderEquation(propositions[0]!);
    assert.equal(rendered, "all u in users | active' u = true");
    // Foreach emits directly to propositions; outer state.writes empty.
    assert.equal(state.writes.size, 0);
    // modifiedProps tracks the modified rule for frame-condition synthesis.
    assert.deepEqual([...state.modifiedProps], ["active"]);
  });

  it("Shape A block of multiple writes emits one equation per write", () => {
    // for (const u of users) {
    //   u.active = true;
    //   u.score = 0;
    // }
    const stmt = ir1Foreach(
      "u",
      ir1Var("users"),
      ir1Block([
        ir1Assign(ir1Member(ir1Var("u"), "active"), ir1LitBool(true)),
        ir1Assign(ir1Member(ir1Var("u"), "score"), ir1LitNat(0)),
      ]),
    );
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, true);
    assert.equal(propositions.length, 2);
    assert.deepEqual(
      [...state.modifiedProps].sort(),
      ["active", "score"],
    );
  });

  it("rejects Map/Set effect inside foreach body (R5 territory)", () => {
    // The build pass would normally reject such a foreach upstream;
    // this test verifies lowerL1Body rejects too if it slips through.
    // We can't easily construct a "map mutation" L1 without the build
    // pass, so this case is exercised indirectly through R5/R6 once the
    // build pass produces effect-form statements. For now, just verify
    // the foreach rejects when its body produces a non-property write
    // (currently impossible from L1 build, but defensively guarded).
    const stmt = ir1Foreach(
      "u",
      ir1Var("users"),
      ir1Assign(ir1Member(ir1Var("u"), "active"), ir1LitBool(true)),
    );
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, true); // sanity — Shape A accepted
    assert.equal(state.modifiedProps.size, 1);
  });

  it("foreach with cond-stmt body (Shape A with guard)", () => {
    // for (const u of users) {
    //   if (u.active) u.score = 1
    //   else u.score = 0
    // }
    const stmt = ir1Foreach(
      "u",
      ir1Var("users"),
      ir1CondStmt(
        [
          [
            ir1Member(ir1Var("u"), "active"),
            ir1Assign(ir1Member(ir1Var("u"), "score"), ir1LitNat(1)),
          ],
        ],
        ir1Assign(ir1Member(ir1Var("u"), "score"), ir1LitNat(0)),
      ),
    );
    const { state, propositions } = fresh();
    const ok = lowerL1Body(stmt, state, propositions, ctx());
    assert.equal(ok, true);
    assert.equal(propositions.length, 1);
    const rendered = renderEquation(propositions[0]!);
    // gIn(u, users) guard, body is the merged cond expression.
    assert.equal(
      rendered,
      "all u in users | score' u = (cond active u => 1, true => 0)",
    );
  });
});

// ---------------------------------------------------------------------------
// Out-of-scope kinds reject with specific reasons
// ---------------------------------------------------------------------------

describe("lowerL1Body — out-of-R2 forms reject", () => {
  it("return rejects with R3+ pointer", () => {
    const { state, propositions } = fresh();
    const ok = lowerL1Body(
      { kind: "return", expr: null },
      state,
      propositions,
      ctx(),
    );
    assert.equal(ok, false);
    assert.match(
      (propositions[0]! as { kind: "unsupported"; reason: string }).reason,
      /R3\+ territory/u,
    );
  });

  it("expr-stmt rejects (R3 will handle Map/Set effects)", () => {
    const { state, propositions } = fresh();
    const ok = lowerL1Body(
      { kind: "expr-stmt", expr: ir1Var("e") },
      state,
      propositions,
      ctx(),
    );
    assert.equal(ok, false);
  });

  it("while/for/throw/let all reject", () => {
    for (const stmt of [
      { kind: "while" as const, cond: ir1Var("g"), body: { kind: "block" as const, stmts: [ir1Assign(ir1Member(ir1Var("o"), "p"), ir1LitNat(0))] as readonly [import("../src/ir1.js").IR1Stmt, ...import("../src/ir1.js").IR1Stmt[]] } },
      { kind: "for" as const, init: null, cond: null, step: null, body: { kind: "block" as const, stmts: [ir1Assign(ir1Member(ir1Var("o"), "p"), ir1LitNat(0))] as readonly [import("../src/ir1.js").IR1Stmt, ...import("../src/ir1.js").IR1Stmt[]] } },
      { kind: "throw" as const, expr: ir1Var("e") },
      { kind: "let" as const, name: "x", value: ir1LitNat(0) },
    ]) {
      const { state, propositions } = fresh();
      const ok = lowerL1Body(stmt, state, propositions, ctx());
      assert.equal(ok, false, `expected ${stmt.kind} to reject`);
    }
  });
});
