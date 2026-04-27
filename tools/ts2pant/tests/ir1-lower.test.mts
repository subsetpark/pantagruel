import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import {
  irAppName,
  irBinop,
  irCombTyped,
  irCond,
  irLitBool,
  irLitNat,
  irUnop,
  irVar,
} from "../src/ir.js";
import { lowerExpr } from "../src/ir-emit.js";
import {
  ir1App,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1Cond,
  ir1CondStmt,
  ir1ExprStmt,
  ir1For,
  ir1Foreach,
  ir1FromL2,
  ir1IsNullish,
  ir1Let,
  ir1LitBool,
  ir1LitNat,
  ir1LitString,
  ir1Member,
  ir1Return,
  ir1Throw,
  ir1Unop,
  ir1Var,
  ir1While,
} from "../src/ir1.js";
import { lowerL1Expr } from "../src/ir1-lower.js";
import { lowerL1Body } from "../src/ir1-lower-body.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { makeSymbolicState } from "../src/translate-body.js";
import type { PropResult } from "../src/types.js";

before(async () => {
  await loadAst();
});

// ---------------------------------------------------------------------------
// L1 → L2 lowering: structural equality
//
// Each test builds an L1 expression and asserts that lowering produces an
// L2 IRExpr structurally equal to a hand-built reference. Structural
// equality on the L2 tree is the cutover gate for Patch 3 — if L1 lowering
// matches the L2 trees today's legacy code produces, then deleting the
// legacy paths cannot regress snapshots.
// ---------------------------------------------------------------------------

describe("lowerL1Expr — atoms", () => {
  it("var lowers to L2 var", () => {
    const l2 = lowerL1Expr(ir1Var("x"));
    assert.deepEqual(l2, irVar("x", false));
  });

  it("primed var lowers to L2 primed var", () => {
    const l2 = lowerL1Expr(ir1Var("x", true));
    assert.deepEqual(l2, irVar("x", true));
  });

  it("lit nat / bool / string lower to corresponding L2 lit", () => {
    assert.deepEqual(lowerL1Expr(ir1LitNat(42)), irLitNat(42));
    assert.deepEqual(lowerL1Expr(ir1LitBool(true)), irLitBool(true));
    assert.deepEqual(lowerL1Expr(ir1LitBool(false)), irLitBool(false));
    assert.deepEqual(lowerL1Expr(ir1LitString("hi")), {
      kind: "lit",
      value: { kind: "string", value: "hi" },
    });
  });
});

describe("lowerL1Expr — operators", () => {
  it("binop lowers to L2 binop", () => {
    const l2 = lowerL1Expr(ir1Binop("add", ir1Var("a"), ir1Var("b")));
    assert.deepEqual(l2, irBinop("add", irVar("a", false), irVar("b", false)));
  });

  it("unop lowers to L2 unop", () => {
    const l2 = lowerL1Expr(ir1Unop("not", ir1Var("p")));
    assert.deepEqual(l2, irUnop("not", irVar("p", false)));
  });

  it("nested binop preserves structure", () => {
    // (a + b) * c
    const l1 = ir1Binop(
      "mul",
      ir1Binop("add", ir1Var("a"), ir1Var("b")),
      ir1Var("c"),
    );
    const l2 = lowerL1Expr(l1);
    assert.deepEqual(
      l2,
      irBinop(
        "mul",
        irBinop("add", irVar("a", false), irVar("b", false)),
        irVar("c", false),
      ),
    );
  });
});

describe("lowerL1Expr — applications", () => {
  it("var-headed app lowers to L2 named application", () => {
    const l1 = ir1App(ir1Var("foo"), [ir1Var("a"), ir1Var("b")]);
    const l2 = lowerL1Expr(l1);
    assert.deepEqual(
      l2,
      irAppName("foo", [irVar("a", false), irVar("b", false)]),
    );
  });

  it("zero-arity var-headed app lowers to L2 named application with no args", () => {
    const l1 = ir1App(ir1Var("foo"), []);
    const l2 = lowerL1Expr(l1);
    assert.deepEqual(l2, irAppName("foo", []));
  });

  it("non-var-headed app lowers via L2 expr-headed application", () => {
    // (cond ? f : g)(x) — pathological but syntactically valid
    const l1 = ir1App(
      ir1Cond([[ir1Var("c"), ir1Var("f")]], ir1Var("g")),
      [ir1Var("x")],
    );
    const l2 = lowerL1Expr(l1);
    // Just check the shape: head is expression-kind, args has one var.
    assert.equal(l2.kind, "app");
    if (l2.kind === "app") {
      assert.equal(l2.head.kind, "expr");
      assert.equal(l2.args.length, 1);
    }
  });

  it("member lowers to qualified named application of receiver", () => {
    // member(obj, "balance") → App(name="balance", [obj])
    const l1 = ir1Member(ir1Var("obj"), "balance");
    const l2 = lowerL1Expr(l1);
    assert.deepEqual(l2, irAppName("balance", [irVar("obj", false)]));
  });
});

describe("lowerL1Expr — cond (byte-equality with legacy ast.cond)", () => {
  it("single-arm cond produces L2 cond with literal-true tail", () => {
    // cond [(g, v)] otherwise=d  →  irCond [(g, v), (true, d)]
    const l1 = ir1Cond([[ir1Var("g"), ir1Var("v")]], ir1Var("d"));
    const l2 = lowerL1Expr(l1);
    assert.deepEqual(
      l2,
      irCond([
        [irVar("g", false), irVar("v", false)],
        [irLitBool(true), irVar("d", false)],
      ]),
    );
  });

  it("multi-arm cond preserves arm order and emits literal-true tail", () => {
    // cond [(g1, v1), (g2, v2), (g3, v3)] otherwise=d
    const l1 = ir1Cond(
      [
        [ir1Var("g1"), ir1Var("v1")],
        [ir1Var("g2"), ir1Var("v2")],
        [ir1Var("g3"), ir1Var("v3")],
      ],
      ir1Var("d"),
    );
    const l2 = lowerL1Expr(l1);
    assert.deepEqual(
      l2,
      irCond([
        [irVar("g1", false), irVar("v1", false)],
        [irVar("g2", false), irVar("v2", false)],
        [irVar("g3", false), irVar("v3", false)],
        [irLitBool(true), irVar("d", false)],
      ]),
    );
  });

  it("nested cond lowers recursively", () => {
    // cond [(g, cond[(h, x)] y)] otherwise=z
    const inner = ir1Cond([[ir1Var("h"), ir1Var("x")]], ir1Var("y"));
    const outer = ir1Cond([[ir1Var("g"), inner]], ir1Var("z"));
    const l2 = lowerL1Expr(outer);
    const expectedInner = irCond([
      [irVar("h", false), irVar("x", false)],
      [irLitBool(true), irVar("y", false)],
    ]);
    assert.deepEqual(
      l2,
      irCond([
        [irVar("g", false), expectedInner],
        [irLitBool(true), irVar("z", false)],
      ]),
    );
  });
});

describe("lowerL1Expr — is-nullish (M4 canonical nullish test)", () => {
  it("IsNullish lowers to binop(eq, unop(card, x), litNat(0))", () => {
    const l1 = ir1IsNullish(ir1Var("x"));
    const l2 = lowerL1Expr(l1);
    assert.deepEqual(
      l2,
      irBinop("eq", irUnop("card", irVar("x", false)), irLitNat(0)),
    );
  });

  it("IsNullish operand can be a member expression", () => {
    // is-nullish(receiver.prop)
    const l1 = ir1IsNullish(ir1Member(ir1Var("receiver"), "prop"));
    const l2 = lowerL1Expr(l1);
    assert.deepEqual(
      l2,
      irBinop(
        "eq",
        irUnop("card", irAppName("prop", [irVar("receiver", false)])),
        irLitNat(0),
      ),
    );
  });

  it("IsNullish lowered shape matches manual ast.binop construction", () => {
    const ast = getAst();

    // Manual: ast.binop(opEq, unop(opCard, var "x"), litNat 0)
    const manual = ast.binop(
      ast.opEq(),
      ast.unop(ast.opCard(), ast.var("x")),
      ast.litNat(0),
    );

    const l1Lowered = lowerExpr(lowerL1Expr(ir1IsNullish(ir1Var("x"))));

    assert.equal(ast.strExpr(l1Lowered), ast.strExpr(manual));
  });
});

describe("lowerL1Expr — from-l2 transitional adapter", () => {
  it("from-l2 unwraps to the held L2 expression", () => {
    const l2Inner = irBinop("add", irVar("a", false), irLitNat(1));
    const l1 = ir1FromL2(l2Inner);
    const lowered = lowerL1Expr(l1);
    assert.equal(lowered, l2Inner); // identity, not just deep equal
  });

  it("from-l2 inside a cond arm composes correctly", () => {
    // cond [(g, from-l2(a + 1))] otherwise=0  →
    //   cond [(g, a+1), (true, 0)]
    const l1 = ir1Cond(
      [
        [
          ir1Var("g"),
          ir1FromL2(irBinop("add", irVar("a", false), irLitNat(1))),
        ],
      ],
      ir1LitNat(0),
    );
    const l2 = lowerL1Expr(l1);
    assert.deepEqual(
      l2,
      irCond([
        [
          irVar("g", false),
          irBinop("add", irVar("a", false), irLitNat(1)),
        ],
        [irLitBool(true), irLitNat(0)],
      ]),
    );
  });
});

describe("L1 → L2 → OpaqueExpr — full pipeline byte-equality with legacy", () => {
  // Verifies that lowering a hand-built L1 cond through L1 → L2 → OpaqueExpr
  // produces the same Pantagruel string as a hand-built legacy `ast.cond`.
  // This is the cutover gate: snapshots are byte-identical iff the full
  // pipeline emits the same string at corresponding inputs.

  it("ternary-shaped cond emits the legacy Pant string", () => {
    const ast = getAst();

    // Legacy: ast.cond([[a >= 0, a], [true, -a]])
    const legacyCond = ast.cond([
      [ast.binop(ast.opGe(), ast.var("a"), ast.litNat(0)), ast.var("a")],
      [ast.litBool(true), ast.unop(ast.opNeg(), ast.var("a"))],
    ]);

    // L1: cond([(a >= 0, a)], otherwise=-a)
    const l1 = ir1Cond(
      [[ir1Binop("ge", ir1Var("a"), ir1LitNat(0)), ir1Var("a")]],
      ir1Unop("neg", ir1Var("a")),
    );
    const l1Lowered = lowerExpr(lowerL1Expr(l1));

    assert.equal(ast.strExpr(l1Lowered), ast.strExpr(legacyCond));
  });

  it("multi-arm cond emits the legacy Pant string", () => {
    const ast = getAst();

    // Legacy: cond([[g1, v1], [g2, v2], [true, d]])
    const legacyCond = ast.cond([
      [ast.var("g1"), ast.var("v1")],
      [ast.var("g2"), ast.var("v2")],
      [ast.litBool(true), ast.var("d")],
    ]);

    const l1 = ir1Cond(
      [
        [ir1Var("g1"), ir1Var("v1")],
        [ir1Var("g2"), ir1Var("v2")],
      ],
      ir1Var("d"),
    );
    const l1Lowered = lowerExpr(lowerL1Expr(l1));

    assert.equal(ast.strExpr(l1Lowered), ast.strExpr(legacyCond));
  });
});

// ---------------------------------------------------------------------------
// Vocabulary-locked stubs throw not-implemented
// ---------------------------------------------------------------------------

describe("M2: ir1Assign and ir1While constructors are active", () => {
  it("ir1Assign builds an `assign` statement", () => {
    const stmt = ir1Assign(ir1Var("i"), ir1LitNat(1));
    assert.equal(stmt.kind, "assign");
    if (stmt.kind === "assign") {
      assert.deepEqual(stmt.target, ir1Var("i"));
      assert.deepEqual(stmt.value, ir1LitNat(1));
    }
  });

  it("ir1Assign target may be a Member expression", () => {
    const stmt = ir1Assign(
      ir1Member(ir1Var("a"), "balance"),
      ir1LitNat(0),
    );
    assert.equal(stmt.kind, "assign");
    if (stmt.kind === "assign") {
      assert.equal(stmt.target.kind, "member");
    }
  });

  it("ir1While builds a `while` statement with body", () => {
    const stmt = ir1While(
      ir1Var("p"),
      ir1Assign(ir1Var("i"), ir1LitNat(0)),
    );
    assert.equal(stmt.kind, "while");
    if (stmt.kind === "while") {
      assert.deepEqual(stmt.cond, ir1Var("p"));
      assert.equal(stmt.body.kind, "assign");
    }
  });
});

describe("M3 statement constructors are active", () => {
  // Constructors return IR1Stmt values of the appropriate kind.
  // Lowering for these forms is provided by `lowerL1Body` (in
  // `ir1-lower-body.ts`) which threads `SymbolicState` directly into
  // `PropResult[]` — the L2 path is expression-only.

  it("ir1CondStmt builds a cond-stmt", () => {
    const stmt = ir1CondStmt(
      [[ir1Var("g"), ir1Return(ir1Var("v"))]],
      null,
    );
    assert.equal(stmt.kind, "cond-stmt");
  });

  it("ir1Foreach builds a foreach", () => {
    // Body must be in `IR1ForeachBody` — Shape A iterator writes only.
    // `ir1Assign(Member(x, "p"), v)` is a member-target assign, the
    // canonical Shape A form.
    const stmt = ir1Foreach(
      "x",
      ir1Var("xs"),
      ir1Assign(ir1Member(ir1Var("x"), "p"), ir1LitNat(1)),
    );
    assert.equal(stmt.kind, "foreach");
  });

  it("ir1For builds a for", () => {
    const stmt = ir1For(null, null, null, ir1Return(null));
    assert.equal(stmt.kind, "for");
  });

  it("ir1Throw builds a throw", () => {
    const stmt = ir1Throw(ir1Var("err"));
    assert.equal(stmt.kind, "throw");
  });

  it("ir1ExprStmt builds an expr-stmt", () => {
    const stmt = ir1ExprStmt(ir1Var("e"));
    assert.equal(stmt.kind, "expr-stmt");
  });
});

// ---------------------------------------------------------------------------
// Active statement constructors don't throw (vocabulary-active subset for M1)
// ---------------------------------------------------------------------------

describe("active statement constructors (block, let, return)", () => {
  it("ir1Block constructs a block of two statements", () => {
    const block = ir1Block([
      ir1Let("x", ir1LitNat(1)),
      ir1Return(ir1Var("x")),
    ]);
    assert.equal(block.kind, "block");
  });

  it("ir1Block collapses single-statement blocks", () => {
    const inner = ir1Return(ir1Var("x"));
    const collapsed = ir1Block([inner]);
    assert.equal(collapsed, inner);
  });

  it("ir1Let constructs a let-binding", () => {
    const stmt = ir1Let("x", ir1LitNat(42));
    assert.equal(stmt.kind, "let");
    if (stmt.kind === "let") {
      assert.equal(stmt.name, "x");
    }
  });

  it("ir1Return with expression", () => {
    const stmt = ir1Return(ir1Var("x"));
    assert.equal(stmt.kind, "return");
    if (stmt.kind === "return") {
      assert.deepEqual(stmt.expr, ir1Var("x"));
    }
  });

  it("ir1Return with null (void return)", () => {
    const stmt = ir1Return(null);
    assert.equal(stmt.kind, "return");
    if (stmt.kind === "return") {
      assert.equal(stmt.expr, null);
    }
  });
});

// ---------------------------------------------------------------------------
// lowerL1Body — branch-local proposition isolation
// ---------------------------------------------------------------------------

describe("lowerL1Body — branch-local foreach equations", () => {
  it("rejects a Shape A foreach inside a cond-stmt branch", () => {
    // `lowerForeach` emits `all $0 in xs | active' $0 = true` directly
    // into the propositions buffer. A foreach inside a branch would
    // skip the conditional merge (foreach equations don't go through
    // `state.writes` / `writtenKeys`), so the per-iter equation would
    // become unconditional. `lowerCondStmt` runs each branch into a
    // local buffer and rejects if any quantified prop appears, rather
    // than silently dropping the guard. This direct test exercises the
    // lower-pass guarantee — the build pass currently rejects the
    // construct earlier, so this is defense-in-depth.
    const stmt = ir1CondStmt(
      [
        [
          ir1Var("g"),
          ir1Foreach(
            "$0",
            ir1Var("xs"),
            ir1Assign(ir1Member(ir1Var("$0"), "active"), ir1LitBool(true)),
          ),
        ],
      ],
      null,
    );
    const propositions: PropResult[] = [];
    const ok = lowerL1Body(stmt, makeSymbolicState(), propositions, {
      applyConst: (e) => e,
    });
    assert.equal(ok, false);
    assert.ok(
      propositions.some(
        (p) =>
          p.kind === "unsupported" &&
          /escape the branch guard/.test(p.reason),
      ),
    );
  });

  it("rejects a nested proposition-emitting foreach body", () => {
    // `lowerForeach` lowers its body into a local `bodyProps` buffer
    // and rejects if any quantified prop appears, mirroring
    // `lowerCondStmt`'s discipline. Today's `IR1ForeachBody` excludes
    // `foreach`, so this is unreachable through the build pass —
    // direct construction exercises the defense-in-depth guarantee.
    // The inner foreach emits `all $1 in ys | active' $1 = true` which
    // would otherwise leak past the outer iterator scope.
    const inner = ir1Foreach(
      "$1",
      ir1Var("ys"),
      ir1Assign(ir1Member(ir1Var("$1"), "active"), ir1LitBool(true)),
    );
    // Construct via cast since `IR1ForeachBody` rejects nested
    // foreach at the type level — this test specifically exercises
    // the runtime path that catches a hand-built (or future-IR)
    // violation.
    const outer = ir1Foreach(
      "$0",
      ir1Var("xs"),
      inner as unknown as Parameters<typeof ir1Foreach>[2],
    );
    const propositions: PropResult[] = [];
    const ok = lowerL1Body(outer, makeSymbolicState(), propositions, {
      applyConst: (e) => e,
    });
    assert.equal(ok, false);
    assert.ok(
      propositions.some(
        (p) =>
          p.kind === "unsupported" &&
          /escape the outer iterator scope/.test(p.reason),
      ),
    );
  });
});

// ---------------------------------------------------------------------------
// L2 `comb-typed` form (M2 cleanup) — typed-comprehension with no source
// ---------------------------------------------------------------------------

describe("L2 comb-typed form (typed comprehension)", () => {
  it("min over each j: Int, j >= 1, ¬p(j) | j → eachComb with typed param", () => {
    const ast = getAst();
    const l2 = irCombTyped(
      "min",
      "j",
      "Int",
      [
        irBinop("ge", irVar("j"), irLitNat(1)),
        irUnop("not", irAppName("p", [irVar("j")])),
      ],
      irVar("j"),
    );
    const lowered = lowerExpr(l2);
    assert.equal(
      ast.strExpr(lowered),
      "min over each j: Int, j >= 1, ~(p j) | j",
    );
  });

  it("byte-equality with a hand-built legacy eachComb", () => {
    const ast = getAst();
    // Hand-built legacy form — what today's translateMuSearchInit emits.
    const legacy = ast.eachComb(
      [ast.param("j", ast.tName("Int"))],
      [
        ast.gExpr(ast.binop(ast.opGe(), ast.var("j"), ast.litNat(1))),
        ast.gExpr(ast.unop(ast.opNot(), ast.app(ast.var("p"), [ast.var("j")]))),
      ],
      ast.combMin(),
      ast.var("j"),
    );
    const l2 = irCombTyped(
      "min",
      "j",
      "Int",
      [
        irBinop("ge", irVar("j"), irLitNat(1)),
        irUnop("not", irAppName("p", [irVar("j")])),
      ],
      irVar("j"),
    );
    assert.equal(ast.strExpr(lowerExpr(l2)), ast.strExpr(legacy));
  });

  it("max combiner accepted (forbids init at type level)", () => {
    const ast = getAst();
    const l2 = irCombTyped("max", "k", "Nat", [], irVar("k"));
    const lowered = lowerExpr(l2);
    // No guards beyond the typed binder.
    assert.match(ast.strExpr(lowered), /max over each k: Nat \| k/);
  });
});
