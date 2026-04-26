import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import {
  irAppName,
  irBinop,
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
import { lowerL1Expr, lowerL1Stmt } from "../src/ir1-lower.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

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

describe("vocabulary-locked stubs (M3 forms)", () => {
  it("ir1CondStmt throws not-implemented (M3)", () => {
    assert.throws(
      () => ir1CondStmt([[ir1Var("g"), ir1Return(ir1Var("v"))]], null),
      /M3/,
    );
  });

  it("ir1Foreach throws not-implemented (M3)", () => {
    assert.throws(
      () => ir1Foreach("x", ir1Var("xs"), ir1Return(ir1Var("x"))),
      /M3/,
    );
  });

  it("ir1For throws not-implemented (M3)", () => {
    assert.throws(() => ir1For(null, null, null, ir1Return(null)), /M3/);
  });

  it("ir1Throw throws not-implemented (M3)", () => {
    assert.throws(() => ir1Throw(ir1Var("err")), /M3/);
  });

  it("ir1ExprStmt throws not-implemented (M3)", () => {
    assert.throws(() => ir1ExprStmt(ir1Var("e")), /M3/);
  });

  it("lowerL1Stmt throws — statement lowering is M3", () => {
    const stmt = ir1Return(ir1Var("x"));
    assert.throws(() => lowerL1Stmt(stmt), /M3/);
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
// L2 `comb-typed` form (M2 cleanup) — typed-comprehension with no source
// ---------------------------------------------------------------------------

describe("L2 comb-typed form (typed comprehension)", () => {
  it("min over each j: Int, j >= 1, ¬p(j) | j → eachComb with typed param", async () => {
    const { irBinop, irLitNat, irUnop, irVar, irAppName, irCombTyped } =
      await import("../src/ir.js");
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

  it("byte-equality with a hand-built legacy eachComb", async () => {
    const { irBinop, irLitNat, irUnop, irVar, irAppName, irCombTyped } =
      await import("../src/ir.js");
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

  it("max combiner accepted (forbids init at type level)", async () => {
    const { irLitNat, irVar, irCombTyped } = await import("../src/ir.js");
    const ast = getAst();
    const l2 = irCombTyped("max", "k", "Nat", [], irVar("k"));
    const lowered = lowerExpr(l2);
    // No guards beyond the typed binder.
    assert.match(ast.strExpr(lowered), /max over each k: Nat \| k/);
    // Suppress unused var warning.
    void irLitNat;
  });
});
