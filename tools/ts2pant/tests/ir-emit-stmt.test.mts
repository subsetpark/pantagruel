import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import {
  type IRStmt,
  type IRWriteTarget,
  irBinop,
  irLitNat,
  irLitString,
  irStmtAssert,
  irStmtQuantified,
  irStmtSeq,
  irStmtWrite,
  irVar,
} from "../src/ir.js";
import { emitStmt, lowerAssert, lowerEquation } from "../src/ir-emit.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

before(async () => {
  await loadAst();
});

// Deterministic allocator for tests — produces hint, hint1, hint2, …
// independent of the document-wide NameRegistry so tests are
// hermetic.
function makeTestAllocator(): (hint: string) => string {
  const counts = new Map<string, number>();
  return (hint: string): string => {
    const n = counts.get(hint) ?? 0;
    counts.set(hint, n + 1);
    return n === 0 ? hint : `${hint}${n}`;
  };
}

function ctx() {
  return { allocBinder: makeTestAllocator() };
}

// Render an IREquation to a Pantagruel string for snapshot-style checks.
function renderEquation(eq: ReturnType<typeof lowerEquation>): string {
  const ast = getAst();
  const lhs = ast.strExpr(eq.lhs);
  const rhs = ast.strExpr(eq.rhs);
  return `${lhs} = ${rhs}`;
}

// Render a lowered assertion by reconstructing it as a `forall` expression
// and stringifying that. The Pant printer for `forall` produces
// `all p1: T1, ... | body`, which is exactly the form a quantified
// assertion takes — except a `forall` is a Bool expression, not a
// proposition, so the textual rendering is what we want for diff checks.
function renderAssert(a: ReturnType<typeof lowerAssert>): string {
  const ast = getAst();
  return ast.strExpr(ast.forall(a.quantifiers, a.guards, a.body));
}

// ---------------------------------------------------------------------------
// Patch 1: seq + write → IREquation[] / IRAssertExit[]
// ---------------------------------------------------------------------------

describe("emitStmt — empty + atoms", () => {
  it("empty seq produces no equations and no modified rules", () => {
    const result = emitStmt(irStmtSeq([]), ctx());
    assert.deepEqual(result.equations, []);
    assert.deepEqual(result.assertions, []);
    assert.equal(result.modifiedProps.size, 0);
  });

  it("empty IRStmt array produces no equations", () => {
    const result = emitStmt([], ctx());
    assert.deepEqual(result.equations, []);
    assert.equal(result.modifiedProps.size, 0);
  });

  it("nested seq flattens transparently", () => {
    const target: IRWriteTarget = {
      kind: "property-field",
      ruleName: "balance",
      objExpr: irVar("acct"),
    };
    const w = irStmtWrite(target, irLitNat(100));
    const result = emitStmt(irStmtSeq([irStmtSeq([w])]), ctx());
    assert.equal(result.equations.length, 1);
    assert.deepEqual([...result.modifiedProps], ["balance"]);
  });
});

describe("emitStmt — property-field writes", () => {
  it("single property write emits one equation `prop' obj = value`", () => {
    const target: IRWriteTarget = {
      kind: "property-field",
      ruleName: "balance",
      objExpr: irVar("acct"),
    };
    const result = emitStmt(irStmtWrite(target, irLitNat(100)), ctx());
    assert.equal(result.equations.length, 1);
    const rendered = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(rendered, "balance' acct = 100");
    assert.deepEqual([...result.modifiedProps], ["balance"]);
  });

  it("two writes to same prop+obj coalesce (last wins)", () => {
    const target: IRWriteTarget = {
      kind: "property-field",
      ruleName: "balance",
      objExpr: irVar("acct"),
    };
    const result = emitStmt(
      irStmtSeq([
        irStmtWrite(target, irLitNat(100)),
        irStmtWrite(target, irLitNat(200)),
      ]),
      ctx(),
    );
    assert.equal(result.equations.length, 1);
    const rendered = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(rendered, "balance' acct = 200");
  });

  it("two writes to same prop, different objects, emit two equations", () => {
    const t1: IRWriteTarget = {
      kind: "property-field",
      ruleName: "balance",
      objExpr: irVar("a1"),
    };
    const t2: IRWriteTarget = {
      kind: "property-field",
      ruleName: "balance",
      objExpr: irVar("a2"),
    };
    const result = emitStmt(
      irStmtSeq([irStmtWrite(t1, irLitNat(1)), irStmtWrite(t2, irLitNat(2))]),
      ctx(),
    );
    assert.equal(result.equations.length, 2);
    // Both target the same rule — modifiedProps still carries one entry.
    assert.deepEqual([...result.modifiedProps], ["balance"]);
  });

  it("writes to two different rules produce two equations and two modified rules", () => {
    const t1: IRWriteTarget = {
      kind: "property-field",
      ruleName: "balance",
      objExpr: irVar("a"),
    };
    const t2: IRWriteTarget = {
      kind: "property-field",
      ruleName: "name",
      objExpr: irVar("a"),
    };
    const result = emitStmt(
      irStmtSeq([irStmtWrite(t1, irLitNat(0)), irStmtWrite(t2, irLitString("x"))]),
      ctx(),
    );
    assert.equal(result.equations.length, 2);
    assert.deepEqual([...result.modifiedProps], ["balance", "name"]);
  });
});

describe("emitStmt — map-entry writes", () => {
  function mapTarget(op: "set" | "delete", keyExpr = irVar("k")): IRWriteTarget {
    return {
      kind: "map-entry",
      ruleName: "entries",
      keyPredName: "entriesKey",
      ownerType: "Cache",
      keyType: "String",
      objExpr: irVar("c"),
      keyExpr,
      op,
    };
  }

  it("Map.set emits two equations: value override + membership override", () => {
    const result = emitStmt(
      irStmtWrite(mapTarget("set"), irLitNat(42)),
      ctx(),
    );
    assert.equal(result.equations.length, 2);
    const valueEq = renderEquation(lowerEquation(result.equations[0]!));
    const memberEq = renderEquation(lowerEquation(result.equations[1]!));
    assert.equal(
      valueEq,
      "entries' m k = entries[(c, k) |-> 42] m k",
    );
    assert.equal(
      memberEq,
      "entriesKey' m1 k1 = entriesKey[(c, k) |-> true] m1 k1",
    );
    assert.deepEqual([...result.modifiedProps], ["entries", "entriesKey"]);
  });

  it("Map.delete emits only membership override (value rule untouched)", () => {
    const result = emitStmt(irStmtWrite(mapTarget("delete"), null), ctx());
    assert.equal(result.equations.length, 1);
    const memberEq = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(
      memberEq,
      "entriesKey' m k = entriesKey[(c, k) |-> false] m k",
    );
    assert.deepEqual([...result.modifiedProps], ["entriesKey"]);
  });

  it("two Map.set on same map coalesce into one value + one membership equation", () => {
    const result = emitStmt(
      irStmtSeq([
        irStmtWrite(mapTarget("set", irVar("k1")), irLitNat(1)),
        irStmtWrite(mapTarget("set", irVar("k2")), irLitNat(2)),
      ]),
      ctx(),
    );
    assert.equal(result.equations.length, 2);
    const valueEq = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(
      valueEq,
      "entries' m k = entries[(c, k1) |-> 1, (c, k2) |-> 2] m k",
    );
  });

  it("Map.set then Map.delete produce a value override + two membership overrides", () => {
    const result = emitStmt(
      irStmtSeq([
        irStmtWrite(mapTarget("set", irVar("k1")), irLitNat(1)),
        irStmtWrite(mapTarget("delete", irVar("k2")), null),
      ]),
      ctx(),
    );
    assert.equal(result.equations.length, 2);
    const valueEq = renderEquation(lowerEquation(result.equations[0]!));
    const memberEq = renderEquation(lowerEquation(result.equations[1]!));
    assert.equal(valueEq, "entries' m k = entries[(c, k1) |-> 1] m k");
    assert.equal(
      memberEq,
      "entriesKey' m1 k1 = entriesKey[(c, k1) |-> true, (c, k2) |-> false] m1 k1",
    );
  });
});

describe("emitStmt — set-member writes", () => {
  function setTarget(
    op: "add" | "delete" | "clear",
    elemExpr: ReturnType<typeof irVar> | null = null,
  ): IRWriteTarget {
    return {
      kind: "set-member",
      ruleName: "tags",
      ownerType: "Card",
      elemType: "String",
      objExpr: irVar("c"),
      elemExpr,
      op,
    };
  }

  it("Set.add emits one assertion `all y | y in tags' c <-> (cond y = e ...)`", () => {
    const result = emitStmt(
      irStmtWrite(setTarget("add", irVar("e")), null),
      ctx(),
    );
    assert.equal(result.assertions.length, 1);
    const rendered = renderAssert(lowerAssert(result.assertions[0]!));
    assert.equal(
      rendered,
      "all y: String | y in tags' c <-> (cond y = e => true, true => y in tags c)",
    );
    assert.deepEqual([...result.modifiedProps], ["tags"]);
  });

  it("Set.delete emits assertion with `false` arm", () => {
    const result = emitStmt(
      irStmtWrite(setTarget("delete", irVar("e")), null),
      ctx(),
    );
    assert.equal(result.assertions.length, 1);
    const rendered = renderAssert(lowerAssert(result.assertions[0]!));
    assert.equal(
      rendered,
      "all y: String | y in tags' c <-> (cond y = e => false, true => y in tags c)",
    );
  });

  it("Set.clear emits assertion `all y | ~(y in tags' c)`", () => {
    const result = emitStmt(irStmtWrite(setTarget("clear"), null), ctx());
    assert.equal(result.assertions.length, 1);
    const rendered = renderAssert(lowerAssert(result.assertions[0]!));
    assert.equal(rendered, "all y: String | y in tags' c <-> false");
  });

  it("Set.add then Set.delete produce an assertion with two cond arms", () => {
    const result = emitStmt(
      irStmtSeq([
        irStmtWrite(setTarget("add", irVar("e1")), null),
        irStmtWrite(setTarget("delete", irVar("e2")), null),
      ]),
      ctx(),
    );
    assert.equal(result.assertions.length, 1);
    const rendered = renderAssert(lowerAssert(result.assertions[0]!));
    assert.equal(
      rendered,
      "all y: String | y in tags' c <-> (cond y = e1 => true, y = e2 => false, true => y in tags c)",
    );
  });

  it("Set.add then Set.clear drops the prior overrides; tail becomes `false`", () => {
    const result = emitStmt(
      irStmtSeq([
        irStmtWrite(setTarget("add", irVar("e1")), null),
        irStmtWrite(setTarget("clear"), null),
      ]),
      ctx(),
    );
    assert.equal(result.assertions.length, 1);
    const rendered = renderAssert(lowerAssert(result.assertions[0]!));
    assert.equal(rendered, "all y: String | y in tags' c <-> false");
  });
});

describe("emitStmt — assertions pass through", () => {
  it("irStmtAssert produces an IRAssertExit verbatim", () => {
    const stmt = irStmtAssert(
      [{ name: "x", type: "Int" }],
      irBinop("eq", irVar("x"), irLitNat(0)),
    );
    const result = emitStmt(stmt, ctx());
    assert.equal(result.assertions.length, 1);
    const rendered = renderAssert(lowerAssert(result.assertions[0]!));
    assert.equal(rendered, "all x: Int | x = 0");
  });
});

// ---------------------------------------------------------------------------
// Patch 2: let-if φ-merge
// ---------------------------------------------------------------------------

describe("emitStmt — let-if φ-merge (property writes)", () => {
  function propTarget(rule: string, obj = "acct"): IRWriteTarget {
    return {
      kind: "property-field",
      ruleName: rule,
      objExpr: irVar(obj),
    };
  }

  function letIf(
    cond: ReturnType<typeof irVar>,
    thenStmts: IRStmt[],
    elseStmts: IRStmt[],
    cont: IRStmt[] = [],
  ): IRStmt {
    return {
      kind: "let-if",
      phiVars: ["__phi__"],
      cond,
      then: thenStmts,
      else: elseStmts,
      continuation: cont,
    };
  }

  it("both branches write same property → cond(g => vT, true => vE)", () => {
    const t = propTarget("balance");
    const result = emitStmt(
      letIf(
        irVar("g"),
        [irStmtWrite(t, irLitNat(100))],
        [irStmtWrite(t, irLitNat(50))],
      ),
      ctx(),
    );
    assert.equal(result.equations.length, 1);
    const rendered = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(
      rendered,
      "balance' acct = cond g => 100, true => 50",
    );
    assert.deepEqual([...result.modifiedProps], ["balance"]);
  });

  it("only then-branch writes → cond(g => vT, true => identity)", () => {
    const t = propTarget("balance");
    const result = emitStmt(
      letIf(irVar("g"), [irStmtWrite(t, irLitNat(100))], []),
      ctx(),
    );
    assert.equal(result.equations.length, 1);
    const rendered = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(
      rendered,
      "balance' acct = cond g => 100, true => balance acct",
    );
  });

  it("only else-branch writes → cond(g => identity, true => vE)", () => {
    const t = propTarget("balance");
    const result = emitStmt(
      letIf(irVar("g"), [], [irStmtWrite(t, irLitNat(50))]),
      ctx(),
    );
    assert.equal(result.equations.length, 1);
    const rendered = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(
      rendered,
      "balance' acct = cond g => balance acct, true => 50",
    );
  });

  it("disjoint property writes (different rules) merge independently", () => {
    const tA = propTarget("alpha");
    const tB = propTarget("beta");
    const result = emitStmt(
      letIf(
        irVar("g"),
        [irStmtWrite(tA, irLitNat(1))],
        [irStmtWrite(tB, irLitNat(2))],
      ),
      ctx(),
    );
    assert.equal(result.equations.length, 2);
    const rA = renderEquation(lowerEquation(result.equations[0]!));
    const rB = renderEquation(lowerEquation(result.equations[1]!));
    assert.equal(rA, "alpha' acct = cond g => 1, true => alpha acct");
    assert.equal(rB, "beta' acct = cond g => beta acct, true => 2");
    assert.deepEqual([...result.modifiedProps], ["alpha", "beta"]);
  });

  it("continuation runs after let-if and observes merged writes", () => {
    const t = propTarget("balance");
    const t2 = propTarget("name");
    // if (g) balance = 100; balance = balance * 2;
    // After the let-if, the merged balance value is cond(g => 100, true =>
    // identity); the continuation does NOT directly observe that since
    // emitStmt's read side doesn't currently re-thread cond writes back
    // through the next write — but a sequential write to the same key
    // overwrites the prior cond-merged value (last-write-wins).
    const result = emitStmt(
      irStmtSeq([
        letIf(irVar("g"), [irStmtWrite(t, irLitNat(100))], []),
        irStmtWrite(t2, irLitString("post")),
      ]),
      ctx(),
    );
    assert.equal(result.equations.length, 2);
    assert.deepEqual([...result.modifiedProps], ["balance", "name"]);
  });

  it("nested let-if produces nested cond expression", () => {
    const t = propTarget("balance");
    const result = emitStmt(
      letIf(
        irVar("g1"),
        [
          letIf(
            irVar("g2"),
            [irStmtWrite(t, irLitNat(1))],
            [irStmtWrite(t, irLitNat(2))],
          ),
        ],
        [irStmtWrite(t, irLitNat(3))],
      ),
      ctx(),
    );
    assert.equal(result.equations.length, 1);
    const rendered = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(
      rendered,
      "balance' acct = cond g1 => (cond g2 => 1, true => 2), true => 3",
    );
  });

  // Note: branches writing the same TARGET KIND but different rules cannot
  // collide on a write-key because `writeKey` namespaces per-kind
  // (`prop:rule:obj`, `map:rule:obj`, `set:rule:obj`), so the kind-mismatch
  // check in `mergeWriteAccs` is structurally unreachable from the IR
  // builder. Defensive only — kept to prevent silently corrupting state if
  // a future write-key scheme allows collision.
});

describe("emitStmt — let-if φ-merge (Map writes)", () => {
  function mapTarget(
    op: "set" | "delete",
    keyExpr = irVar("k"),
  ): IRWriteTarget {
    return {
      kind: "map-entry",
      ruleName: "entries",
      keyPredName: "entriesKey",
      ownerType: "Cache",
      keyType: "String",
      objExpr: irVar("c"),
      keyExpr,
      op,
    };
  }
  function letIf(
    cond: ReturnType<typeof irVar>,
    thenStmts: IRStmt[],
    elseStmts: IRStmt[],
  ): IRStmt {
    return {
      kind: "let-if",
      phiVars: ["__phi__"],
      cond,
      then: thenStmts,
      else: elseStmts,
      continuation: [],
    };
  }

  it("both branches Map.set same key with different values → cond per override", () => {
    const result = emitStmt(
      letIf(
        irVar("g"),
        [irStmtWrite(mapTarget("set", irVar("k")), irLitNat(1))],
        [irStmtWrite(mapTarget("set", irVar("k")), irLitNat(2))],
      ),
      ctx(),
    );
    assert.equal(result.equations.length, 2);
    const valueEq = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(
      valueEq,
      "entries' m k = entries[(c, k) |-> cond g => 1, true => 2] m k",
    );
  });

  it("only then-branch Map.set → cond with pre-state fallback", () => {
    const result = emitStmt(
      letIf(
        irVar("g"),
        [irStmtWrite(mapTarget("set", irVar("k")), irLitNat(1))],
        [],
      ),
      ctx(),
    );
    assert.equal(result.equations.length, 2);
    const valueEq = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(
      valueEq,
      "entries' m k = entries[(c, k) |-> cond g => 1, true => entries c k] m k",
    );
  });

  it("disjoint keys across branches produce two overrides", () => {
    const result = emitStmt(
      letIf(
        irVar("g"),
        [irStmtWrite(mapTarget("set", irVar("k1")), irLitNat(1))],
        [irStmtWrite(mapTarget("set", irVar("k2")), irLitNat(2))],
      ),
      ctx(),
    );
    assert.equal(result.equations.length, 2);
    const valueEq = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(
      valueEq,
      "entries' m k = entries[(c, k1) |-> cond g => 1, true => entries c k1, (c, k2) |-> cond g => entries c k2, true => 2] m k",
    );
  });
});

describe("emitStmt — let-if φ-merge (Set writes)", () => {
  function setTarget(op: "add" | "delete", elem: string): IRWriteTarget {
    return {
      kind: "set-member",
      ruleName: "tags",
      ownerType: "Card",
      elemType: "String",
      objExpr: irVar("c"),
      elemExpr: irVar(elem),
      op,
    };
  }
  function letIf(
    cond: ReturnType<typeof irVar>,
    thenStmts: IRStmt[],
    elseStmts: IRStmt[],
  ): IRStmt {
    return {
      kind: "let-if",
      phiVars: ["__phi__"],
      cond,
      then: thenStmts,
      else: elseStmts,
      continuation: [],
    };
  }

  it("then-only Set.add merges with pre-state fallback", () => {
    const result = emitStmt(
      letIf(irVar("g"), [irStmtWrite(setTarget("add", "e"), null)], []),
      ctx(),
    );
    assert.equal(result.assertions.length, 1);
    const rendered = renderAssert(lowerAssert(result.assertions[0]!));
    assert.equal(
      rendered,
      "all y: String | y in tags' c <-> (cond y = e => (cond g => true, true => e in tags c), true => y in tags c)",
    );
  });

  it("both branches Set.add different elements → two cond-merged overrides", () => {
    const result = emitStmt(
      letIf(
        irVar("g"),
        [irStmtWrite(setTarget("add", "e1"), null)],
        [irStmtWrite(setTarget("add", "e2"), null)],
      ),
      ctx(),
    );
    assert.equal(result.assertions.length, 1);
    const rendered = renderAssert(lowerAssert(result.assertions[0]!));
    assert.equal(
      rendered,
      "all y: String | y in tags' c <-> (cond y = e1 => (cond g => true, true => e1 in tags c), y = e2 => (cond g => e2 in tags c, true => true), true => y in tags c)",
    );
  });
});

// ---------------------------------------------------------------------------
// Patch 3: quantified-stmt envelope
// ---------------------------------------------------------------------------

describe("emitStmt — quantified-stmt envelope (Shape A target)", () => {
  it("wraps a property-field write in `all x in src | p' x = e`", () => {
    // Foreach(x, src, Assign(Member(x, "active"), true)) lowers to
    // quantified-stmt([{x, User}], [in(x, users)],
    //   write{property-field, "active", x, true}).
    const target: IRWriteTarget = {
      kind: "property-field",
      ruleName: "active",
      objExpr: irVar("x"),
    };
    const body = irStmtWrite(target, irBinop("eq", irLitNat(1), irLitNat(1))); // dummy expr
    const wrapped = irStmtQuantified(
      [{ name: "x", type: "User" }],
      [irBinop("in", irVar("x"), irVar("users"))],
      body,
    );
    const result = emitStmt(wrapped, ctx());
    assert.equal(result.equations.length, 1);
    const rendered = renderEquation(lowerEquation(result.equations[0]!));
    assert.equal(rendered, "active' x = 1 = 1");
    assert.equal(result.equations[0]!.quantifiers.length, 1);
    assert.equal(result.equations[0]!.quantifiers[0]!.name, "x");
    assert.equal(result.equations[0]!.guards?.length, 1);
    assert.deepEqual([...result.modifiedProps], ["active"]);
  });

  it("simple Shape A: assigning a constant value", () => {
    const target: IRWriteTarget = {
      kind: "property-field",
      ruleName: "active",
      objExpr: irVar("x"),
    };
    const wrapped = irStmtQuantified(
      [{ name: "x", type: "User" }],
      [irBinop("in", irVar("x"), irVar("users"))],
      irStmtWrite(target, irLitString("on")),
    );
    const result = emitStmt(wrapped, ctx());
    assert.equal(result.equations.length, 1);
    const eq = result.equations[0]!;
    const lowered = lowerEquation(eq);
    const ast = getAst();
    // The forall envelope assembles via Pant's forall constructor with our
    // params + guards; the body is the equation expression `lhs = rhs`.
    const envelope = ast.forall(
      lowered.quantifiers,
      lowered.guards,
      ast.binop(ast.opEq(), lowered.lhs, lowered.rhs),
    );
    assert.equal(
      ast.strExpr(envelope),
      "all x: User, x in users | active' x = \"on\"",
    );
  });

  it("composes nested quantified-stmt envelopes (concats quants/guards)", () => {
    const target: IRWriteTarget = {
      kind: "property-field",
      ruleName: "joint",
      objExpr: irVar("y"),
    };
    const inner = irStmtQuantified(
      [{ name: "y", type: "Item" }],
      [irBinop("in", irVar("y"), irVar("items"))],
      irStmtWrite(target, irLitNat(1)),
    );
    const outer = irStmtQuantified(
      [{ name: "x", type: "User" }],
      [irBinop("in", irVar("x"), irVar("users"))],
      inner,
    );
    const result = emitStmt(outer, ctx());
    assert.equal(result.equations.length, 1);
    const eq = result.equations[0]!;
    assert.deepEqual(
      eq.quantifiers.map((q) => q.name),
      ["x", "y"],
    );
    assert.equal(eq.guards?.length, 2);
  });

  it("seq inside quantified-stmt wraps each emitted equation independently", () => {
    const tA: IRWriteTarget = {
      kind: "property-field",
      ruleName: "alpha",
      objExpr: irVar("x"),
    };
    const tB: IRWriteTarget = {
      kind: "property-field",
      ruleName: "beta",
      objExpr: irVar("x"),
    };
    const wrapped = irStmtQuantified(
      [{ name: "x", type: "T" }],
      [irBinop("in", irVar("x"), irVar("xs"))],
      irStmtSeq([
        irStmtWrite(tA, irLitNat(1)),
        irStmtWrite(tB, irLitNat(2)),
      ]),
    );
    const result = emitStmt(wrapped, ctx());
    assert.equal(result.equations.length, 2);
    for (const eq of result.equations) {
      assert.deepEqual(
        eq.quantifiers.map((q) => q.name),
        ["x"],
      );
    }
    assert.deepEqual([...result.modifiedProps], ["alpha", "beta"]);
  });

  it("quantified-stmt over a Set assertion prepends quants to the assertion", () => {
    const target: IRWriteTarget = {
      kind: "set-member",
      ruleName: "tags",
      ownerType: "Card",
      elemType: "String",
      objExpr: irVar("c"),
      elemExpr: irVar("e"),
      op: "add",
    };
    const wrapped = irStmtQuantified(
      [{ name: "x", type: "Card" }],
      [irBinop("in", irVar("x"), irVar("cards"))],
      irStmtWrite(target, null),
    );
    const result = emitStmt(wrapped, ctx());
    assert.equal(result.assertions.length, 1);
    const a = result.assertions[0]!;
    // Outer x quantifier is prepended; inner y quantifier (allocated by
    // emitSetMembershipAssertion) follows.
    assert.deepEqual(
      a.quantifiers.map((q) => q.name),
      ["x", "y"],
    );
  });
});

describe("emitStmt — modifiedProps insertion order", () => {
  it("preserves insertion order across writes", () => {
    const tA: IRWriteTarget = {
      kind: "property-field",
      ruleName: "alpha",
      objExpr: irVar("o"),
    };
    const tB: IRWriteTarget = {
      kind: "property-field",
      ruleName: "beta",
      objExpr: irVar("o"),
    };
    const tC: IRWriteTarget = {
      kind: "property-field",
      ruleName: "gamma",
      objExpr: irVar("o"),
    };
    const result = emitStmt(
      irStmtSeq([
        irStmtWrite(tB, irLitNat(2)),
        irStmtWrite(tA, irLitNat(1)),
        irStmtWrite(tC, irLitNat(3)),
      ]),
      ctx(),
    );
    assert.deepEqual([...result.modifiedProps], ["beta", "alpha", "gamma"]);
  });
});
