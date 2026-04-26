import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import {
  type IRStmt,
  type IRWriteTarget,
  irBinop,
  irLitNat,
  irLitString,
  irStmtAssert,
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

describe("emitStmt — let-if defers to Patch 2", () => {
  it("let-if throws NOT_IMPL pointing at M3 Patch 2", () => {
    const stmt: IRStmt = {
      kind: "let-if",
      phiVars: ["balance:acct"],
      cond: irVar("g"),
      then: [],
      else: [],
      continuation: [],
    };
    assert.throws(() => emitStmt(stmt, ctx()), /Patch 2/);
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
