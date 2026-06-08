// @archlint.module test
// @archlint.domain ts2pant.ir1-ssa-foreach

import assert from "node:assert/strict";
import * as fc from "fast-check";
import { before, describe, it } from "node:test";

import {
  type IR1FoldLeaf,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1LitBool,
  ir1LitNat,
  ir1Member,
  ir1Var,
} from "../src/ir1.js";
import {
  foreachShapeBAccumulatorKey,
  lowerForeachShapeAAsGeneralLoop,
  lowerForeachShapeBAsGeneralLoop,
  lowerShapeAExpr,
  shapeAKey,
  type ShapeASummaryState,
  summarizeForeachShapeABody,
  summarizeShapeAAssign,
  summarizeShapeACond,
} from "../src/ir1-ssa-foreach.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

before(async () => {
  await loadAst();
});

const item = ir1Var("item");
const source = ir1Var("items");
const account = ir1Var("account");

function lowerShapeA() {
  return lowerForeachShapeAAsGeneralLoop({
    binder: "item",
    source,
    body: ir1Block([
      ir1Assign(ir1Member(item, "Item_seen"), ir1LitBool(true)),
      ir1Assign(ir1Member(item, "Item_score"), ir1LitNat(7)),
    ]),
  });
}

function shapeBFoldLeaves(): IR1FoldLeaf[] {
  return [
    {
      target: account,
      prop: "Account_total",
      combiner: "add",
      outerOp: "add",
      rhs: ir1Member(item, "Item_amount"),
      guard: null,
    },
    {
      target: account,
      prop: "Account_count",
      combiner: "add",
      outerOp: "add",
      rhs: ir1LitNat(1),
      guard: ir1Binop("gt", ir1Member(item, "Item_amount"), ir1LitNat(0)),
    },
  ];
}

function lowerShapeB() {
  return lowerForeachShapeBAsGeneralLoop({
    binder: "item",
    source,
    foldLeaves: shapeBFoldLeaves(),
  });
}

describe("ir1-ssa-foreach", () => {
  it("generated Shape A summaries preserve property keys", () => {
    fc.assert(
      fc.property(fc.constantFrom("Item_seen", "Item_score"), (prop) => {
        const state: ShapeASummaryState = {
          current: new Map(),
          lowerOpaque: (e) => e,
          initialPropertyValues: new Map(),
        };
        const diagnostics = [];
        const assign = ir1Assign(ir1Member(item, prop), ir1LitNat(1));
        assert.equal(summarizeShapeAAssign(assign, state, diagnostics), true);
        assert.equal(summarizeForeachShapeABody(assign, state, diagnostics), true);
        assert.equal(
          summarizeShapeACond(
            {
              kind: "cond-stmt",
              arms: [[ir1LitBool(true), assign]],
              otherwise: null,
            },
            state,
            diagnostics,
          ),
          true,
        );
        const lowered = lowerShapeAExpr(ir1Member(item, prop), state);
        assert.deepEqual(lowerForeachShapeAAsGeneralLoop({
          binder: "item",
          source,
          body: assign,
        }).diagnostics, []);
        assert.deepEqual(lowerForeachShapeBAsGeneralLoop({
          binder: "item",
          source,
          foldLeaves: shapeBFoldLeaves(),
        }).diagnostics, []);
        assert.equal(shapeAKey(prop, getAst().var("item")).startsWith(prop), true);
        assert.equal(
          foreachShapeBAccumulatorKey(prop, getAst().var("account")).startsWith(prop),
          true,
        );
        assert.equal(getAst().strExpr(lowered).length > 0, true);
      }),
    );
  });

  it("Shape A: emits one header join per mutated location", () => {
    const result = lowerShapeA();

    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.program.loopHeaderJoins.length, 2);
    assert.equal(result.program.loopBodies.length, 2);
    for (const body of result.program.loopBodies) {
      assert.equal(body.headerJoins.length, 1);
      assert.equal(body.headerJoins[0]?.closed, true);
      assert.ok(result.program.loopHeaderJoins.includes(body.headerJoins[0]!));
    }
  });

  it("Shape A: emits a synthetic iteration-source termination metric", () => {
    const result = lowerShapeA();

    for (const body of result.program.loopBodies) {
      const metric = body.terminationMetric;
      assert.equal(metric?.kind, "ssa-iterating-source-metric");
      if (metric?.kind === "ssa-iterating-source-metric") {
        assert.deepEqual(metric.source, source);
      }
    }
  });

  it(
    "Shape A: header join's loopBackVersion equals the per-iteration write version",
    () => {
      const result = lowerShapeA();

      for (const body of result.program.loopBodies) {
        const header = body.headerJoins[0]!;
        const write = body.writes[0]!;
        assert.equal(header.loopBackVersion, write.version);
      }
    },
  );

  it("Shape A: emits a single per-element quantified equation per location", () => {
    const ast = getAst();
    const result = lowerShapeA();

    assert.equal(result.propositions.length, 2);
    assert.deepEqual(
      result.propositions.map((prop) =>
        prop.kind === "equation" ? ast.strExpr(prop.lhs) : "",
      ),
      ["Item_seen' item", "Item_score' item"],
    );
    for (const prop of result.propositions) {
      assert.equal(prop.kind, "equation");
      if (prop.kind === "equation") {
        assert.equal(prop.guards?.length, 1);
      }
    }
  });

  it("Shape B: emits one header join per mutated location", () => {
    const result = lowerShapeB();

    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.program.loopHeaderJoins.length, 2);
    assert.equal(result.program.loopBodies.length, 2);
    assert.deepEqual(result.program.modifiedRules, [
      "Account_total",
      "Account_count",
    ]);
  });

  it("Shape B: emits a synthetic iteration-source termination metric", () => {
    const result = lowerShapeB();

    for (const body of result.program.loopBodies) {
      const metric = body.terminationMetric;
      assert.equal(metric?.kind, "ssa-iterating-source-metric");
      if (metric?.kind === "ssa-iterating-source-metric") {
        assert.deepEqual(metric.source, source);
      }
    }
  });

  it(
    "Shape B: accumulator-fold write reads prior version through the header join",
    () => {
      const result = lowerShapeB();

      for (const body of result.program.loopBodies) {
        const header = body.headerJoins[0]!;
        const write = body.writes[0]!;
        assert.equal(header.loopBackVersion, write.version);
        assert.notEqual(header.loopBackVersion, header.preheaderVersion);
        assert.equal(write.value.kind, "property");
        if (write.value.kind === "property") {
          assert.equal(write.value.value.kind, "binop");
          if (write.value.value.kind === "binop") {
            assert.deepEqual(
              write.value.value.lhs,
              ir1Member(account, write.location.ruleName),
            );
          }
        }
      }
    },
  );

  it("Shape B: emits one accumulator-fold equation per location", () => {
    const ast = getAst();
    const result = lowerShapeB();

    assert.equal(result.propositions.length, 2);
    assert.deepEqual(
      result.propositions.map((prop) =>
        prop.kind === "equation" ? ast.strExpr(prop.lhs) : "",
      ),
      ["Account_total' account", "Account_count' account"],
    );
    assert.equal(result.propositions[0]?.kind, "equation");
    if (result.propositions[0]?.kind === "equation") {
      assert.equal(
        ast.strExpr(result.propositions[0].rhs),
        "Account_total account + (+ over each item in items | Item_amount item)",
      );
    }
    assert.equal(result.propositions[1]?.kind, "equation");
    if (result.propositions[1]?.kind === "equation") {
      assert.equal(
        ast.strExpr(result.propositions[1].rhs),
        "Account_count account + (+ over each item in items, Item_amount item > 0 | 1)",
      );
    }
  });
});
