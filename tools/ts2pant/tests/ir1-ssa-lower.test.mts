import assert from "node:assert/strict";
import { before, describe, it } from "node:test";

import {
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1LitBool,
  ir1LitNat,
  ir1MapSet,
  ir1Member,
  ir1SsaPropertyLocation,
  ir1Var,
} from "../src/ir1.js";
import {
  adaptCollectionSsaLowerResult,
  adaptLoopSummaryLowerResult,
  adaptScalarSsaLowerResult,
  lowerCollectionL1BodyToSsaResult,
  lowerScalarL1BodyToSsaResult,
} from "../src/ir1-lower-body.js";
import { lowerCollectionSsaToProps } from "../src/ir1-ssa-collections.js";
import {
  appendFramesForUnmodifiedRules,
  combineIR1SsaBodyLowerResults,
  ir1SsaBodyLowerSuccess,
  ir1SsaBodyLowerUnsupported,
} from "../src/ir1-ssa-lower.js";
import {
  lowerForeachShapeASummaries,
  lowerForeachShapeBSummaries,
  lowerMuSearchSummary,
} from "../src/ir1-ssa-loops.js";
import { lowerScalarSsaToProps } from "../src/ir1-ssa-scalars.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

before(async () => {
  await loadAst();
});

describe("ir1-ssa-lower", () => {
  it("combines body-lowering results with proposition order and deduped modified rules", () => {
    const programA = {
      reads: [],
      writes: [],
      joins: [],
      loopSummaries: [],
      loopHeaderJoins: [],
      loopBodies: [],
      declaredRules: ["Account_balance", "Account_limit"],
      modifiedRules: ["Account_balance"],
      framedRules: ["Account_limit"],
    };
    const programB = {
      reads: [],
      writes: [],
      joins: [],
      loopSummaries: [],
      loopHeaderJoins: [],
      loopBodies: [],
      declaredRules: ["Account_balance", "Account_seen"],
      modifiedRules: ["Account_seen", "Account_balance"],
      framedRules: [],
    };
    const result = combineIR1SsaBodyLowerResults(
      ir1SsaBodyLowerSuccess({
        propositions: [
          { kind: "raw", text: "first" },
          { kind: "raw", text: "second" },
        ],
        modifiedRules: ["Account_balance"],
        programs: [programA],
        finalProperties: [
          {
            kind: "property",
            prop: "Account_balance",
            objExpr: getAst().var("a"),
            rhs: getAst().litNat(1),
          },
        ],
      }),
      ir1SsaBodyLowerUnsupported("unsupported loop", {
        diagnostics: [{ kind: "unsupported", reason: "unsupported map" }],
        propositions: [{ kind: "raw", text: "third" }],
        modifiedRules: ["Account_seen", "Account_balance"],
        programs: [programB],
      }),
    );

    assert.deepEqual(
      result.propositions.map((prop) =>
        prop.kind === "raw" ? prop.text : prop.kind,
      ),
      ["first", "second", "third"],
    );
    assert.deepEqual(result.modifiedRules, ["Account_balance", "Account_seen"]);
    assert.deepEqual(result.diagnostics, [
      { kind: "unsupported", reason: "unsupported loop" },
      { kind: "unsupported", reason: "unsupported map" },
    ]);
    assert.deepEqual(result.programs, [programA, programB]);
    assert.equal(result.finalProperties?.length, 1);
  });

  it("appends frames only for unmodified rules when diagnostics are absent", () => {
    const ast = getAst();
    const result = appendFramesForUnmodifiedRules(
      ir1SsaBodyLowerSuccess({
        propositions: [{ kind: "raw", text: "body" }],
        modifiedRules: ["Account_balance"],
      }),
      [
        {
          kind: "rule",
          name: "Account_balance",
          params: [{ name: "a", type: "Account" }],
          returnType: "Nat",
        },
        {
          kind: "rule",
          name: "Account_limit",
          params: [{ name: "a", type: "Account" }],
          returnType: "Nat",
        },
      ],
    );

    assert.equal(result.propositions.length, 2);
    const frame = result.propositions[1];
    assert.equal(frame?.kind, "equation");
    if (frame?.kind !== "equation") {
      assert.fail("expected a frame equation");
    }
    assert.equal(ast.strExpr(frame.lhs), "Account_limit' a");
    assert.equal(ast.strExpr(frame.rhs), "Account_limit a");
    assert.ok(!result.modifiedRules.includes("Account_limit"));
  });

  it("suppresses frame emission when diagnostics are present", () => {
    const result = appendFramesForUnmodifiedRules(
      ir1SsaBodyLowerUnsupported("unsupported path", {
        propositions: [{ kind: "raw", text: "body" }],
      }),
      [
        {
          kind: "rule",
          name: "Account_limit",
          params: [{ name: "a", type: "Account" }],
          returnType: "Nat",
        },
      ],
    );

    assert.deepEqual(result.propositions, [{ kind: "raw", text: "body" }]);
  });

  it("adapts scalar, collection, and loop summary helpers into the unified shape", () => {
    const scalar = adaptScalarSsaLowerResult(
      lowerScalarSsaToProps(
        ir1Assign(ir1Member(ir1Var("a"), "Account_balance"), ir1LitNat(1)),
      ),
    );
    const collection = adaptCollectionSsaLowerResult(
      lowerCollectionSsaToProps(
        ir1MapSet(
          "Cache_value",
          "Cache_hasKey",
          "Owner",
          "Key",
          ir1Var("cache"),
          ir1Var("key"),
          ir1LitNat(7),
        ),
      ),
    );
    const loop = adaptLoopSummaryLowerResult(
      lowerForeachShapeASummaries({
        binder: "item0",
        source: getAst().var("items"),
        body: ir1Assign(
          ir1Member(ir1Var("item0"), "Item_seen"),
          ir1LitBool(true),
        ),
      }),
    );

    assert.equal(scalar.programs.length, 1);
    assert.deepEqual(scalar.modifiedRules, ["Account_balance"]);
    assert.equal(scalar.finalProperties?.length, 1);
    assert.equal(scalar.diagnostics.length, 0);

    assert.equal(collection.programs.length, 1);
    assert.deepEqual(
      new Set(collection.modifiedRules),
      new Set(["Cache_value", "Cache_hasKey"]),
    );
    assert.equal(collection.finalProperties?.length, 0);
    assert.equal(collection.diagnostics.length, 0);

    assert.equal(loop.programs.length, 1);
    assert.deepEqual(loop.modifiedRules, ["Item_seen"]);
    assert.equal(loop.finalProperties, undefined);
    assert.equal(loop.diagnostics.length, 0);
  });

  it("scalar properties lower through one result", () => {
    const ast = getAst();
    const stmt = ir1Block([
      ir1Assign(ir1Member(ir1Var("account"), "Account_balance"), ir1LitNat(1)),
      ir1Assign(ir1Member(ir1Var("account"), "Account_balance"), ir1LitNat(2)),
    ]);
    const result = lowerScalarL1BodyToSsaResult(stmt, { applyConst: (e) => e });

    assert.deepEqual(result.diagnostics, []);
    assert.deepEqual(result.modifiedRules, ["Account_balance"]);
    assert.equal(result.finalProperties?.length, 1);
    assert.equal(result.propositions.length, 1);
    const [equation] = result.propositions;
    assert.equal(equation?.kind, "equation");
    if (equation?.kind !== "equation") {
      assert.fail("expected scalar final equation");
    }
    assert.equal(ast.strExpr(equation.lhs), "Account_balance' account");
    assert.equal(ast.strExpr(equation.rhs), "2");
  });

  it("collections lower value and membership props through one result", () => {
    const ast = getAst();
    const result = lowerCollectionL1BodyToSsaResult(
      ir1MapSet(
        "Cache_value",
        "Cache_hasKey",
        "Owner",
        "Key",
        ir1Var("cache"),
        ir1Var("key"),
        ir1LitNat(7),
      ),
      { applyConst: (e) => e },
    );

    assert.deepEqual(result.diagnostics, []);
    assert.deepEqual(
      new Set(result.modifiedRules),
      new Set(["Cache_value", "Cache_hasKey"]),
    );
    assert.equal(result.propositions.length, 2);
    const lowered = result.propositions
      .filter((p) => p.kind === "equation")
      .map((p) => ast.strExpr(p.lhs));
    assert.ok(lowered.some((lhs) => lhs.startsWith("Cache_hasKey'")));
    assert.ok(lowered.some((lhs) => lhs.startsWith("Cache_value'")));
  });

  it("production scalar SSA emission returns final properties without staged state", () => {
    const result = lowerScalarL1BodyToSsaResult(
      ir1Block([
        ir1Assign(
          ir1Member(ir1Var("account"), "Account_balance"),
          ir1LitNat(1),
        ),
        ir1Assign(
          ir1Member(ir1Var("account"), "Account_balance"),
          ir1LitNat(2),
        ),
      ]),
      { applyConst: (e) => e },
    );
    assert.deepEqual(result.diagnostics, []);
    assert.deepEqual(result.modifiedRules, ["Account_balance"]);
    assert.equal(result.finalProperties?.length, 1);
    assert.equal(result.propositions.length, 1);
  });

  it("preserves helper diagnostics through adapters", () => {
    const scalar = adaptScalarSsaLowerResult(
      lowerScalarSsaToProps({
        kind: "expr-stmt",
        expr: ir1LitBool(true),
      }),
    );
    const location = ir1SsaPropertyLocation(
      "Account_balance",
      ir1Var("a"),
      "balance",
    );
    const loop = adaptLoopSummaryLowerResult({
      program: {
        reads: [],
        writes: [],
        joins: [],
        loopSummaries: [],
        loopHeaderJoins: [],
        loopBodies: [],
        declaredRules: ["Account_balance"],
        modifiedRules: [],
        framedRules: ["Account_balance"],
      },
      summary: null,
      propositions: [],
      modifiedRules: [],
      diagnostics: [
        { kind: "unsupported", reason: "general loops are not supported" },
      ],
    });

    assert.deepEqual(scalar.diagnostics, [
      {
        kind: "unsupported",
        reason: "statement is not supported by scalar SSA lowering",
      },
    ]);
    assert.equal(location.kind, "property");
    assert.deepEqual(loop.diagnostics, [
      { kind: "unsupported", reason: "general loops are not supported" },
    ]);
  });

  it("loop summaries report propositions and modified rules through one result", () => {
    const ast = getAst();
    const shapeA = adaptLoopSummaryLowerResult(
      lowerForeachShapeASummaries({
        binder: "item0",
        source: ast.var("items"),
        body: ir1Assign(
          ir1Member(ir1Var("item0"), "Item_seen"),
          ir1LitBool(true),
        ),
      }),
    );
    const shapeB = adaptLoopSummaryLowerResult(
      lowerForeachShapeBSummaries({
        binder: "item0",
        source: ast.var("items"),
        foldLeaves: [
          {
            target: ir1Var("account"),
            prop: "Account_total",
            combiner: "add",
            outerOp: "add",
            rhs: ir1Member(ir1Var("item0"), "Item_value"),
            guard: ir1Binop(
              "gt",
              ir1Member(ir1Var("item0"), "Item_value"),
              ir1LitNat(0),
            ),
          },
        ],
      }),
    );
    const muSearch = adaptLoopSummaryLowerResult(
      lowerMuSearchSummary({
        location: ir1SsaPropertyLocation(
          "Name_firstUnusedSuffix",
          ir1Var("name"),
          "firstUnusedSuffix",
        ),
        counterType: "Int",
        counterPantName: "i",
        binder: "j1",
        initExpr: ast.litNat(1),
        predicateExpr: ast.binop(ast.opIn(), ast.var("i"), ast.var("used")),
      }),
    );
    const result = combineIR1SsaBodyLowerResults(shapeA, shapeB, muSearch);

    assert.equal(result.diagnostics.length, 0);
    assert.equal(result.propositions.length, 2);
    assert.deepEqual(
      new Set(result.modifiedRules),
      new Set(["Item_seen", "Account_total", "Name_firstUnusedSuffix"]),
    );
    assert.equal(result.programs.length, 3);
    assert.deepEqual(
      result.programs.flatMap((program) =>
        program.loopSummaries.map((summary) => summary.shape),
      ),
      ["foreach-shape-a", "foreach-shape-b", "mu-search"],
    );

    const shapeAProp = result.propositions[0];
    assert.equal(shapeAProp?.kind, "equation");
    if (shapeAProp?.kind === "equation") {
      assert.equal(ast.strExpr(shapeAProp.lhs), "Item_seen' item0");
      assert.equal(ast.strExpr(shapeAProp.rhs), "true");
    }

    const shapeBProp = result.propositions[1];
    assert.equal(shapeBProp?.kind, "equation");
    if (shapeBProp?.kind === "equation") {
      assert.equal(ast.strExpr(shapeBProp.lhs), "Account_total' account");
      assert.equal(
        ast.strExpr(shapeBProp.rhs),
        "Account_total account + (+ over each item0 in items, Item_value item0 > 0 | Item_value item0)",
      );
    }
  });

  describe("returnValue emission", () => {
    it.skip("null returnValue emits no extra equation", () => {
      // PENDING Patch 2.
    });

    it.skip("non-null returnValue emits function-level equation", () => {
      // PENDING Patch 2.
    });

    it.skip("equation ordering is rule-modifying then return-value then frames", () => {
      // PENDING Patch 2.
    });
  });
});
