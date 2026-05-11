import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";

import { createSourceFile } from "../src/extract.js";
import { lowerExpr } from "../src/ir-emit.js";
import {
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1LitNat,
  ir1Member,
  ir1SsaPropertyLocation,
  ir1SsaRuleOfLocation,
  ir1Var,
} from "../src/ir1.js";
import { lowerL1Expr } from "../src/ir1-lower.js";
import {
  buildLoopSsaProgram,
  lowerForeachShapeASummaries,
  lowerForeachShapeBSummaries,
  lowerForeachSummary,
  lowerMuSearchSummary,
} from "../src/ir1-ssa-loops.js";
import { assertWasmTypeChecks, getAst, loadAst } from "../src/pant-wasm.js";
import {
  buildDocumentFromSourceFile,
  containsUnsupportedLine,
  emitDocument,
} from "./helpers.mts";

before(async () => {
  await loadAst();
});

describe("ir1-ssa-loops", () => {
  // PENDING Patch 2: introduce the dedicated loop-summary SSA helper and
  // summary/version bookkeeping for supported loop-like constructs.
  // PENDING Patch 3: route canonical mu-search lowering through loop summaries.
  // PENDING Patch 4: route foreach Shape A lowering through loop summaries.
  // PENDING Patch 5: route foreach Shape B lowering through loop summaries.

  it("records canonical mu-search as a loop summary", () => {
    const location = ir1SsaPropertyLocation(
      "Name_firstUnusedSuffix",
      ir1Var("name"),
      "firstUnusedSuffix",
    );

    const result = buildLoopSsaProgram({
      kind: "mu-search",
      location,
    });

    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.program.loopSummaries.length, 1);
    assert.deepEqual(result.program.modifiedRules, [
      ir1SsaRuleOfLocation(location),
    ]);

    const [summary] = result.program.loopSummaries;
    assert.equal(summary!.shape, "mu-search");
    assert.equal(summary!.location, location);
    assert.equal(summary!.summaryVersion.origin, "loop-summary");
    assert.equal(summary!.summaryVersion.location, location);
  });

  it("lowers mu-search summaries to the existing min-over-each shape", () => {
    const location = ir1SsaPropertyLocation(
      "Name_firstUnusedSuffix",
      ir1Var("name"),
      "firstUnusedSuffix",
    );

    const result = lowerMuSearchSummary({
      location,
      counterType: "Int",
      counterPantName: "i",
      binder: "j1",
      initExpr: lowerExpr(lowerL1Expr(ir1LitNat(1))),
      predicateExpr: lowerExpr(
        lowerL1Expr(ir1Binop("in", ir1Var("i"), ir1Var("used"))),
      ),
    });

    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.summary?.shape, "mu-search");
    assert.equal(result.summary?.location, location);
    assert.equal(result.summary?.summaryVersion.location, location);
    assert.ok(result.loweredExpr !== null);
    assert.equal(
      getAst().strExpr(result.loweredExpr!),
      "min over each j1: Int, j1 >= 1, ~(j1 in used) | j1",
    );
  });

  it("records foreach Shape A quantified writes as loop summaries", () => {
    const location = ir1SsaPropertyLocation(
      "Item_seen",
      ir1Var("$item"),
      "seen",
    );
    const proposition = { kind: "raw" as const, text: "quantified-write" };

    const result = lowerForeachSummary({
      input: {
        kind: "foreach-shape-a",
        location,
        propositions: [proposition],
      },
    });

    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.summary?.shape, "foreach-shape-a");
    assert.equal(result.summary?.location, location);
    assert.equal(result.summary?.summaryVersion.origin, "loop-summary");
    assert.equal(result.summary?.summaryVersion.location, location);
    assert.deepEqual(result.propositions, [proposition]);
    assert.deepEqual(result.modifiedRules, [ir1SsaRuleOfLocation(location)]);
  });

  it("records foreach Shape B accumulator folds as loop summaries", () => {
    const ast = getAst();
    const account = ir1Var("account");
    const item = ir1Var("$item");
    const value = ir1Member(item, "Item_value");
    const result = lowerForeachShapeBSummaries({
      binder: "$item",
      source: ast.var("items"),
      foldLeaves: [
        {
          target: account,
          prop: "Account_total",
          combiner: "add",
          outerOp: "add",
          rhs: value,
          guard: ir1Binop("gt", value, ir1LitNat(0)),
        },
      ],
      priorAccumulatorValues: new Map([
        ["Account_total::account", ast.litNat(10)],
      ]),
      declaredRules: ["Account_limit"],
    });

    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.summaries.length, 1);
    const [summary] = result.summaries;
    assert.equal(summary!.shape, "foreach-shape-b");
    assert.equal(summary!.location.ruleName, "Account_total");
    assert.equal(summary!.summaryVersion.origin, "loop-summary");
    assert.equal(summary!.summaryVersion.location, summary!.location);
    assert.equal(result.propositions.length, 1);
    const [prop] = result.propositions;
    assert.equal(prop!.kind, "equation");
    if (prop?.kind === "equation") {
      assert.equal(ast.strExpr(prop.lhs), "Account_total' account");
      assert.match(
        ast.strExpr(prop.rhs),
        /^10 \+ \(\+ over each \$item in items, Item_value \$item > 0 \| Item_value \$item\)$/u,
      );
    }
    assert.deepEqual(result.program.modifiedRules, ["Account_total"]);
    assert.deepEqual(
      new Set(result.program.declaredRules),
      new Set(["Account_total", "Account_limit"]),
    );
    assert.deepEqual(result.program.framedRules, ["Account_limit"]);
  });

  it("creates distinct summary versions and tracks modified rules", () => {
    const shapeALocation = ir1SsaPropertyLocation(
      "Item_seen",
      ir1Var("$item"),
      "seen",
    );
    const shapeBLocation = ir1SsaPropertyLocation(
      "Account_total",
      ir1Var("account"),
      "total",
    );

    const result = buildLoopSsaProgram(
      [
        { kind: "foreach-shape-a", location: shapeALocation },
        { kind: "foreach-shape-b", location: shapeBLocation },
      ],
      { declaredRules: ["Account_limit"] },
    );

    const [shapeA, shapeB] = result.program.loopSummaries;
    assert.notEqual(shapeA!.summaryVersion, shapeB!.summaryVersion);
    assert.notEqual(shapeA!.summaryVersion.id, shapeB!.summaryVersion.id);
    assert.equal(shapeA!.summaryVersion.location, shapeALocation);
    assert.equal(shapeB!.summaryVersion.location, shapeBLocation);
    assert.deepEqual(
      new Set(result.program.modifiedRules),
      new Set(["Item_seen", "Account_total"]),
    );
    assert.deepEqual(
      new Set(result.program.declaredRules),
      new Set(["Item_seen", "Account_total", "Account_limit"]),
    );
    assert.deepEqual(result.program.framedRules, ["Account_limit"]);
  });

  it("reports unsupported loop summary inputs without summaries", () => {
    const result = buildLoopSsaProgram(
      {
        kind: "unsupported",
        reason: "general loops are not supported by loop-summary SSA",
      },
      { declaredRules: ["Account_total"] },
    );

    assert.equal(result.program.loopSummaries.length, 0);
    assert.deepEqual(result.program.modifiedRules, []);
    assert.deepEqual(result.program.framedRules, ["Account_total"]);
    assert.deepEqual(result.diagnostics, [
      {
        kind: "unsupported",
        reason: "general loops are not supported by loop-summary SSA",
      },
    ]);
  });

  it("represents supported in-iteration read-after-write without subState mutation", () => {
    const item = ir1Var("$item");
    const value = ir1Member(item, "Item_value");
    const score = ir1Member(item, "Item_score");
    const result = lowerForeachShapeASummaries({
      binder: "$item",
      source: getAst().var("items"),
      body: ir1Block([
        ir1Assign(value, ir1Binop("add", value, ir1LitNat(1))),
        ir1Assign(score, value),
      ]),
    });

    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.summaries.length, 2);
    assert.deepEqual(
      result.summaries.map((s) => s.shape),
      ["foreach-shape-a", "foreach-shape-a"],
    );
    assert.deepEqual(
      new Set(result.modifiedRules),
      new Set(["Item_value", "Item_score"]),
    );

    const ast = getAst();
    const scoreEq = result.propositions.find(
      (p) =>
        p.kind === "equation" && ast.strExpr(p.lhs) === "Item_score' $item",
    );
    assert.ok(scoreEq, "expected a quantified score write");
    if (scoreEq?.kind === "equation") {
      assert.equal(ast.strExpr(scoreEq.rhs), "Item_value $item + 1");
      assert.equal(scoreEq.guards?.length, 1);
      assert.equal(
        ast.strExpr(ast.forall([], scoreEq.guards ?? [], ast.var("__body__"))),
        "all $item in items | __body__",
      );
    }
  });

  it("preserves unsupported diagnostics for out-of-scope loops", () => {
    const result = buildLoopSsaProgram([
      {
        kind: "unsupported",
        reason: "general loops are not supported by loop-summary SSA",
      },
      {
        kind: "unsupported",
        reason:
          "nested proposition-emitting loop body is not supported — the inner proposition would escape the outer iterator scope",
      },
      {
        kind: "unsupported",
        reason: "Map mutation inside foreach body is out of scope",
      },
    ]);

    assert.equal(result.program.loopSummaries.length, 0);
    assert.deepEqual(result.program.modifiedRules, []);
    assert.deepEqual(
      result.diagnostics.map((d) => d.reason),
      [
        "general loops are not supported by loop-summary SSA",
        "nested proposition-emitting loop body is not supported — the inner proposition would escape the outer iterator scope",
        "Map mutation inside foreach body is out of scope",
      ],
    );
  });

  it("preserves production parity for foreach and mu-search fixtures", async () => {
    const constructsDir = resolve(import.meta.dirname, "fixtures/constructs");
    const cases = [
      {
        file: "functions-mutating-loop.ts",
        functionName: "sumAmounts",
        expected:
          /account--total' a = account--total a \+ \(\+ over each \w+ in items \| item--value \w+\)\./u,
      },
      {
        file: "functions-mutating-loop.ts",
        functionName: "sumPositive",
        expected:
          /account--total' a = account--total a \+ \(\+ over each \w+ in items, item--value \w+ > 0 \| item--value \w+\)\./u,
      },
      {
        file: "functions-mutating-loop.ts",
        functionName: "mixedUpdates",
        expected: /item--tagged' \w+ = true\./u,
      },
      {
        file: "functions-mutating-loop.ts",
        functionName: "forEachSum",
        expected:
          /account--total' a = account--total a \+ \(\+ over each \w+ in items \| item--value \w+\)\./u,
      },
      {
        file: "functions-mutating-loop.ts",
        functionName: "sumIntoAnon",
        expected:
          /total-rec--total' acc = total-rec--total acc \+ \(\+ over each \w+ in items \| \w+\)\./u,
      },
      {
        file: "expressions-while-mu-search.ts",
        functionName: "firstUnusedSuffix",
        expected: /min over each/u,
      },
    ];

    for (const { file, functionName, expected } of cases) {
      const sourceFile = createSourceFile(resolve(constructsDir, file));
      const doc = await buildDocumentFromSourceFile(sourceFile, functionName);
      const output = emitDocument(doc);
      assert.ok(
        !containsUnsupportedLine(output),
        `${file} > ${functionName} should stay supported`,
      );
      assert.match(output, expected, `${file} > ${functionName}`);
      await assertWasmTypeChecks(output, doc.bundleModules);
    }
  });
});
