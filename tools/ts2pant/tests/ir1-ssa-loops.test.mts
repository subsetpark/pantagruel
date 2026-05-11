import assert from "node:assert/strict";
import { describe, it } from "node:test";

import {
  ir1SsaPropertyLocation,
  ir1SsaRuleOfLocation,
  ir1Var,
} from "../src/ir1.js";
import {
  buildLoopSsaProgram,
  lowerForeachSummary,
} from "../src/ir1-ssa-loops.js";

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
    const location = ir1SsaPropertyLocation(
      "Account_total",
      ir1Var("account"),
      "total",
    );
    const proposition = { kind: "raw" as const, text: "accumulator-fold" };

    const result = lowerForeachSummary({
      input: {
        kind: "foreach-shape-b",
        location,
        propositions: [proposition],
      },
      declaredRules: ["Account_limit"],
    });

    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.summary?.shape, "foreach-shape-b");
    assert.equal(result.summary?.location, location);
    assert.equal(result.summary?.summaryVersion.origin, "loop-summary");
    assert.equal(result.summary?.summaryVersion.location, location);
    assert.deepEqual(result.propositions, [proposition]);
    assert.deepEqual(result.program.modifiedRules, [
      ir1SsaRuleOfLocation(location),
    ]);
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

  it.skip("represents supported in-iteration read-after-write without subState mutation", async () => {
    // PENDING Patch 4: cover the currently supported loop-body read semantics
    // so the helper proves read-after-write behavior without executing the
    // body inside a SymbolicState subState.
  });

  it.skip("preserves unsupported diagnostics for out-of-scope loops", async () => {
    // PENDING Patch 2: pin current unsupported boundaries for general loops,
    // proposition-emitting nested loop bodies, and unsupported in-loop
    // collection mutation with clear diagnostic reasons.
  });

  it.skip("preserves production parity for foreach and mu-search fixtures", async () => {
    // PENDING Patch 4: pin parity for `functions-mutating-loop.ts`
    // (for example `forEachActivate`, `forEachSum`, or `mixedUpdates`) once
    // foreach lowering routes through the loop-summary helper.
    // PENDING Patch 3: pin parity for `expressions-while-mu-search.ts`
    // (for example `firstUnusedSuffix`) once mu-search lowering routes through
    // the loop-summary helper.
  });
});
