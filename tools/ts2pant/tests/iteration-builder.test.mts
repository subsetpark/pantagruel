// @archlint.module test
// @archlint.domain ts2pant.iteration-builder

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { buildDocument, emitAndCheck } from "./helpers.mjs";

const FIXTURE = resolve(
  import.meta.dirname,
  "fixtures/constructs/expressions-iteration-builder.ts",
);

async function emitFixture(functionName: string): Promise<string> {
  return emitAndCheck(await buildDocument(FIXTURE, functionName));
}

describe("iteration builder", () => {
  // Patch 2 unskips this list-builder widening case.
  it("listConstProjection inlines loop-local const into the comprehension", async () => {
    const output = await emitFixture("listConstProjection");
    assert.match(
      output,
      /^list-const-projection items = \(each item in items \| iteration-item--label item\)\.$/mu,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  // Patch 2 unskips this list-builder widening case.
  it("listCompoundGuard folds both conjuncts into guards", async () => {
    const output = await emitFixture("listCompoundGuard");
    assert.match(
      output,
      /^list-compound-guard items = \(each item in items, iteration-item--active item, iteration-item--ready item \| item\)\.$/mu,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  // Patch 2 unskips this list-builder widening case.
  it("listNestedGuard folds nested if guards into the comprehension", async () => {
    const output = await emitFixture("listNestedGuard");
    assert.match(
      output,
      /^list-nested-guard items = \(each item in items, iteration-item--active item, iteration-item--ready item \| iteration-item--id item\)\.$/mu,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  // Patch 3 unskips this Set-builder case.
  it("setAddForOf emits membership equivalence over a some-comprehension", async () => {
    const output = await emitFixture("setAddForOf");
    assert.match(
      output,
      /x in set-add-for-of items <-> \(some (n[0-9]*) in items \| x = iteration-item--id \1\)/u,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  // Patch 4 unskips this scalar-fold case.
  it("sumIntFold lowers to init plus a sum comprehension", async () => {
    const output = await emitFixture("sumIntFold");
    assert.match(
      output,
      /^sum-int-fold items = \(\+ over each item in items \| iteration-item--count item\)\.$/mu,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  // Patch 4 unskips this scalar-fold case.
  it("countGuardedFold folds the guard into the sum comprehension", async () => {
    const output = await emitFixture("countGuardedFold");
    assert.match(
      output,
      /^count-guarded-fold items = \(\+ over each item in items, iteration-item--active item \| 1\)\.$/mu,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  // Patch 4 unskips this scalar-fold case.
  it("allActiveFold lowers to an and comprehension", async () => {
    const output = await emitFixture("allActiveFold");
    assert.match(
      output,
      /^all-active-fold items = \(and over each item in items \| iteration-item--active item\)\.$/mu,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  // Patch 2 unskips this preserved list-builder rejection.
  it("listEarlyReturnInLoopRejected remains unsupported", async () => {
    const output = await emitFixture("listEarlyReturnInLoopRejected");
    assert.match(
      output,
      /^> UNSUPPORTED: list-early-return-in-loop-rejected/mu,
    );
    assert.match(output, /for-of|return|loop/u);
  });

  // Patch 2 unskips this preserved list-builder rejection.
  it("listBreakRejected remains unsupported", async () => {
    const output = await emitFixture("listBreakRejected");
    assert.match(output, /^> UNSUPPORTED: list-break-rejected/mu);
    assert.match(output, /for-of|break|loop/u);
  });

  // Patch 2 unskips this preserved list-builder rejection.
  it("listNestedLoopRejected remains unsupported", async () => {
    const output = await emitFixture("listNestedLoopRejected");
    assert.match(output, /^> UNSUPPORTED: list-nested-loop-rejected/mu);
    assert.match(output, /for-of|nested|loop/u);
  });

  // Patch 2 unskips this preserved list-builder rejection.
  it("listAccumulatorAliasRejected remains unsupported", async () => {
    const output = await emitFixture("listAccumulatorAliasRejected");
    assert.match(output, /^> UNSUPPORTED: list-accumulator-alias-rejected/mu);
    assert.match(output, /alias|accumulator|builder/u);
  });

  // Patch 3 unskips this preserved Map-builder rejection.
  it("mapBuilderForOfRejected remains unsupported", async () => {
    const output = await emitFixture("mapBuilderForOfRejected");
    assert.match(output, /^> UNSUPPORTED: map-builder-for-of-rejected/mu);
    assert.match(output, /Map|map|builder/u);
  });

  // Patch 3 unskips this preserved Set-builder rejection.
  it("setDeleteForOfRejected remains unsupported", async () => {
    const output = await emitFixture("setDeleteForOfRejected");
    assert.match(output, /^> UNSUPPORTED: set-delete-for-of-rejected/mu);
    assert.match(output, /Set|delete|builder/u);
  });

  it("setAccumulatorAliasForOfRejected remains unsupported", async () => {
    const output = await emitFixture("setAccumulatorAliasForOfRejected");
    assert.match(
      output,
      /^> UNSUPPORTED: set-accumulator-alias-for-of-rejected/mu,
    );
    assert.match(output, /alias|escape|accumulator|builder/u);
  });

  it("setAccumulatorReadForOfRejected remains unsupported", async () => {
    const output = await emitFixture("setAccumulatorReadForOfRejected");
    assert.match(
      output,
      /^> UNSUPPORTED: set-accumulator-read-for-of-rejected/mu,
    );
    assert.match(output, /read the accumulator/u);
  });

  it("setTdzSourceForOfRejected remains unsupported", async () => {
    const output = await emitFixture("setTdzSourceForOfRejected");
    assert.match(output, /^> UNSUPPORTED: set-tdz-source-for-of-rejected/mu);
    assert.match(output, /later binding/u);
  });

  // Patch 4 unskips this preserved scalar-fold rejection.
  it("conjoinNoIdentityRejected remains unsupported", async () => {
    const output = await emitFixture("conjoinNoIdentityRejected");
    assert.match(output, /^> UNSUPPORTED: conjoin-no-identity-rejected/mu);
    assert.match(output, /identity|null|fold|unsupported/u);
  });

  // Patch 4 unskips this preserved scalar-fold rejection.
  it("foldNonCommutativeRejected remains unsupported", async () => {
    const output = await emitFixture("foldNonCommutativeRejected");
    assert.match(output, /^> UNSUPPORTED: fold-non-commutative-rejected/mu);
    assert.match(output, /commutative|fold|unsupported/u);
  });
});
