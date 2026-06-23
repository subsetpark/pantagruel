// @archlint.module test
// @archlint.domain ts2pant.local-collection-builder

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { buildDocument, emitAndCheck } from "./helpers.mjs";

const FIXTURE = resolve(
  import.meta.dirname,
  "fixtures/constructs/expressions-local-collection-builder.ts",
);

async function emitFixture(functionName: string): Promise<string> {
  return emitAndCheck(await buildDocument(FIXTURE, functionName));
}

describe("local collection builder", () => {
  it("listSinglePush emits finite ordered list assertions", async () => {
    const output = await emitFixture("listSinglePush");
    assert.match(output, /^#\(list-single-push seed\) = 1\.$/mu);
    assert.match(output, /^\(list-single-push seed\) 1 = seed\.$/mu);
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("listMultiplePushes preserves push order", async () => {
    const output = await emitFixture("listMultiplePushes");
    assert.match(output, /^#\(list-multiple-pushes first second\) = 2\.$/mu);
    assert.match(
      output,
      /^\(list-multiple-pushes first second\) 1 = first\.$/mu,
    );
    assert.match(
      output,
      /^\(list-multiple-pushes first second\) 2 = second\.$/mu,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("listPushConstProjection lowers const-bound pushed values", async () => {
    const output = await emitFixture("listPushConstProjection");
    assert.match(output, /^projected\s+=> String\.$/mu);
    assert.match(output, /^projected = builder-item--label item\.$/mu);
    assert.match(
      output,
      /^\(list-push-const-projection item\) 1 = projected\.$/mu,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("listParenthesizedReturn unwraps the returned accumulator", async () => {
    const output = await emitFixture("listParenthesizedReturn");
    assert.match(output, /^#\(list-parenthesized-return seed\) = 1\.$/mu);
    assert.match(output, /^\(list-parenthesized-return seed\) 1 = seed\.$/mu);
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("setAddBuilder emits membership equivalence", async () => {
    const output = await emitFixture("setAddBuilder");
    assert.match(
      output,
      /x in set-add-builder first second <-> x = first or x = second/u,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("setUntypedAccumulator uses the function return element type", async () => {
    const output = await emitFixture("setUntypedAccumulator");
    assert.match(output, /all x: String \|/u);
    assert.match(output, /x in set-untyped-accumulator seed <-> x = seed/u);
    assert.doesNotMatch(output, /all x: Opaque/u);
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("setBinderCollision avoids capturing parameter names", async () => {
    const output = await emitFixture("setBinderCollision");
    assert.match(
      output,
      /all x[0-9]+: String \| x[0-9]+ in set-binder-collision x <-> x[0-9]+ = x/u,
    );
    assert.doesNotMatch(
      output,
      /all x: String \| x in set-binder-collision x <-> x = x/u,
    );
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("setEmptyBuilder emits empty membership assertion", async () => {
    const output = await emitFixture("setEmptyBuilder");
    assert.match(output, /all x: String \| ~\(x in set-empty-builder\)\./u);
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("setIterableRejected remains unsupported", async () => {
    const output = await emitFixture("setIterableRejected");
    assert.match(output, /^> UNSUPPORTED: set-iterable-rejected/mu);
    assert.match(output, /Set builder from iterable/u);
  });

  it("setDeleteRejected remains unsupported", async () => {
    const output = await emitFixture("setDeleteRejected");
    assert.match(output, /^> UNSUPPORTED: set-delete-rejected/mu);
    assert.match(output, /Set builder mutation \.delete/u);
  });

  it("listAliasEscapeRejected remains unsupported", async () => {
    const output = await emitFixture("listAliasEscapeRejected");
    assert.match(output, /^> UNSUPPORTED: list-alias-escape-rejected/mu);
    assert.match(output, /alias|escape|builder/u);
  });

  it("listUnknownMutationRejected remains unsupported", async () => {
    const output = await emitFixture("listUnknownMutationRejected");
    assert.match(output, /^> UNSUPPORTED: list-unknown-mutation-rejected/mu);
    assert.match(output, /unknown|mutation|builder|unshift/u);
  });

  it("listGuardReadRejected remains unsupported", async () => {
    const output = await emitFixture("listGuardReadRejected");
    assert.match(output, /^> UNSUPPORTED: list-guard-read-rejected/mu);
    assert.match(output, /guard|read|accumulator|builder/u);
  });

  it("mapBuilderRejected remains unsupported", async () => {
    const output = await emitFixture("mapBuilderRejected");
    assert.match(output, /^> UNSUPPORTED: map-builder-rejected/mu);
    assert.match(output, /Map|map|builder/u);
  });

  it("mapFinalMutationRejected remains unsupported", async () => {
    const output = await emitFixture("mapFinalMutationRejected");
    assert.match(output, /^> UNSUPPORTED: map-final-mutation-rejected/mu);
    assert.match(
      output,
      /Map builder construction is not supported in this milestone/u,
    );
  });
});
