// @archlint.module test
// @archlint.domain ts2pant.local-collection-builder

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { buildDocument, emitAndCheck, runCheck } from "./helpers.mjs";

const FIXTURE = resolve(
  import.meta.dirname,
  "fixtures/constructs/expressions-local-collection-builder.ts",
);

async function emitFixture(functionName: string): Promise<string> {
  return emitAndCheck(await buildDocument(FIXTURE, functionName));
}

describe("local collection builder", () => {
  it(
    "listSinglePush emits finite ordered list assertions",
    {
      skip:
        "Patch 2 will unskip after finite ordered list builder lowering lands",
    },
    async () => {
      const output = await emitFixture("listSinglePush");
      assert.match(output, /^#\(list-single-push seed\) = 1\.$/mu);
      assert.match(output, /^\(list-single-push seed\) 1 = seed\.$/mu);
      assert.doesNotMatch(output, /UNSUPPORTED/u);
      assert.ok(runCheck(output).passed);
    },
  );

  it(
    "listMultiplePushes preserves push order",
    {
      skip:
        "Patch 2 will unskip after finite ordered list builder lowering lands",
    },
    async () => {
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
      assert.ok(runCheck(output).passed);
    },
  );

  it(
    "listPushConstProjection lowers const-bound pushed values",
    {
      skip:
        "Patch 2 will unskip after const-bound list builder values are lowered",
    },
    async () => {
      const output = await emitFixture("listPushConstProjection");
      assert.match(output, /^projected\s+=> String\.$/mu);
      assert.match(output, /^projected = builder-item--label item\.$/mu);
      assert.match(
        output,
        /^\(list-push-const-projection item\) 1 = projected\.$/mu,
      );
      assert.doesNotMatch(output, /UNSUPPORTED/u);
      assert.ok(runCheck(output).passed);
    },
  );

  it(
    "setAddBuilder emits membership equivalence",
    {
      skip: "Patch 3 will unskip after Set add builder lowering lands",
    },
    async () => {
      const output = await emitFixture("setAddBuilder");
      assert.match(
        output,
        /x in set-add-builder first second <-> \(x = first or x = second\)/u,
      );
      assert.doesNotMatch(output, /UNSUPPORTED/u);
      assert.ok(runCheck(output).passed);
    },
  );

  it(
    "listAliasEscapeRejected remains unsupported",
    {
      skip:
        "Patch 2 will unskip after list builder alias rejection is diagnosed",
    },
    async () => {
      const output = await emitFixture("listAliasEscapeRejected");
      assert.match(output, /^> UNSUPPORTED: list-alias-escape-rejected/mu);
      assert.match(output, /alias|escape|builder/u);
    },
  );

  it(
    "listUnknownMutationRejected remains unsupported",
    {
      skip:
        "Patch 2 will unskip after unknown list mutation rejection is diagnosed",
    },
    async () => {
      const output = await emitFixture("listUnknownMutationRejected");
      assert.match(
        output,
        /^> UNSUPPORTED: list-unknown-mutation-rejected/mu,
      );
      assert.match(output, /unknown|mutation|builder|unshift/u);
    },
  );

  it(
    "mapBuilderRejected remains unsupported",
    {
      skip: "Patch 3 will unskip after Map builder rejection is preserved",
    },
    async () => {
      const output = await emitFixture("mapBuilderRejected");
      assert.match(output, /^> UNSUPPORTED: map-builder-rejected/mu);
      assert.match(output, /Map|map|builder/u);
    },
  );
});
