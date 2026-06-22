// @archlint.module test
// @archlint.domain ts2pant.for-of-comprehension.integration

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";
import { loadAst } from "../../src/pant-wasm.js";
import { buildDocument, emitAndCheck, runCheck } from "../helpers.mjs";

const FIXTURE = resolve(
  import.meta.dirname,
  "../fixtures/constructs/expressions-for-of-comprehension.ts",
);

before(async () => {
  await loadAst();
});

describe("for-of comprehension integration", () => {
  it("@pant on a build-list entails", {
    skip: "PENDING: Patch 3",
  }, async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "collectSet"),
    );
    const result = runCheck(output);
    const entailments = result.checks.filter(
      (c) =>
        c.message.startsWith("OK: Entailed:") ||
        c.message.startsWith("FAIL: Not entailed:"),
    );
    assert.ok(entailments.length >= 1, "expected an entailment result");
    assert.ok(result.passed, `pant --check failed:\n${result.output}`);
    assert.ok(entailments.every((c) => c.passed));
  });
});
