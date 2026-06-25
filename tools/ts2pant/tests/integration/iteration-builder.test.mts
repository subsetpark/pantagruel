// @archlint.module test
// @archlint.domain ts2pant.iteration-builder.integration

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import {
  buildDocument,
  emitAndCheck,
  runCheck,
  solverAvailable,
} from "../helpers.mjs";

const FIXTURE = resolve(
  import.meta.dirname,
  "../fixtures/constructs/expressions-iteration-builder.ts",
);

const hasSolver = solverAvailable();

async function checkFixture(functionName: string): Promise<void> {
  const output = await emitAndCheck(await buildDocument(FIXTURE, functionName));
  if (!hasSolver) {
    return;
  }
  const result = runCheck(output);
  assert.ok(result.passed, `pant --check failed:\n${result.output}`);
}

describe("iteration builder integration", () => {
  // Patch 2 unskips these list-builder checker stubs.
  it("list builder positives pass pant --check", {
    skip: "Patch 2 implements for-of list builder widening",
  }, async () => {
    await checkFixture("listConstProjection");
    await checkFixture("listCompoundGuard");
    await checkFixture("listNestedGuard");
  });

  // Patch 3 unskips this Set-builder checker stub.
  it("setAddForOf passes pant --check", async () => {
    await checkFixture("setAddForOf");
  });

  // Patch 4 unskips these scalar-fold checker stubs.
  it("scalar fold positives pass pant --check", async () => {
    await checkFixture("sumIntFold");
    await checkFixture("countGuardedFold");
    await checkFixture("allActiveFold");
  });
});
