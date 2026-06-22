// @archlint.module test
// @archlint.domain ts2pant.for-of-comprehension

import { resolve } from "node:path";
import { describe, it } from "node:test";
import {
  buildDocument,
  emitAndCheck,
  runCheck,
  solverAvailable,
} from "./helpers.mjs";

const FIXTURE = resolve(
  import.meta.dirname,
  "fixtures/constructs/expressions-for-of-comprehension.ts",
);

const PATCH_2_SKIP = "PENDING: Patch 2";
const PATCH_3_SKIP = "PENDING: Patch 3";

describe("for-of comprehension recognizer", () => {
  it("matches map/filter build-list and rejects out-of-scope shapes", {
    skip: PATCH_2_SKIP,
  }, async () => {
    await buildDocument(FIXTURE, "_mapLabels");
  });
});

describe("for-of comprehension integration", () => {
  const hasSolver = solverAvailable();
  const patch3Skip = hasSolver
    ? PATCH_3_SKIP
    : `${PATCH_3_SKIP} (z3 unavailable)`;

  it("translates a build-list to an each comprehension", {
    skip: patch3Skip,
  }, async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "_mapLabels"),
    );
    void output;
  });

  it("filtered build-list emits a guarded each", {
    skip: patch3Skip,
  }, async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "_filterIfActive"),
    );
    void output;
  });

  it("@pant on a build-list entails", { skip: patch3Skip }, async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "_collectSet"),
    );
    const result = runCheck(output);
    void result;
  });

  it("existing snapshots are unchanged (additive)", {
    skip: patch3Skip,
  }, async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "_filterContinueActive"),
    );
    void output;
  });

  it("out-of-scope scalar fold still refuses with a precise reason", {
    skip: patch3Skip,
  }, async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "_sumLengths"),
    );
    void output;
  });

  it("Map-entry destructuring still refuses with a precise reason", {
    skip: patch3Skip,
  }, async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "_mapEntryCopy"),
    );
    void output;
  });
});
