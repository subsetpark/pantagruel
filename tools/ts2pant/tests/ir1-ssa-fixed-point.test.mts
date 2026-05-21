import assert from "node:assert/strict";
import { describe, it } from "node:test";

describe("ir1-ssa-fixed-point", () => {
  it.skip("recognizes a single-location while-loop fixed-point shape", () => {
    // PENDING Patch 3: implement fixed-point while-loop shape recognition.
  });

  it.skip("rejects literal-true guards at the recognizer level", () => {
    // PENDING Patch 3: implement literal-true guard rejection.
  });

  it.skip("rejects multi-location while bodies", () => {
    // PENDING Patch 3: implement multi-location while body rejection.
  });

  it.skip("constructs IR1SsaLoopBody with terminationMetric: null", () => {
    // PENDING Patch 3: construct fixed-point loop bodies with null metrics.
  });

  it.skip("synthesises a uniquely-named recursive helper rule via NameRegistry", () => {
    // PENDING Patch 3: synthesise recursive helper names through NameRegistry.
  });

  it.skip("emits a rule-decl PropResult for the helper and an equation PropResult for the caller", () => {
    // PENDING Patch 3: emit helper rule-decl and caller equation PropResults.
  });
});
