import { describe, it } from "node:test";

describe("ir1-ssa-loops", () => {
  // PENDING Patch 2: introduce the dedicated loop-summary SSA helper and
  // summary/version bookkeeping for supported loop-like constructs.
  // PENDING Patch 3: route canonical mu-search lowering through loop summaries.
  // PENDING Patch 4: route foreach Shape A lowering through loop summaries.
  // PENDING Patch 5: route foreach Shape B lowering through loop summaries.

  it.skip("records canonical mu-search as a loop summary", async () => {
    // PENDING Patch 3: cover a canonical let/while/increment recognizer path
    // and assert it records a `mu-search` loop summary before lowering to the
    // same `min over each` expression emitted today.
  });

  it.skip(
    "records foreach Shape A quantified writes as loop summaries",
    async () => {
      // PENDING Patch 4: cover a foreach iterator-write fixture and assert the
      // helper records a `foreach-shape-a` summary whose lowering still emits
      // the current quantified write proposition shape.
    },
  );

  it.skip(
    "records foreach Shape B accumulator folds as loop summaries",
    async () => {
      // PENDING Patch 5: cover a foreach accumulator fixture and assert the
      // helper records a `foreach-shape-b` summary whose lowering still emits
      // the current accumulator-fold proposition shape.
    },
  );

  it.skip(
    "represents supported in-iteration read-after-write without subState mutation",
    async () => {
      // PENDING Patch 4: cover the currently supported loop-body read semantics
      // so the helper proves read-after-write behavior without executing the
      // body inside a SymbolicState subState.
    },
  );

  it.skip(
    "preserves unsupported diagnostics for out-of-scope loops",
    async () => {
      // PENDING Patch 2: pin current unsupported boundaries for general loops,
      // proposition-emitting nested loop bodies, and unsupported in-loop
      // collection mutation with clear diagnostic reasons.
    },
  );

  it.skip("preserves production parity for foreach and mu-search fixtures", async () => {
    // PENDING Patch 4: pin parity for `functions-mutating-loop.ts`
    // (for example `forEachActivate`, `forEachSum`, or `mixedUpdates`) once
    // foreach lowering routes through the loop-summary helper.
    // PENDING Patch 3: pin parity for `expressions-while-mu-search.ts`
    // (for example `firstUnusedSuffix`) once mu-search lowering routes through
    // the loop-summary helper.
  });
});
