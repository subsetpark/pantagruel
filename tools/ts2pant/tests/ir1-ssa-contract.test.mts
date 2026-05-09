import { describe, it } from "node:test";

describe("ir1-ssa-contract", () => {
  it.skip("allocates opaque versions with location metadata", () => {
    // PENDING: Patch 2 implements opaque version allocation + location metadata.
  });

  it.skip(
    "simplifies degenerate joins before constructing join nodes",
    () => {
      // PENDING: Patch 2 implements join construction and simplification.
    },
  );

  it.skip(
    "models Map state as coordinated value and membership locations",
    () => {
      // PENDING: Patch 2 implements coordinated Map SSA locations.
    },
  );

  it.skip("models Set clear inside membership SSA value", () => {
    // PENDING: Patch 2 implements Set membership SSA value construction.
  });

  it.skip("exposes distinct loop-summary versions", () => {
    // PENDING: Patch 2 implements loop-summary SSA version allocation.
  });
});
