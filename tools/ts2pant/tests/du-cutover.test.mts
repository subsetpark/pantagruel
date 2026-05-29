import { describe, it } from "node:test";

describe("du-cutover", () => {
  it.skip("non-discriminated union field access is refused", () => {
    // PENDING: Patch 2.
  });

  it.skip("intersection field access remains sound (resolves a single owner)", () => {
    // PENDING: Patch 2.
  });

  it.skip("detected DU that fails tagged registration is refused (no + fallthrough)", () => {
    // PENDING: Patch 4.
  });
});
