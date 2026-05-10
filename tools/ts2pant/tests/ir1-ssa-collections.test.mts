import assert from "node:assert/strict";
import { describe, it } from "node:test";
import {
  ir1LitNat,
  ir1SsaMapMembershipValue,
  ir1SsaMapSetValue,
  ir1SsaSetClearValue,
} from "../src/ir1.js";

describe("ir1-ssa-collections", () => {
  // PENDING Patch 2: introduce shared collection SSA state for coordinated
  // Map value/membership and Set membership versions.
  // PENDING Patch 3: implement Map SSA writes, staged reads, and lowering.
  // PENDING Patch 4: implement Set SSA writes, staged reads, clear, and lowering.
  // PENDING Patch 5: route non-looping Map/Set bodies through collection SSA.
  // PENDING Patch 6: keep production parity fixtures pinned while the old
  // SymbolicState-only path is narrowed.

  it.skip("records Map.set as coordinated value and membership SSA writes", () => {
    const value = ir1SsaMapSetValue(ir1LitNat(7));
    const membership = ir1SsaMapMembershipValue("set");

    assert.equal(value.kind, "map-value");
    assert.equal(membership.kind, "map-membership");
    // PENDING Patch 3: build collection SSA for a `Map.set(k, v)` body and
    // assert it records one map-value write plus one paired map-membership
    // write for the same receiver/key location.
  });

  it.skip("resolves Map.get and Map.has through staged SSA writes", () => {
    // PENDING Patch 3: lower a non-looping `set -> get/has` body and assert
    // both reads resolve against the dominating staged versions rather than
    // the pre-state rule helpers from `readMapThroughWrites`.
  });

  it.skip("joins Map value and membership versions across branches", () => {
    // PENDING Patch 3: cover `if (gate) m.set(...)` / `else m.delete(...)`
    // and assert collection SSA emits IR1SsaJoin nodes for both map-value and
    // map-membership locations instead of relying on `mergeOverrides`.
  });

  it.skip("records Set.add delete and clear as membership SSA writes", () => {
    const clear = ir1SsaSetClearValue();

    assert.equal(clear.kind, "set-membership");
    assert.equal(clear.op, "clear");
    // PENDING Patch 4: build collection SSA for `.add`, `.delete`, and
    // `.clear()` and assert all writes target set-membership locations, with
    // clear represented by `ir1SsaSetClearValue()`.
  });

  it.skip("resolves Set.has through staged membership and clear versions", () => {
    // PENDING Patch 4: lower `add/delete/clear -> has` bodies and assert
    // reads observe later-wins membership versions plus clear fallthrough,
    // matching the existing `readSetThroughWrites` semantics.
  });

  it.skip("preserves production parity for Map and Set mutation fixtures", async () => {
    const mapCases = [
      ["expressions-map-mutation.ts", "put"],
      ["expressions-map-mutation.ts", "remove"],
      ["expressions-map-mutation.ts", "setAndCopy"],
      ["expressions-state-aware-reads.ts", "entrySetThenCheck"],
      ["expressions-state-aware-reads.ts", "bumpInBranch"],
    ] as const;

    const setCases = [
      ["expressions-set-mutation-field.ts", "tagAddThenRemove"],
      ["expressions-set-mutation-field.ts", "tagClearAndAdd"],
      ["expressions-state-aware-reads.ts", "tagThenCheck"],
    ] as const;

    void mapCases;
    void setCases;
    // PENDING Patch 5: route the non-looping production path through
    // collection SSA and keep these existing Map/Set mutation and staged-read
    // fixtures byte-equivalent at emit time.
  });
});
