import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { checkPantBundle } from "../src/pant-wasm.js";

describe("pant-wasm > checkPantBundle", () => {
  it("resolves a single import against an in-memory dep", async () => {
    const dep = `module DEP_LIB.

User.
nobody => User.
---
true.
`;

    const consumer = `module CONSUMER.

import DEP_LIB.

Doc.
---
all u: User | u = nobody.
`;

    const error = await checkPantBundle(
      consumer,
      new Map([["DEP_LIB", dep]]),
    );
    assert.equal(error, null);
  });

  it("reports unresolved import name as a missing-module error", async () => {
    const consumer = `module CONSUMER.

import MISSING.

Doc.
---
true.
`;

    const error = await checkPantBundle(consumer, new Map());
    assert.notEqual(error, null);
    assert.match(error!, /MISSING/);
  });

  it("reports a typecheck error in a dep module", async () => {
    // The dep references an undeclared type 'Bogus', so collecting its
    // environment fails; that error must surface to the caller.
    const dep = `module BROKEN_DEP.

f x: Bogus => Bool.
---
true.
`;

    const consumer = `module CONSUMER.

import BROKEN_DEP.

Doc.
---
true.
`;

    const error = await checkPantBundle(
      consumer,
      new Map([["BROKEN_DEP", dep]]),
    );
    assert.notEqual(error, null);
  });
});
