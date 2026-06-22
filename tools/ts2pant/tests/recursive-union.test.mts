// @archlint.module test
// @archlint.domain ts2pant.recursive-union

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";

import { emitDocument } from "../src/emit.js";
import { buildDocument } from "./helpers.mjs";

const FIXTURE = resolve(
  import.meta.dirname,
  "fixtures/recursive-union/cases.ts",
);

describe("recursive discriminated union", () => {
  it("refuses gracefully instead of crashing (regression)", async () => {
    // `treeKind`'s parameter is a self-referential discriminated union
    // (`Tree` node variant has `left`/`right`: `Tree`). DU synthesis used to
    // recurse forever and throw `RangeError: Maximum call stack size
    // exceeded`. It must now build without throwing and refuse the function
    // with a clean UNSUPPORTED skip — not leak the refusal sentinel into a
    // rule head.
    await assert.doesNotReject(buildDocument(FIXTURE, "treeKind"));
    const output = emitDocument(await buildDocument(FIXTURE, "treeKind"));
    assert.match(output, /> UNSUPPORTED: tree-kind param 't'/u);
    assert.doesNotMatch(
      output,
      /tree-kind t: discriminated union could not be registered/u,
    );
  });

  it("a non-recursive discriminated union still registers (control)", async () => {
    const output = emitDocument(await buildDocument(FIXTURE, "shapeKind"));
    assert.doesNotMatch(output, /UNSUPPORTED/u);
    assert.match(output, /^Shape\.$/mu);
    assert.match(output, /shape--kind s: Shape => String\./u);
  });
});
