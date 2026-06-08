// @archlint.module test
// @archlint.domain ts2pant.collection-ssa-parity

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { buildDocument, emitAndCheck } from "../helpers.mjs";

describe("collection SSA production parity", () => {
  it("preserves production parity for Map and Set mutation fixtures", async () => {
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

    const fixturePath = (file: string): string =>
      resolve(import.meta.dirname, "../fixtures/constructs", file);
    for (const [file, functionName] of [...mapCases, ...setCases]) {
      const doc = await buildDocument(fixturePath(file), functionName);
      const output = await emitAndCheck(doc);
      assert.doesNotMatch(output, /^> UNSUPPORTED:/mu);
      assert.match(output, /\n---\n\n\S/mu);
    }
  });
});
