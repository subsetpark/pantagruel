// @archlint.module test
// @archlint.domain ts2pant.opaque

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { createSourceFile } from "../src/extract.js";
import { buildDocumentFromSourceFile, emitDocument } from "./helpers.mts";

const CONSTRUCTS_FIXTURE_DIR = resolve(
  import.meta.dirname,
  "fixtures/constructs",
);

describe("opaque default", () => {
  it("any and unknown lower to the Opaque domain", async () => {
    const sourceFile = createSourceFile(
      resolve(CONSTRUCTS_FIXTURE_DIR, "opaque-any-unknown.ts"),
    );
    const targets = [
      "anyParam",
      "anyReturn",
      "anyArrayReturn",
      "unknownParam",
      "unknownReturn",
    ];

    try {
      for (const target of targets) {
        const doc = await buildDocumentFromSourceFile(sourceFile, target);
        const output = emitDocument(doc);

        assert.match(output, /^Opaque\.$/mu, `${target} should declare Opaque`);
        assert.doesNotMatch(
          output,
          /^> UNSUPPORTED:/mu,
          `${target} should not reject`,
        );
      }
    } finally {
      sourceFile.getProject().removeSourceFile(sourceFile);
    }
  });
});
