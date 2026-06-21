// @archlint.module test
// @archlint.domain ts2pant.opaque

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { createSourceFile } from "../src/extract.js";
import { emitDocument } from "./helpers.mts";
import { buildDocumentFromSourceFile } from "./helpers.mts";

const CONSTRUCTS_FIXTURE_DIR = resolve(
  import.meta.dirname,
  "fixtures/constructs",
);

describe("opaque default", () => {
  it("any and unknown lower to the Opaque domain with no explicit policy", async () => {
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
          `${target} should not reject under the default policy`,
        );
      }
    } finally {
      sourceFile.getProject().removeSourceFile(sourceFile);
    }
  });

  it("explicit reject policy still emits UNSUPPORTED_UNKNOWN", async () => {
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
        const doc = await buildDocumentFromSourceFile(sourceFile, target, {
          policy: "reject",
        });
        const output = emitDocument(doc);

        assert.match(
          output,
          /^> UNSUPPORTED: .*TS unknown is not expressible in Pantagruel; declare a specific type$/mu,
          `${target} should keep reject diagnostics`,
        );
        assert.doesNotMatch(
          output,
          /^Opaque\.$/mu,
          `${target} should not declare Opaque under reject`,
        );
      }
    } finally {
      sourceFile.getProject().removeSourceFile(sourceFile);
    }
  });
});
