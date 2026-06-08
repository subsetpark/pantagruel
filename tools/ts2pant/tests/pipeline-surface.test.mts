// @archlint.module test
// @archlint.domain ts2pant.pipeline

import assert from "node:assert/strict";
import * as fc from "fast-check";
import { it } from "node:test";
import { createSourceFileFromSource } from "../src/extract.js";
import { buildPantDocument } from "../src/pipeline.js";
import { IntStrategy } from "../src/translate-types.js";

const cases = [
  { name: "score", expr: "value + 1" },
  { name: "count", expr: "value - 1" },
  { name: "rank", expr: "Math.max(value, 0)" },
  { name: "clamp", expr: "value > 0 ? value : 0" },
].map(({ name, expr }) => ({
  name,
  sourceFile: createSourceFileFromSource(`
    function ${name}(value: number): number {
      return ${expr};
    }
  `),
}));

it("generated source builds a Pant document through the pipeline", async () => {
  await fc.assert(
    fc.asyncProperty(
      fc.constantFrom(...cases),
      async ({ name, sourceFile }) => {
        const doc = await buildPantDocument({
          sourceFile,
          functionName: name,
          strategy: IntStrategy,
        });
        assert.equal(doc.declarations.length > 0, true);
      },
    ),
    { numRuns: cases.length },
  );
});
