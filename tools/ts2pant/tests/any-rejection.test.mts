// @archlint.module test
// @archlint.domain ts2pant.any-rejection

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import { emitDocument } from "../src/emit.js";
import { createSourceFileFromSource } from "../src/extract.js";
import { loadAst } from "../src/pant-wasm.js";
import { translateSignature } from "../src/translate-signature.js";
import {
  cellEmitSynth,
  IntStrategy,
  newSynthCell,
} from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

function signatureOutput(source: string, functionName: string): string {
  const sourceFile = createSourceFileFromSource(source);
  const synthCell = newSynthCell();
  const result = translateSignature(
    sourceFile,
    functionName,
    IntStrategy,
    synthCell,
  );
  const { decls } = cellEmitSynth(synthCell);
  return emitDocument({
    moduleName: "ANY_OPAQUE",
    imports: [],
    declarations: [...decls, result.declaration],
    propositions: [],
    checks: [],
  });
}

describe("any rejection", () => {
  it("any and unknown parameters map to the shared Opaque domain", () => {
    const anyOutput = signatureOutput(
      `
        function withAny(value: any): number {
          return 0;
        }
      `,
      "withAny",
    );
    const unknownOutput = signatureOutput(
      `
        function withUnknown(value: unknown): number {
          return 0;
        }
      `,
      "withUnknown",
    );

    assert.match(anyOutput, /^Opaque\.$/mu);
    assert.match(anyOutput, /^with-any value: Opaque => Int\.$/mu);
    assert.match(unknownOutput, /^Opaque\.$/mu);
    assert.match(unknownOutput, /^with-unknown value: Opaque => Int\.$/mu);
  });
});
