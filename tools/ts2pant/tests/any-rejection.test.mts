// @archlint.module test
// @archlint.domain ts2pant.any-rejection

import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { createSourceFileFromSource } from "../src/extract.js";
import { translateSignature } from "../src/translate-signature.js";
import {
  IntStrategy,
  UNSUPPORTED_UNKNOWN_REASON,
} from "../src/translate-types.js";

function unsupportedReason(source: string, functionName: string): string {
  const sourceFile = createSourceFileFromSource(source);
  const result = translateSignature(sourceFile, functionName, IntStrategy);
  assert.equal(result.declaration.kind, "unsupported");
  if (result.declaration.kind !== "unsupported") {
    throw new Error("expected unsupported declaration");
  }
  return result.declaration.reason;
}

function reasonCause(reason: string): string {
  return reason.slice(reason.indexOf(": ") + 2);
}

describe("any rejection", () => {
  it("any maps to UNSUPPORTED_UNKNOWN with the same reason as unknown", () => {
    const anyReason = unsupportedReason(
      `
        function withAny(value: any): number {
          return 0;
        }
      `,
      "withAny",
    );
    const unknownReason = unsupportedReason(
      `
        function withUnknown(value: unknown): number {
          return 0;
        }
      `,
      "withUnknown",
    );
    const anyCause = reasonCause(anyReason);
    const unknownCause = reasonCause(unknownReason);

    assert.equal(anyCause, unknownCause);
    assert.equal(anyCause, UNSUPPORTED_UNKNOWN_REASON);
    assert.equal(
      anyReason,
      `with-any param 'value': ${UNSUPPORTED_UNKNOWN_REASON}`,
    );
    assert.equal(
      unknownReason,
      `with-unknown param 'value': ${UNSUPPORTED_UNKNOWN_REASON}`,
    );
  });
});
