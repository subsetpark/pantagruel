// @archlint.module test
// @archlint.domain ts2pant.opaque

import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { createSourceFileFromSource } from "../src/extract.js";
import { OPAQUE_DOMAIN } from "../src/opaque.js";
import { translateSignature } from "../src/translate-signature.js";
import {
  IntStrategy,
  UNSUPPORTED_UNKNOWN_REASON,
} from "../src/translate-types.js";
import { buildDocumentFromSourceFile } from "./helpers.mjs";

describe("opaque default", () => {
  // PENDING: Patch 2 flips the default opacity policy to opaque.
  it.skip("any and unknown lower to the Opaque domain with no explicit policy", async () => {
    const sourceFile = createSourceFileFromSource(`
        export function withAny(value: any): any {
          return value;
        }
      `);

    const signature = translateSignature(sourceFile, "withAny", IntStrategy);
    assert.equal(signature.declaration.kind, "rule");
    if (signature.declaration.kind !== "rule") {
      throw new Error("expected rule declaration");
    }
    assert.equal(signature.declaration.returnType, OPAQUE_DOMAIN);

    const doc = await buildDocumentFromSourceFile(sourceFile, "withAny");
    assert.ok(
      doc.declarations.some(
        (decl) => decl.kind === "domain" && decl.name === OPAQUE_DOMAIN,
      ),
    );
  });

  // PENDING: Patch 2 flips the default opacity policy to opaque.
  it.skip("explicit reject policy still emits UNSUPPORTED_UNKNOWN", () => {
    const sourceFile = createSourceFileFromSource(`
      export function withUnknown(value: unknown): number {
        return 0;
      }
    `);

    const result = translateSignature(
      sourceFile,
      "withUnknown",
      IntStrategy,
      undefined,
      undefined,
      { typeMapping: { policy: "reject" } },
    );
    assert.equal(result.declaration.kind, "unsupported");
    if (result.declaration.kind !== "unsupported") {
      throw new Error("expected unsupported declaration");
    }
    assert.equal(
      result.declaration.reason,
      `with-unknown param 'value': ${UNSUPPORTED_UNKNOWN_REASON}`,
    );
  });
});
