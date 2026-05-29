import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { createSourceFileFromSource } from "../src/extract.js";
import { buildDocumentFromSourceFile, emitAndCheck } from "./helpers.mjs";

describe("du-cutover", () => {
  it("non-discriminated union field access is refused", async () => {
    const sourceFile = createSourceFileFromSource(`
      type NonDiscriminated =
        | { owner: string; left: number }
        | { owner: string; right: number };

      export function readLeft(x: NonDiscriminated): number {
        return x.left;
      }
    `);

    const output = await emitAndCheck(
      await buildDocumentFromSourceFile(sourceFile, "readLeft"),
    );

    assert.match(
      output,
      /UNSUPPORTED: property access \.left: field access on a non-discriminated union is not expressible in Pantagruel/u,
    );
  });

  it("intersection field access remains sound (resolves a single owner)", async () => {
    const sourceFile = createSourceFileFromSource(`
      interface HasId { id: number; }
      interface HasLabel { label: string; }
      /** @pant-type x: HasId */
      export function readId(x: HasId & HasLabel): number {
        return x.id;
      }
    `);

    const output = await emitAndCheck(
      await buildDocumentFromSourceFile(sourceFile, "readId"),
    );

    assert.match(output, /^has-id--id h: HasId => Int\.$/mu);
    assert.match(output, /^read-id x: HasId => Int\.$/mu);
    assert.match(output, /^read-id x = has-id--id x\.$/mu);
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it.skip("detected DU that fails tagged registration is refused (no + fallthrough)", () => {
    // PENDING: Patch 4.
  });
});
