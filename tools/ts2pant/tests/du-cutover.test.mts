// @archlint.module test
// @archlint.domain ts2pant.du-cutover

import assert from "node:assert/strict";
import { describe, it } from "node:test";
import {
  createSourceFileFromSource,
  extractAllTypes,
  getChecker,
} from "../src/extract.js";
import {
  IntStrategy,
  mapTsType,
  newSynthCell,
} from "../src/translate-types.js";
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

  it("detected DU that fails tagged registration is refused (no + fallthrough)", () => {
    const sourceFile = createSourceFileFromSource(`
      type MixedDiscriminant =
        | { kind: "left"; value: number }
        | { kind: 1; value: number };
    `);
    const checker = getChecker(sourceFile);
    const alias = extractAllTypes(sourceFile).aliases[0];
    assert.ok(alias);

    const mapped = mapTsType(alias.type, checker, IntStrategy, newSynthCell());

    // Refused (not fallen through to the `A + B` union encoding).
    assert.deepEqual(mapped, {
      ok: false,
      reason: "discriminant literals do not share one Pantagruel sort",
    });
  });
});
