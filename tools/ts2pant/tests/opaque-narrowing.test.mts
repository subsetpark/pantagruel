// @archlint.module test
// @archlint.domain ts2pant.opaque-narrowing

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { createSourceFile } from "../src/extract.js";
import { buildDocumentFromSourceFile, emitAndCheck } from "./helpers.mjs";

const OPAQUE_FIXTURE_DIR = resolve(import.meta.dirname, "fixtures/opaque");

describe("opaque narrowing", () => {
  it(
    "a typeof-narrowed unknown read lowers to the concrete sort, not Opaque",
    async () => {
      const sourceFile = createSourceFile(
        resolve(OPAQUE_FIXTURE_DIR, "narrowing.ts"),
      );
      const output = await emitAndCheck(
        await buildDocumentFromSourceFile(sourceFile, "typeofString"),
      );

      assert.match(output, /^typeof-string value: Opaque => String\.$/mu);
      assert.match(output, /^opaque_value_.* => String\.$/mu);
      assert.doesNotMatch(output, /^opaque_value_.* => Opaque\.$/mu);
    },
  );

  it("supported non-scalar narrowed reads lower to their concrete sorts", async () => {
    const sourceFile = createSourceFile(
      resolve(OPAQUE_FIXTURE_DIR, "narrowing.ts"),
    );

    const classOutput = await emitAndCheck(
      await buildDocumentFromSourceFile(sourceFile, "instanceofClass"),
    );
    assert.match(
      classOutput,
      /^instanceof-class value: Opaque, fallback: TokenBox => TokenBox\.$/mu,
    );
    assert.match(classOutput, /^opaque_value_.* => TokenBox\.$/mu);

    const discriminatorOutput = await emitAndCheck(
      await buildDocumentFromSourceFile(sourceFile, "discriminatorPredicate"),
    );
    assert.match(
      discriminatorOutput,
      /^discriminator-predicate value: Opaque, fallback: Circle => Circle\.$/mu,
    );
    assert.match(discriminatorOutput, /^opaque_value_.* => Circle\.$/mu);

    const arrayOutput = await emitAndCheck(
      await buildDocumentFromSourceFile(sourceFile, "arrayIsArray"),
    );
    assert.match(
      arrayOutput,
      /^array-is-array value: Opaque, fallback: \[Int\] => \[Int\]\.$/mu,
    );
    assert.match(arrayOutput, /^opaque_value_.* => \[Int\]\.$/mu);
  });

  it("un-narrowed and unmappable narrowed any/unknown reads stay Opaque", async () => {
    const sourceFile = createSourceFile(
      resolve(OPAQUE_FIXTURE_DIR, "narrowing.ts"),
    );

    const unNarrowedOutput = await emitAndCheck(
      await buildDocumentFromSourceFile(sourceFile, "unNarrowed"),
    );
    assert.match(unNarrowedOutput, /^un-narrowed value: Opaque => Opaque\.$/mu);
    assert.match(unNarrowedOutput, /^un-narrowed value = value\.$/mu);
    assert.doesNotMatch(unNarrowedOutput, /^opaque_value_/mu);

    const unmappableArrayOutput = await emitAndCheck(
      await buildDocumentFromSourceFile(sourceFile, "arrayIsArrayUnmappable"),
    );
    assert.match(
      unmappableArrayOutput,
      /^array-is-array-unmappable value: Opaque => Opaque\.$/mu,
    );
    assert.match(
      unmappableArrayOutput,
      /^array-is-array-unmappable value = \(cond is-any-array-by-array-is-array value => value, true => value\)\.$/mu,
    );
    assert.doesNotMatch(
      unmappableArrayOutput,
      /^opaque_value_.* => \[.*\]\.$/mu,
    );
  });

  it(
    "narrowing is use-site-local and the parameter signature sort stays Opaque",
    async () => {
      const sourceFile = createSourceFile(
        resolve(OPAQUE_FIXTURE_DIR, "narrowing.ts"),
      );
      const output = await emitAndCheck(
        await buildDocumentFromSourceFile(sourceFile, "perUseSite"),
      );

      assert.match(
        output,
        /^per-use-site value: Opaque, flag: Bool => String\.$/mu,
      );
      assert.equal(
        [...output.matchAll(/^opaque_value_.* => String\.$/gmu)].length,
        2,
      );
      assert.doesNotMatch(output, /^per-use-site value: String/mu);
    },
  );

  it("same-line narrowed use sites keep distinct origins", async () => {
    const sourceFile = createSourceFile(
      resolve(OPAQUE_FIXTURE_DIR, "narrowing.ts"),
    );
    const output = await emitAndCheck(
      await buildDocumentFromSourceFile(sourceFile, "sameLineUseSites"),
    );

    assert.match(
      output,
      /^same-line-use-sites first: Opaque, second: Opaque, fallback: TokenBox => TokenBox\.$/mu,
    );
    assert.equal(
      [...output.matchAll(/^opaque_value_.* => String\.$/gmu)].length,
      1,
    );
    assert.equal(
      [...output.matchAll(/^opaque_value_.* => TokenBox\.$/gmu)].length,
      1,
    );
  });
});
