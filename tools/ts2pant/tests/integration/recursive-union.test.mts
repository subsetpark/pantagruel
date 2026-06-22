// @archlint.module test
// @archlint.domain ts2pant.recursive-union

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";

import { loadAst } from "../../src/pant-wasm.js";
import {
  buildDocument,
  emitAndCheck,
  runCheck,
  solverAvailable,
} from "../helpers.mjs";

const FIXTURE = resolve(
  import.meta.dirname,
  "../fixtures/recursive-union/cases.ts",
);

describe("recursive discriminated union integration", () => {
  before(async () => {
    await loadAst();
  });

  for (const fn of ["treeKind", "leafValue", "leftChildKind"]) {
    it(`@pant on \`${fn}\` (recursive union) entails`, {
      skip: !solverAvailable() ? "z3 not available" : undefined,
    }, async () => {
      const output = await emitAndCheck(await buildDocument(FIXTURE, fn));
      const result = runCheck(output);
      const entailments = result.checks.filter(
        (c) =>
          c.message.startsWith("OK: Entailed:") ||
          c.message.startsWith("FAIL: Not entailed:"),
      );
      assert.ok(entailments.length >= 1, "expected an entailment result");
      assert.ok(result.passed, `pant --check failed:\n${result.output}`);
      assert.ok(entailments.every((c) => c.passed));
    });
  }
});
