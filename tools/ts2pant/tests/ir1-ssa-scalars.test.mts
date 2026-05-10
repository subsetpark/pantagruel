import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { resolve } from "node:path";

import { createSourceFile } from "../src/extract.js";
import { buildDocumentFromSourceFile, emitAndCheck } from "./helpers.mjs";

const CONSTRUCTS_DIR = resolve(import.meta.dirname, "fixtures/constructs");

function loadFixture(fileName: string) {
  return createSourceFile(resolve(CONSTRUCTS_DIR, fileName));
}

async function emitFixture(fileName: string, functionName: string) {
  const sourceFile = loadFixture(fileName);
  const doc = await buildDocumentFromSourceFile(sourceFile, functionName);
  return emitAndCheck(doc);
}

describe.skip("ir1-ssa-scalars", () => {
  // PENDING Patch 2: add the scalar SSA builder state and write/version recording.
  // PENDING Patch 3: lower final scalar versions back into the current Pant equations.
  // PENDING Patch 4: route scalar assignments and scalar if-mutation through SSA.
  // PENDING Patch 5: route early-exit continuation merges through SSA.

  it.skip("records direct property assignment as an SSA write", async () => {
    const sourceFile = loadFixture("functions-mutating.ts");
    void sourceFile;

    // `reset` is the minimal scalar write shape: one property assignment
    // whose lowered output should ultimately expose a single SSA write and
    // the corresponding final property version.
    assert.fail("PENDING: scalar SSA write/version assertions are not wired yet");
  });

  it.skip("resolves compound property assignment through the dominating prior version", async () => {
    const source = `
      interface Account {
        balance: number;
      }
      export function f(a: Account): void {
        a.balance = 1;
        a.balance += 2;
      }
    `;
    void source;

    // PENDING Patch 2 + Patch 3: this should assert that the compound write
    // reads the dominating scalar version rather than the pre-state value.
    assert.fail("PENDING: compound scalar SSA read dominance is not implemented yet");
  });

  it.skip("joins a single-arm if against the initial property version", async () => {
    const source = `
      interface Account {
        balance: number;
      }
      export function f(a: Account, g: boolean): void {
        if (g) {
          a.balance = 1;
        }
      }
    `;
    void source;

    // PENDING Patch 4: the scalar SSA route should build a join against the
    // pre-state initial version for the else-free arm.
    assert.fail("PENDING: single-arm scalar join lowering is not implemented yet");
  });

  it.skip("lowers nested scalar if bodies through SSA joins", async () => {
    const source = `
      interface Account {
        balance: number;
      }
      export function f(a: Account, x: boolean, y: boolean): void {
        if (x) {
          if (y) {
            a.balance = 1;
          } else {
            a.balance = 2;
          }
        } else {
          a.balance = 3;
        }
      }
    `;
    void source;

    // PENDING Patch 4: nested scalar conditionals should lower as nested SSA
    // joins or equivalent nested cond output, preserving the current output shape.
    assert.fail("PENDING: nested scalar SSA joins are not implemented yet");
  });

  it.skip(
    "preserves translateBody parity for scalar sequential write/read fixtures",
    async () => {
      const output = await emitFixture(
        "functions-mutating-conditional.ts",
        "accumulateIf",
      );
      void output;

      // PENDING Patch 4: this fixture should still emit the same Pantagruel
      // text after the scalar SSA route becomes active.
      assert.fail("PENDING: scalar translateBody parity is not implemented yet");
    },
  );

  it.skip(
    "preserves translateBody parity for asymmetric branch write fixtures",
    async () => {
      const output = await emitFixture(
        "functions-mutating-conditional.ts",
        "asymmetric",
      );
      void output;

      // PENDING Patch 4: asymmetric branch writes should keep the current
      // output while switching the scalar mutation path to SSA internally.
      assert.fail("PENDING: asymmetric branch parity is not implemented yet");
    },
  );

  it.skip(
    "preserves translateBody parity for chained early-return fixtures",
    async () => {
      const output = await emitFixture(
        "functions-mutating-early-exit.ts",
        "chainedEarlyReturns",
      );
      void output;

      // PENDING Patch 5: continuation merges after chained early exits should
      // remain byte-stable at the Pantagruel boundary.
      assert.fail("PENDING: chained early-return parity is not implemented yet");
    },
  );

  it.skip(
    "preserves translateBody parity for else-branch early-return fixtures",
    async () => {
      const output = await emitFixture(
        "functions-mutating-early-exit.ts",
        "elseBranchReturn",
      );
      void output;

      // PENDING Patch 5: else-branch early returns should keep the current
      // continuation-matching output after SSA routing lands.
      assert.fail("PENDING: else-branch early-return parity is not implemented yet");
    },
  );

  it.skip(
    "preserves translateBody parity for write-then-early-return fixtures",
    async () => {
      const output = await emitFixture(
        "functions-mutating-early-exit.ts",
        "writeThenEarlyReturn",
      );
      void output;

      // PENDING Patch 5: the post-return write path should stay output-stable
      // once early-exit merges are routed through scalar SSA.
      assert.fail("PENDING: write-then-early-return parity is not implemented yet");
    },
  );
});
