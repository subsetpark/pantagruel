import assert from "node:assert/strict";
import { describe, it } from "node:test";

import { createSourceFileFromSource } from "../src/extract.js";
import { buildDocumentFromSourceFile, emitAndCheck } from "./helpers.mjs";

describe("loop-break-continue", () => {
  it.skip("counter loop with break translates and bumps to fixed-point", () => {
    // PENDING Patches 5 and 6.
  });

  it.skip("counter loop with continue translates and stays bounded-quantified", () => {
    // PENDING Patches 5 and 6.
  });

  it.skip("bounded-while with break translates and bumps to fixed-point", () => {
    // PENDING Patches 5 and 6.
  });

  it.skip("fixed-point while with break translates", () => {
    // PENDING Patches 5 and 6.
  });

  it.skip("fixed-point while with continue translates", () => {
    // PENDING Patches 5 and 6.
  });

  it.skip("fixed-point while with bare return translates", () => {
    // PENDING Patches 5 and 6.
  });

  it.skip("fixed-point while with return value translates and emits return-value equation", () => {
    // PENDING Patches 5 and 6.
  });

  it.skip("fixed-point while with throw translates and emits iteration precondition", () => {
    // PENDING Patches 5 and 6.
  });

  it.skip("while(true)+break translates (event-loop idiom)", () => {
    // PENDING Patches 5 and 6.
  });

  it("labeled break rejects with M7 diagnostic", async () => {
    const sourceFile = createSourceFileFromSource(`
      interface Account {
        balance: number;
      }

      function update(a: Account): void {
        outer: for (let i = 0; i < 3; i++) {
          a.balance = i;
          break outer;
        }
      }
    `);

    const output = await emitAndCheck(
      await buildDocumentFromSourceFile(sourceFile, "update"),
    );

    assert.match(
      output,
      /labeled loop early-exit is M7 future work; remove the label or restructure/u,
    );
  });
});
