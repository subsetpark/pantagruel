// @archlint.module test
// @archlint.domain ts2pant.opaque-narrowing

import { describe, it } from "node:test";
import { createSourceFileFromSource as createSourceFile } from "../src/extract.js";
import { buildDocumentFromSourceFile, emitAndCheck } from "./helpers.mjs";

describe("opaque narrowing", () => {
  it.skip(
    "a typeof-narrowed unknown read lowers to the concrete sort, not Opaque",
    async () => {
      // PENDING: Patch 2 wires narrowedSortForUse into the dynamic-use-site lowering path.
      const sourceFile = createSourceFile(`
        function f(x: unknown): string {
          if (typeof x === "string") {
            return x;
          }
          return "";
        }
      `);
      await emitAndCheck(await buildDocumentFromSourceFile(sourceFile, "f"));
    },
  );

  it.skip("an un-narrowed any/unknown read stays Opaque", async () => {
    // PENDING: Patch 2 adds assertions once use-site narrowing behavior exists.
    const sourceFile = createSourceFile(`
      function f(x: unknown): unknown {
        return x;
      }
    `);
    await emitAndCheck(await buildDocumentFromSourceFile(sourceFile, "f"));
  });

  it.skip(
    "narrowing is use-site-local and the parameter signature sort stays Opaque",
    async () => {
      // PENDING: Patch 2 covers the cross-function-boundary invariant.
      const sourceFile = createSourceFile(`
        function f(x: unknown): string {
          if (typeof x === "string") {
            return x;
          }
          return "";
        }
      `);
      await emitAndCheck(await buildDocumentFromSourceFile(sourceFile, "f"));
    },
  );
});
