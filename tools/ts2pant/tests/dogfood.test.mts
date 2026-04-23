import { resolve } from "node:path";
import { describe, it } from "node:test";
import {
  assertPantTypeChecks,
  buildDocument as buildDocumentFromPath,
  emitDocument,
} from "./helpers.mjs";

// Dogfood: translate ts2pant's own source files with ts2pant itself.
// Depth-first — one target module at a time, smallest function first.
// Each passing case here is a concrete self-translation baseline; failing
// cases are tracked in the project roadmap as diagnostics driving the next
// feature to build.

const SRC = resolve(import.meta.dirname, "../src");
const PANT_TIMEOUT_MS = Number(
  process.env.TS2PANT_DOGFOOD_TIMEOUT_MS ?? 30_000,
);

describe("dogfood: src/name-registry.ts", () => {
  const filePath = resolve(SRC, "name-registry.ts");

  it("isUsed — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "isUsed");
    const output = emitDocument(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
  });

  it("emptyNameRegistry — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "emptyNameRegistry");
    const output = emitDocument(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
  });
});
