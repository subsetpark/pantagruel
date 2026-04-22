import { execFileSync } from "node:child_process";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import {
  buildDocument as buildDocumentFromPath,
  emitDocument,
} from "./helpers.mjs";

// Dogfood: translate ts2pant's own source files with ts2pant itself.
// Depth-first — one target module at a time, smallest function first.
// See /Users/zax/.claude/plans/let-s-start-with-annotating-distributed-koala.md
// for the roadmap. Each passing case here is a concrete self-translation
// baseline; failing cases are documented separately in the plan file as
// diagnostics driving the next feature to build.

const SRC = resolve(import.meta.dirname, "../src");
const PROJECT_ROOT = resolve(import.meta.dirname, "../../..");

function assertPantTypeChecks(output: string): void {
  execFileSync("dune", ["exec", "pant", "--"], {
    cwd: PROJECT_ROOT,
    input: output,
    encoding: "utf-8",
  });
}

describe("dogfood: src/name-registry.ts", () => {
  const filePath = resolve(SRC, "name-registry.ts");

  it("isUsed — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "isUsed");
    const output = emitDocument(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output);
  });
});
