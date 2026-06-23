// @archlint.module test
// @archlint.domain ts2pant.constructs

import { readdirSync } from "node:fs";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { emitDocument } from "../src/emit.js";
import type { SourceFile } from "../src/extract.js";
import { createSourceFile } from "../src/extract.js";
import { buildDocumentFromSourceFile, emitAndCheck } from "./helpers.mjs";

const CONSTRUCTS_DIR = resolve(import.meta.dirname, "fixtures/constructs");
const SCAFFOLD_ONLY_FIXTURES = new Set([
  "expressions-for-of-comprehension.ts",
  "expressions-local-collection-builder.ts",
  "foreign-accessor-dependency.ts",
]);

import { KNOWN_TYPECHECK_FAILURES } from "./known-typecheck-failures.mjs";

/** Discover exported function names and class method names in a TS source file. */
function discoverTestTargets(sourceFile: SourceFile): string[] {
  const targets: string[] = [];

  for (const func of sourceFile.getFunctions()) {
    if (func.isExported()) {
      const name = func.getName();
      if (name) {
        targets.push(name);
      }
    }
  }

  for (const cls of sourceFile.getClasses()) {
    if (cls.isExported()) {
      const clsName = cls.getName();
      for (const method of cls.getMethods()) {
        const qualified = clsName
          ? `${clsName}.${method.getName()}`
          : method.getName();
        targets.push(qualified);
      }
    }
  }

  return targets;
}

const fixtureFiles = readdirSync(CONSTRUCTS_DIR)
  .filter((f) => f.endsWith(".ts"))
  .filter((f) => !SCAFFOLD_ONLY_FIXTURES.has(f))
  .sort();

for (const file of fixtureFiles) {
  describe(file, () => {
    const filePath = resolve(CONSTRUCTS_DIR, file);
    const discoverySourceFile = createSourceFile(filePath);
    const targets = discoverTestTargets(discoverySourceFile);
    discoverySourceFile.getProject().removeSourceFile(discoverySourceFile);
    if (targets.length === 0) {
      throw new Error(`No exported snapshot targets found in ${file}`);
    }

    for (const funcName of targets) {
      const key = `${file} > ${funcName}`;
      const knownBad = KNOWN_TYPECHECK_FAILURES.get(key);
      it(funcName, async (t) => {
        const sourceFile = createSourceFile(filePath);
        try {
          const doc = await buildDocumentFromSourceFile(sourceFile, funcName);
          const output = knownBad
            ? emitDocument(doc) // skip wasm typecheck — see KNOWN_TYPECHECK_FAILURES.
            : await emitAndCheck(doc);
          t.assert.snapshot(output);
        } finally {
          sourceFile.getProject().removeSourceFile(sourceFile);
        }
      });
    }
  });
}
