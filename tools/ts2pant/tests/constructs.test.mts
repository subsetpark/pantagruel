import { readdirSync } from "node:fs";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import type { SourceFile } from "../src/extract.js";
import { createSourceFile } from "../src/extract.js";
import { buildDocument, emitDocument } from "./helpers.mjs";

const CONSTRUCTS_DIR = resolve(import.meta.dirname, "fixtures/constructs");

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
  .sort();

for (const file of fixtureFiles) {
  describe(file, () => {
    const filePath = resolve(CONSTRUCTS_DIR, file);
    const sourceFile = createSourceFile(filePath);
    const targets = discoverTestTargets(sourceFile);
    if (targets.length === 0) {
      throw new Error(`No exported snapshot targets found in ${file}`);
    }

    for (const funcName of targets) {
      it(funcName, async (t) => {
        const doc = await buildDocument(filePath, funcName);
        const output = emitDocument(doc);
        t.assert.snapshot(output);
      });
    }
  });
}
