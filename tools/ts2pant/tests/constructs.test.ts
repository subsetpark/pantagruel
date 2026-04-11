import { describe, it, expect } from "vitest";
import { resolve } from "path";
import { readdirSync } from "fs";
import { createSourceFile } from "../src/extract.js";
import type { SourceFile } from "../src/extract.js";
import { buildDocument, emitDocument } from "./helpers.js";

const CONSTRUCTS_DIR = resolve(__dirname, "fixtures/constructs");

/** Discover exported function names and class method names in a TS source file. */
function discoverTestTargets(sourceFile: SourceFile): string[] {
  const targets: string[] = [];

  for (const func of sourceFile.getFunctions()) {
    if (func.isExported()) {
      const name = func.getName();
      if (name) targets.push(name);
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
      it(funcName, () => {
        const doc = buildDocument(filePath, funcName);
        const output = emitDocument(doc);
        expect(output).toMatchSnapshot();
      });
    }
  });
}
