import { describe, it, expect } from "vitest";
import { resolve } from "path";
import { readdirSync } from "fs";
import ts from "typescript";
import { createProgram } from "../src/extract.js";
import { buildDocument, emitDocument } from "./helpers.js";

const CONSTRUCTS_DIR = resolve(__dirname, "fixtures/constructs");

/** Discover exported function names and class method names in a TS source file. */
function discoverTestTargets(program: ts.Program, filePath: string): string[] {
  const sourceFile = program.getSourceFile(filePath);
  if (!sourceFile) return [];

  const targets: string[] = [];

  for (const stmt of sourceFile.statements) {
    const isExported = ts.canHaveModifiers(stmt) &&
      ts.getModifiers(stmt)?.some(
        (m) => m.kind === ts.SyntaxKind.ExportKeyword,
      );

    if (!isExported) continue;

    if (ts.isFunctionDeclaration(stmt) && stmt.name) {
      targets.push(stmt.name.text);
    }

    if (ts.isClassDeclaration(stmt) && stmt.name) {
      for (const member of stmt.members) {
        if (
          ts.isMethodDeclaration(member) &&
          ts.isIdentifier(member.name)
        ) {
          targets.push(member.name.text);
        }
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
    const program = createProgram(filePath);
    const targets = discoverTestTargets(program, filePath);

    for (const funcName of targets) {
      it(funcName, () => {
        const doc = buildDocument(filePath, funcName);
        const output = emitDocument(doc);
        expect(output).toMatchSnapshot();
      });
    }
  });
}
