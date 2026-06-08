// @archlint.module test
// @archlint.domain ts2pant.ts-ast-block-return

import assert from "node:assert/strict";
import { describe, it } from "node:test";
import * as fc from "fast-check";
import ts from "typescript";
import {
  extractBlockReturn,
  extractBlockReturnFromStatements,
} from "../src/ts-ast-block-return.js";

const TS_KEYWORDS = new Set([
  "as",
  "await",
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "enum",
  "export",
  "extends",
  "false",
  "finally",
  "for",
  "from",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "new",
  "null",
  "return",
  "super",
  "switch",
  "this",
  "throw",
  "true",
  "try",
  "typeof",
  "undefined",
  "var",
  "void",
  "while",
  "with",
  "yield",
]);

const identifierArb = fc
  .stringMatching(/^[a-z][a-z0-9]{0,6}$/)
  .filter((name) => !TS_KEYWORDS.has(name));

function parseBlock(source: string): ts.Block {
  const sourceFile = ts.createSourceFile(
    "block-return-test.ts",
    `function f() ${source}`,
    ts.ScriptTarget.Latest,
    true,
  );
  const declaration = sourceFile.statements[0];
  if (
    !declaration ||
    !ts.isFunctionDeclaration(declaration) ||
    !declaration.body
  ) {
    throw new Error("expected function body");
  }
  return declaration.body;
}

describe("ts-ast-block-return", () => {
  it("extracts generated const bindings before a return", () => {
    fc.assert(
      fc.property(
        fc.uniqueArray(identifierArb, {
          maxLength: 8,
        }),
        fc.integer({ min: 0, max: 1000 }),
        (names, returnValue) => {
          const statements = names
            .map((name, index) => `const ${name} = ${index};`)
            .join("\n");
          const block = parseBlock(`{ ${statements} return ${returnValue}; }`);

          const extracted = extractBlockReturn(block);
          const fromStatements = extractBlockReturnFromStatements(
            block.statements,
          );

          assert.deepEqual(
            extracted?.bindings.map((binding) => binding.tsName),
            names,
          );
          assert.equal(extracted?.returnExpr.getText(), String(returnValue));
          assert.equal(
            fromStatements?.returnExpr.getText(),
            String(returnValue),
          );
        },
      ),
    );
  });
});
