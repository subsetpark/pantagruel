// @archlint.module test
// @archlint.domain ts2pant.annotations

import assert from "node:assert/strict";
import { describe, it } from "node:test";
import * as fc from "fast-check";
import ts from "typescript";
import {
  extractAnnotations,
  extractFunctionAnnotations,
  extractFunctionAnnotationsAndOverrides,
  extractFunctionTypeOverrides,
} from "../src/annotations.js";
import { createSourceFileFromSource } from "../src/extract.js";

// ---------------------------------------------------------------------------
// Helper: create a SourceFile from raw TS source and find the first function
// ---------------------------------------------------------------------------
function firstFunction(source: string): ts.FunctionDeclaration {
  const sourceFile = ts.createSourceFile(
    "test.ts",
    source,
    ts.ScriptTarget.Latest,
    /* setParentNodes */ true,
  );
  let fn: ts.FunctionDeclaration | undefined;
  ts.forEachChild(sourceFile, (child) => {
    if (ts.isFunctionDeclaration(child) && !fn) {
      fn = child;
    }
  });
  if (!fn) {
    throw new Error("No function declaration found in source");
  }
  return fn;
}

/** Wrap a JSDoc body in `/** ... *\/` + a dummy function declaration. */
function functionWithJsDoc(body: string): ts.FunctionDeclaration {
  return firstFunction(`/**\n${body}\n */\nfunction subject(): void {}\n`);
}

describe("extractAnnotations", () => {
  it("extracts a single @pant proposition", () => {
    const node = functionWithJsDoc(" * @pant result f x = x + 1");
    const result = extractAnnotations(node);
    assert.equal(result.propositions.length, 1);
    assert.equal(result.propositions[0].text, "result f x = x + 1");
    assert.equal(result.typeOverrides.length, 0);
  });

  it("extracts multiple @pant propositions", () => {
    const node = functionWithJsDoc(
      " * @pant result f x >= 0\n * @pant result f x <= 100",
    );
    const result = extractAnnotations(node);
    assert.equal(result.propositions.length, 2);
    assert.equal(result.propositions[0].text, "result f x >= 0");
    assert.equal(result.propositions[1].text, "result f x <= 100");
  });

  it("extracts a multi-line @pant-begin / @pant-end block", () => {
    const node = functionWithJsDoc(
      " * @pant-begin\n" +
        " * all x: Nat |\n" +
        " *   result f x >= 0\n" +
        " * @pant-end",
    );
    const result = extractAnnotations(node);
    assert.equal(result.propositions.length, 1);
    assert.ok(result.propositions[0].text.includes("all x: Nat |"));
    assert.ok(result.propositions[0].text.includes("result f x >= 0"));
  });

  it("extracts @pant-type overrides", () => {
    const node = functionWithJsDoc(
      " * @pant-type amount: Nat\n * @pant-type rate: Real",
    );
    const result = extractAnnotations(node);
    assert.equal(result.propositions.length, 0);
    assert.equal(result.typeOverrides.length, 2);
    assert.deepEqual(result.typeOverrides[0], { name: "amount", type: "Nat" });
    assert.deepEqual(result.typeOverrides[1], { name: "rate", type: "Real" });
  });

  it("resets @pant-begin state for each JSDoc block", () => {
    const node = firstFunction(
      "/**\n" +
        " * @pant-begin\n" +
        " * unterminated block\n" +
        " */\n" +
        "/**\n" +
        " * @pant result subject = 1\n" +
        " * @pant-type value: Nat\n" +
        " */\n" +
        "function subject(): void {}\n",
    );
    const result = extractAnnotations(node);
    assert.deepEqual(
      result.propositions.map((p) => p.text),
      ["result subject = 1"],
    );
    assert.deepEqual(result.typeOverrides, [{ name: "value", type: "Nat" }]);
  });

  it("returns empty result for JSDoc without @pant tags", () => {
    const node = functionWithJsDoc(
      " * Just a regular JSDoc comment.\n" +
        " * @param x - a number\n" +
        " * @returns the result",
    );
    const result = extractAnnotations(node);
    assert.equal(result.propositions.length, 0);
    assert.equal(result.typeOverrides.length, 0);
  });

  it("skips empty @pant tags", () => {
    const node = functionWithJsDoc(" * @pant");
    const result = extractAnnotations(node);
    assert.equal(result.propositions.length, 0);
  });

  it("handles mixed annotations and type overrides", () => {
    const node = functionWithJsDoc(
      " * @pant-type n: Nat0\n" +
        " * @pant result factorial n >= 1\n" +
        " * @pant-begin\n" +
        " * all m: Nat0 |\n" +
        " *   result factorial m >= 1\n" +
        " * @pant-end",
    );
    const result = extractAnnotations(node);
    assert.equal(result.typeOverrides.length, 1);
    assert.deepEqual(result.typeOverrides[0], { name: "n", type: "Nat0" });
    assert.equal(result.propositions.length, 2);
    assert.equal(result.propositions[0].text, "result factorial n >= 1");
    assert.ok(result.propositions[1].text.includes("all m: Nat0 |"));
    assert.ok(result.propositions[1].text.includes("result factorial m >= 1"));
  });

  it("ignores empty @pant-begin / @pant-end blocks", () => {
    const node = functionWithJsDoc(" * @pant-begin\n * @pant-end");
    const result = extractAnnotations(node);
    assert.equal(result.propositions.length, 0);
  });

  it("returns empty for function with no JSDoc", () => {
    const node = firstFunction("function noop(): void {}\n");
    const result = extractAnnotations(node);
    assert.equal(result.propositions.length, 0);
    assert.equal(result.typeOverrides.length, 0);
  });

  it("ignores plain (non-JSDoc) comments", () => {
    const node = firstFunction(
      "// @pant this should not be extracted\nfunction noop(): void {}\n",
    );
    const result = extractAnnotations(node);
    assert.equal(result.propositions.length, 0);
  });

  it("function annotation wrappers agree on generated JSDoc", () => {
    fc.assert(
      fc.property(
        fc.constantFrom("x", "value", "amount", "count"),
        fc.stringMatching(/^[a-z][a-z0-9]{0,6}$/u),
        fc.constantFrom("Nat", "Nat0", "Int", "Real"),
        (param, rule, pantType) => {
          const proposition = `${rule} ${param} = ${param}`;
          const sourceFile = createSourceFileFromSource(
            `/**\n * @pant ${proposition}\n * @pant-type ${param}: ${pantType}\n */\nexport function subject(${param}: number): number { return ${param}; }\n`,
          );

          const combined = extractFunctionAnnotationsAndOverrides(
            sourceFile,
            "subject",
          );

          assert.deepEqual(extractFunctionAnnotations(sourceFile, "subject"), [
            proposition,
          ]);
          assert.equal(
            extractFunctionTypeOverrides(sourceFile, "subject").get(param),
            pantType,
          );
          assert.deepEqual(combined.propositionTexts, [proposition]);
          assert.equal(combined.typeOverrides.get(param), pantType);
        },
      ),
    );
  });
});
