import { describe, it } from "node:test";
import assert from "node:assert/strict";
import ts from "typescript";
import {
  parseAnnotations,
  extractAnnotations,
  type AnnotationResult,
} from "../src/annotations.js";

// ---------------------------------------------------------------------------
// Helper: create a SourceFile from raw TS source and find the first function
// ---------------------------------------------------------------------------
function firstFunction(source: string): {
  node: ts.FunctionDeclaration;
  sourceFile: ts.SourceFile;
} {
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
  if (!fn) throw new Error("No function declaration found in source");
  return { node: fn, sourceFile };
}

// ---------------------------------------------------------------------------
// parseAnnotations — raw text parsing
// ---------------------------------------------------------------------------
describe("parseAnnotations", () => {
  it("extracts a single @pant proposition", () => {
    const text = `
     * Some description.
     * @pant result f x = x + 1
     `;
    const result = parseAnnotations(text);
    assert.equal(result.propositions.length, 1);
    assert.equal(result.propositions[0].text, "result f x = x + 1");
    assert.equal(result.typeOverrides.length, 0);
  });

  it("extracts multiple @pant propositions", () => {
    const text = `
     * @pant result f x >= 0
     * @pant result f x <= 100
     `;
    const result = parseAnnotations(text);
    assert.equal(result.propositions.length, 2);
    assert.equal(result.propositions[0].text, "result f x >= 0");
    assert.equal(result.propositions[1].text, "result f x <= 100");
  });

  it("extracts a multi-line @pant-begin / @pant-end block", () => {
    const text = `
     * @pant-begin
     * all x: Nat |
     *   result f x >= 0
     * @pant-end
     `;
    const result = parseAnnotations(text);
    assert.equal(result.propositions.length, 1);
    assert.equal(result.propositions[0].text, "all x: Nat |\n  result f x >= 0");
  });

  it("extracts @pant-type overrides", () => {
    const text = `
     * @pant-type amount: Nat
     * @pant-type rate: Real
     `;
    const result = parseAnnotations(text);
    assert.equal(result.propositions.length, 0);
    assert.equal(result.typeOverrides.length, 2);
    assert.equal(result.typeOverrides[0].name, "amount");
    assert.equal(result.typeOverrides[0].type, "Nat");
    assert.equal(result.typeOverrides[1].name, "rate");
    assert.equal(result.typeOverrides[1].type, "Real");
  });

  it("returns empty result for no annotations", () => {
    const text = `
     * Just a regular JSDoc comment.
     * @param x - a number
     * @returns the result
     `;
    const result = parseAnnotations(text);
    assert.equal(result.propositions.length, 0);
    assert.equal(result.typeOverrides.length, 0);
  });

  it("skips empty @pant tags", () => {
    const text = `
     * @pant
     `;
    const result = parseAnnotations(text);
    assert.equal(result.propositions.length, 0);
  });

  it("handles mixed annotations and type overrides", () => {
    const text = `
     * @pant-type n: Nat0
     * @pant result factorial n >= 1
     * @pant-begin
     * all m: Nat0 |
     *   result factorial m >= 1
     * @pant-end
     `;
    const result = parseAnnotations(text);
    assert.equal(result.typeOverrides.length, 1);
    assert.equal(result.typeOverrides[0].name, "n");
    assert.equal(result.typeOverrides[0].type, "Nat0");
    assert.equal(result.propositions.length, 2);
    assert.equal(result.propositions[0].text, "result factorial n >= 1");
    assert.equal(result.propositions[1].text,
      "all m: Nat0 |\n  result factorial m >= 1",
    );
  });

  it("ignores empty @pant-begin / @pant-end blocks", () => {
    const text = `
     * @pant-begin
     * @pant-end
     `;
    const result = parseAnnotations(text);
    assert.equal(result.propositions.length, 0);
  });
});

// ---------------------------------------------------------------------------
// extractAnnotations — TS compiler API integration
// ---------------------------------------------------------------------------
describe("extractAnnotations", () => {
  it("extracts annotations from a JSDoc comment on a function", () => {
    const source = `
/**
 * Add two numbers.
 * @pant result add a b = a + b
 */
function add(a: number, b: number): number {
  return a + b;
}
`;
    const { node, sourceFile } = firstFunction(source);
    const result = extractAnnotations(node, sourceFile);
    assert.equal(result.propositions.length, 1);
    assert.equal(result.propositions[0].text, "result add a b = a + b");
  });

  it("returns empty for function with no JSDoc", () => {
    const source = `
function noop(): void {}
`;
    const { node, sourceFile } = firstFunction(source);
    const result = extractAnnotations(node, sourceFile);
    assert.equal(result.propositions.length, 0);
    assert.equal(result.typeOverrides.length, 0);
  });

  it("returns empty for function with plain (non-JSDoc) comment", () => {
    const source = `
// @pant this should not be extracted
function noop(): void {}
`;
    const { node, sourceFile } = firstFunction(source);
    const result = extractAnnotations(node, sourceFile);
    assert.equal(result.propositions.length, 0);
  });

  it("extracts @pant-type from JSDoc on a function", () => {
    const source = `
/**
 * @pant-type amount: Nat
 * @pant result withdraw amount >= 0
 */
function withdraw(amount: number): number {
  return amount;
}
`;
    const { node, sourceFile } = firstFunction(source);
    const result = extractAnnotations(node, sourceFile);
    assert.equal(result.typeOverrides.length, 1);
    assert.equal(result.typeOverrides[0].name, "amount");
    assert.equal(result.typeOverrides[0].type, "Nat");
    assert.equal(result.propositions.length, 1);
  });

  it("extracts multi-line block from JSDoc", () => {
    const source = `
/**
 * @pant-begin
 * all x: Nat |
 *   result f x > 0
 * @pant-end
 */
function f(x: number): number {
  return x + 1;
}
`;
    const { node, sourceFile } = firstFunction(source);
    const result = extractAnnotations(node, sourceFile);
    assert.equal(result.propositions.length, 1);
    assert.ok(result.propositions[0].text.includes("all x: Nat |"));
    assert.ok(result.propositions[0].text.includes("result f x > 0"));
  });
});
