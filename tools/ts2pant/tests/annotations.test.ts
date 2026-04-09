import { describe, it, expect } from "vitest";
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
    expect(result.propositions.length).toBe(1);
    expect(result.propositions[0].text).toBe("result f x = x + 1");
    expect(result.typeOverrides.length).toBe(0);
  });

  it("extracts multiple @pant propositions", () => {
    const text = `
     * @pant result f x >= 0
     * @pant result f x <= 100
     `;
    const result = parseAnnotations(text);
    expect(result.propositions.length).toBe(2);
    expect(result.propositions[0].text).toBe("result f x >= 0");
    expect(result.propositions[1].text).toBe("result f x <= 100");
  });

  it("extracts a multi-line @pant-begin / @pant-end block", () => {
    const text = `
     * @pant-begin
     * all x: Nat |
     *   result f x >= 0
     * @pant-end
     `;
    const result = parseAnnotations(text);
    expect(result.propositions.length).toBe(1);
    expect(result.propositions[0].text).toBe("all x: Nat |\n  result f x >= 0");
  });

  it("extracts @pant-type overrides", () => {
    const text = `
     * @pant-type amount: Nat
     * @pant-type rate: Real
     `;
    const result = parseAnnotations(text);
    expect(result.propositions.length).toBe(0);
    expect(result.typeOverrides.length).toBe(2);
    expect(result.typeOverrides[0].name).toBe("amount");
    expect(result.typeOverrides[0].type).toBe("Nat");
    expect(result.typeOverrides[1].name).toBe("rate");
    expect(result.typeOverrides[1].type).toBe("Real");
  });

  it("returns empty result for no annotations", () => {
    const text = `
     * Just a regular JSDoc comment.
     * @param x - a number
     * @returns the result
     `;
    const result = parseAnnotations(text);
    expect(result.propositions.length).toBe(0);
    expect(result.typeOverrides.length).toBe(0);
  });

  it("skips empty @pant tags", () => {
    const text = `
     * @pant
     `;
    const result = parseAnnotations(text);
    expect(result.propositions.length).toBe(0);
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
    expect(result.typeOverrides.length).toBe(1);
    expect(result.typeOverrides[0].name).toBe("n");
    expect(result.typeOverrides[0].type).toBe("Nat0");
    expect(result.propositions.length).toBe(2);
    expect(result.propositions[0].text).toBe("result factorial n >= 1");
    expect(result.propositions[1].text).toBe(
      "all m: Nat0 |\n  result factorial m >= 1",
    );
  });

  it("ignores empty @pant-begin / @pant-end blocks", () => {
    const text = `
     * @pant-begin
     * @pant-end
     `;
    const result = parseAnnotations(text);
    expect(result.propositions.length).toBe(0);
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
    expect(result.propositions.length).toBe(1);
    expect(result.propositions[0].text).toBe("result add a b = a + b");
  });

  it("returns empty for function with no JSDoc", () => {
    const source = `
function noop(): void {}
`;
    const { node, sourceFile } = firstFunction(source);
    const result = extractAnnotations(node, sourceFile);
    expect(result.propositions.length).toBe(0);
    expect(result.typeOverrides.length).toBe(0);
  });

  it("returns empty for function with plain (non-JSDoc) comment", () => {
    const source = `
// @pant this should not be extracted
function noop(): void {}
`;
    const { node, sourceFile } = firstFunction(source);
    const result = extractAnnotations(node, sourceFile);
    expect(result.propositions.length).toBe(0);
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
    expect(result.typeOverrides.length).toBe(1);
    expect(result.typeOverrides[0].name).toBe("amount");
    expect(result.typeOverrides[0].type).toBe("Nat");
    expect(result.propositions.length).toBe(1);
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
    expect(result.propositions.length).toBe(1);
    expect(result.propositions[0].text).toContain("all x: Nat |");
    expect(result.propositions[0].text).toContain("result f x > 0");
  });
});
