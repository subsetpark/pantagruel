import { describe, it } from "node:test";
import assert from "node:assert/strict";
import ts from "typescript";
import { extractAnnotations } from "../src/annotations.js";

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
  if (!fn) throw new Error("No function declaration found in source");
  return fn;
}

/** Wrap a JSDoc body in `/** ... *\/` + a dummy function declaration. */
function functionWithJsDoc(body: string): ts.FunctionDeclaration {
  return firstFunction(
    `/**\n${body}\n */\nfunction subject(): void {}\n`,
  );
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
});
