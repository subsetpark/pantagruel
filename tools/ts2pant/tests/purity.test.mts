import { describe, it } from "node:test";
import assert from "node:assert/strict";
import ts from "typescript";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { isKnownPureCall } from "../src/purity.js";

/**
 * Find the first CallExpression in a source file's function body.
 * Searches depth-first through all function declarations.
 */
function findCallExpression(
  sourceFile: ts.SourceFile,
): ts.CallExpression | undefined {
  let result: ts.CallExpression | undefined;
  function visit(node: ts.Node) {
    if (result) return;
    if (ts.isCallExpression(node)) {
      result = node;
      return;
    }
    ts.forEachChild(node, visit);
  }
  ts.forEachChild(sourceFile, visit);
  return result;
}

/**
 * Helper: create a source with a function containing the expression,
 * extract the call expression and checker, and return isKnownPureCall result.
 */
function checkPurity(source: string): boolean {
  const sf = createSourceFileFromSource(source);
  const checker = getChecker(sf);
  const callExpr = findCallExpression(sf.compilerNode);
  if (!callExpr) {
    throw new Error("No CallExpression found in source");
  }
  return isKnownPureCall(callExpr, checker);
}

describe("isKnownPureCall", () => {
  // --- Tier 1a: Pure namespaces ---

  it("should return true for Math.max", () => {
    assert.equal(
      checkPurity(`
        function f(a: number, b: number) { return Math.max(a, b); }
      `),
      true,
    );
  });

  it("should return true for Math.abs", () => {
    assert.equal(
      checkPurity(`
        function f(x: number) { return Math.abs(x); }
      `),
      true,
    );
  });

  // --- Tier 1a: String methods ---

  it("should return true for String.prototype.indexOf", () => {
    assert.equal(
      checkPurity(`
        function f(s: string) { return s.indexOf("x"); }
      `),
      true,
    );
  });

  // --- Tier 1a: Array methods (no callback) ---

  it("should return true for Array.prototype.includes", () => {
    assert.equal(
      checkPurity(`
        function f(arr: number[]) { return arr.includes(1); }
      `),
      true,
    );
  });

  // --- Tier 1a: Higher-order array methods with arrow callbacks ---

  it("should return true for array.filter with pure callback", () => {
    assert.equal(
      checkPurity(`
        function f(arr: number[]) { return arr.filter(x => x > 0); }
      `),
      true,
    );
  });

  it("should return false for array.filter with impure callback", () => {
    assert.equal(
      checkPurity(`
        declare function sideEffect(x: number): boolean;
        function f(arr: number[]) { return arr.filter(x => sideEffect(x)); }
      `),
      false,
    );
  });

  it("should return true for array.map with pure arrow callback", () => {
    assert.equal(
      checkPurity(`
        function f(arr: number[]) { return arr.map(x => x * 2); }
      `),
      true,
    );
  });

  it("should return false for array.map with impure arrow callback", () => {
    assert.equal(
      checkPurity(`
        declare function transform(x: number): number;
        function f(arr: number[]) { return arr.map(x => transform(x)); }
      `),
      false,
    );
  });

  it("should return true for array.filter with expression-bodied arrow", () => {
    assert.equal(
      checkPurity(`
        function f(arr: string[]) { return arr.filter(s => s.length > 0); }
      `),
      true,
    );
  });

  it("should return false for array.filter with block-bodied arrow containing side effect", () => {
    assert.equal(
      checkPurity(`
        declare function log(x: number): void;
        function f(arr: number[]) {
          return arr.filter(x => { log(x); return x > 0; });
        }
      `),
      false,
    );
  });

  // --- Tier 1b: Effect-TS ---

  it("should return true for Effect-returning function", () => {
    // Simulate Effect type via structural branded type
    assert.equal(
      checkPurity(`
        interface Effect<A, E, R> {
          readonly EffectTypeId: unique symbol;
          readonly _A: A;
          readonly _E: E;
          readonly _R: R;
        }
        declare function succeed<A>(value: A): Effect<A, never, never>;
        function f(x: number) { return succeed(x); }
      `),
      true,
    );
  });

  it("should return false for Effect.runSync", () => {
    // Even if it returns Effect, runSync is impure
    assert.equal(
      checkPurity(`
        interface Effect<A, E, R> {
          readonly EffectTypeId: unique symbol;
          readonly _A: A;
          readonly _E: E;
          readonly _R: R;
        }
        declare const Effect: {
          runSync<A>(effect: Effect<A, never, never>): A;
        };
        declare const eff: Effect<number, never, never>;
        function f() { return Effect.runSync(eff); }
      `),
      false,
    );
  });

  it("should return true for pipe from effect/Function", () => {
    assert.equal(
      checkPurity(`
        declare function pipe<A, B>(a: A, f: (a: A) => B): B;
        function f(x: number) { return pipe(x, n => n + 1); }
      `),
      true,
    );
  });

  // --- Tier 1c: Conservative default ---

  it("should return false for unknown function", () => {
    assert.equal(
      checkPurity(`
        declare function unknownFn(x: number): number;
        function f(x: number) { return unknownFn(x); }
      `),
      false,
    );
  });
});
