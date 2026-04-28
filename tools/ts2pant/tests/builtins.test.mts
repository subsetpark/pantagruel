import { describe, it } from "node:test";
import assert from "node:assert/strict";
import ts from "typescript";
import {
  type DepModuleName,
  loadBuiltinDepModule,
  lookupBuiltinByCall,
} from "../src/builtins.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { assertWasmTypeChecks } from "../src/pant-wasm.js";

function findCallExpression(
  sourceFile: ts.SourceFile,
): ts.CallExpression | undefined {
  let result: ts.CallExpression | undefined;
  function visit(node: ts.Node): void {
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

function lookupFromSource(source: string) {
  const sf = createSourceFileFromSource(source);
  const checker = getChecker(sf);
  const call = findCallExpression(sf.compilerNode);
  if (!call) throw new Error("No CallExpression found in source");
  return lookupBuiltinByCall(call, checker);
}

describe("builtins dispatch", () => {
  it("Math.max maps to JS_MATH::max-of in JS_MATH", () => {
    const spec = lookupFromSource(`
      function f(a: number, b: number) { return Math.max(a, b); }
    `);
    assert.deepEqual(spec, { rule: "JS_MATH::max-of", mod: "JS_MATH" });
  });

  it("Math.abs maps to JS_MATH::abs", () => {
    const spec = lookupFromSource(`
      function f(x: number) { return Math.abs(x); }
    `);
    assert.deepEqual(spec, { rule: "JS_MATH::abs", mod: "JS_MATH" });
  });

  it("s.toUpperCase() resolves to JS_STRING::to-upper-case", () => {
    const spec = lookupFromSource(`
      function f(s: string) { return s.toUpperCase(); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_STRING::to-upper-case",
      mod: "JS_STRING",
    });
  });

  it("s.indexOf(t) resolves to JS_STRING::index-of", () => {
    const spec = lookupFromSource(`
      function f(s: string, t: string) { return s.indexOf(t); }
    `);
    assert.deepEqual(spec, { rule: "JS_STRING::index-of", mod: "JS_STRING" });
  });

  it("user-shadowed Math identifier does not match Math.max (symbol-based dispatch)", () => {
    // The shadowing `const Math` declares a fresh symbol whose declaration
    // sits in the user's source file, not in a TS lib .d.ts. The lookup
    // therefore rejects, and the call would fall through to ts2pant's
    // ordinary EUF lowering.
    const spec = lookupFromSource(`
      function f(a: number, b: number) {
        const Math = { max: (x: number, y: number) => x };
        return Math.max(a, b);
      }
    `);
    assert.equal(spec, null);
  });

  it("returns null for non-builtin calls", () => {
    const spec = lookupFromSource(`
      function f() { return Math.random(); }
    `);
    assert.equal(spec, null);
  });

  it("returns null for unrelated method calls on non-string receivers", () => {
    const spec = lookupFromSource(`
      function f(a: number[]) { return a.indexOf(0); }
    `);
    assert.equal(spec, null);
  });
});

describe("loadBuiltinDepModule", () => {
  it("loadBuiltinDepModule('JS_MATH') returns the on-disk text", () => {
    const text = loadBuiltinDepModule("JS_MATH");
    assert.match(text, /^module JS_MATH\./m);
    assert.match(text, /max-of a: Int, b: Int => Int\./);
    assert.match(text, /abs x: Int => Int\./);
  });

  it("loadBuiltinDepModule('JS_STRING') returns the on-disk text", () => {
    const text = loadBuiltinDepModule("JS_STRING");
    assert.match(text, /^module JS_STRING\./m);
    assert.match(text, /to-upper-case s: String => String\./);
    assert.match(text, /index-of s: String, t: String => Int\./);
  });

  it("caches the loaded module text across calls", () => {
    const a = loadBuiltinDepModule("JS_MATH");
    const b = loadBuiltinDepModule("JS_MATH");
    assert.equal(a, b);
  });
});

describe("hand-written js-stdlib modules typecheck", () => {
  for (const name of ["JS_MATH", "JS_STRING"] satisfies DepModuleName[]) {
    it(`samples/js-stdlib/${name}.pant typechecks standalone via wasm`, async () => {
      await assertWasmTypeChecks(loadBuiltinDepModule(name));
    });
  }
});
