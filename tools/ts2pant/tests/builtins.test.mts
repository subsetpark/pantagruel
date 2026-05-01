import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { Project } from "ts-morph";
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
  predicate?: (call: ts.CallExpression) => boolean,
): ts.CallExpression | undefined {
  let result: ts.CallExpression | undefined;
  function visit(node: ts.Node): void {
    if (result) return;
    if (ts.isCallExpression(node) && (!predicate || predicate(node))) {
      result = node;
      return;
    }
    ts.forEachChild(node, visit);
  }
  ts.forEachChild(sourceFile, visit);
  return result;
}

function lookupFromSource(
  source: string,
  predicate?: (call: ts.CallExpression) => boolean,
) {
  const sf = createSourceFileFromSource(source);
  const checker = getChecker(sf);
  const program = sf.getProject().getProgram().compilerObject;
  const call = findCallExpression(sf.compilerNode, predicate);
  if (!call) throw new Error("No CallExpression found in source");
  return lookupBuiltinByCall(call, checker, program);
}

describe("builtins dispatch", () => {
  it("Math.max maps to JS_MATH::max-of in JS_MATH", () => {
    const spec = lookupFromSource(`
      function f(a: number, b: number) { return Math.max(a, b); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_MATH::max-of",
      mod: "JS_MATH",
      arity: 2,
    });
  });

  it("Math.abs maps to JS_MATH::abs", () => {
    const spec = lookupFromSource(`
      function f(x: number) { return Math.abs(x); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_MATH::abs",
      mod: "JS_MATH",
      arity: 1,
    });
  });

  it("s.toUpperCase() resolves to JS_STRING::to-upper-case", () => {
    const spec = lookupFromSource(`
      function f(s: string) { return s.toUpperCase(); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_STRING::to-upper-case",
      mod: "JS_STRING",
      arity: 0,
    });
  });

  it("s.indexOf(t) resolves to JS_STRING::index-of", () => {
    const spec = lookupFromSource(`
      function f(s: string, t: string) { return s.indexOf(t); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_STRING::index-of",
      mod: "JS_STRING",
      arity: 1,
    });
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

  it("Math.max with arity != 2 falls through to null (variadic JS form)", () => {
    // Math.max accepts any number of args in JS but the JS_MATH::max-of
    // rule is binary; off-arity calls must reach the EUF lowering rather
    // than mis-dispatching against a fixed-arity Pant rule.
    assert.equal(
      lookupFromSource(`
        function f(a: number, b: number, c: number) { return Math.max(a, b, c); }
      `),
      null,
    );
    assert.equal(
      lookupFromSource(`
        function f() { return Math.max(); }
      `),
      null,
    );
  });

  it("s.indexOf with the optional fromIndex falls through (rule has no fromIndex)", () => {
    const spec = lookupFromSource(`
      function f(s: string, t: string) { return s.indexOf(t, 1); }
    `);
    assert.equal(spec, null);
  });

  it("Math.abs() with no args falls through", () => {
    const spec = lookupFromSource(`
      function f() { return Math.abs(); }
    `);
    assert.equal(spec, null);
  });

  it("user `interface String` augmentation does not match a method shadowed in the augmentation", () => {
    // A project-local .d.ts that augments `interface String` with a
    // method of the same name as a real String.prototype method
    // would, under the old `isDeclarationFile`-based check, still
    // route to the JS_STRING dispatch (the lib decl is also present
    // in the symbol's declarations array). Default-library detection
    // via Program.isSourceFileDefaultLibrary is the right gate, but
    // the inverse direction — a method that exists *only* in a user
    // .d.ts — is the one that has to be rejected, since matching it
    // against JS_STRING would silently mis-emit.
    const project = new Project({
      compilerOptions: {
        target: ts.ScriptTarget.ES2022,
        module: ts.ModuleKind.NodeNext,
        moduleResolution: ts.ModuleResolutionKind.NodeNext,
        strict: true,
      },
      useInMemoryFileSystem: true,
    });
    project.createSourceFile(
      "augment.d.ts",
      "interface String { onlyInUserDts(): string; }\n",
    );
    const sf = project.createSourceFile(
      "main.ts",
      "function f(s: string) { return s.onlyInUserDts(); }\n",
    );
    const checker = project.getTypeChecker().compilerObject;
    const program = project.getProgram().compilerObject;
    const call = findCallExpression(sf.compilerNode);
    if (!call) throw new Error("expected a call");
    // No BUILTINS entry exists for `onlyInUserDts`, so the lookup
    // returns null even though `isStringPrototypeMember` walks the
    // method symbol's declarations (which include the user .d.ts
    // augmentation). The point is that lib-only filtering keeps the
    // *parent String-interface* match honest — a user augmentation
    // could not retroactively name a method that masquerades as a
    // BUILTINS key (e.g., reusing "toUpperCase") and steal dispatch
    // away from the lib version, because lib filtering would still
    // accept the lib decl on the merged symbol.
    assert.equal(lookupBuiltinByCall(call, checker, program), null);
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
  for (const name of [
    "JS_MATH",
    "JS_STRING",
    "TS_PRELUDE",
  ] satisfies DepModuleName[]) {
    it(`samples/js-stdlib/${name}.pant typechecks standalone via wasm`, async () => {
      await assertWasmTypeChecks(loadBuiltinDepModule(name));
    });
  }
});
