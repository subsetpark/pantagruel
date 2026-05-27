import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { Project } from "ts-morph";
import ts from "typescript";
import {
  deriveBuiltinSpec,
  type DepModuleName,
  loadBuiltinDepModule,
  lookupBuiltinByCall,
} from "../src/builtins.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { assertWasmTypeChecks } from "../src/pant-wasm.js";
import {
  buildDocumentFromSourceFile,
  emitAndCheck,
} from "./helpers.mjs";

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

async function emitFromSource(source: string, functionName: string) {
  const sourceFile = createSourceFileFromSource(source);
  const doc = await buildDocumentFromSourceFile(sourceFile, functionName);
  return emitAndCheck(doc);
}

describe("deriveBuiltinSpec", () => {
  it("maps Array.* keys to JS_ARRAY", () => {
    assert.deepEqual(deriveBuiltinSpec("Array.from", 1), {
      rule: "JS_ARRAY::from",
      mod: "JS_ARRAY",
      arity: 1,
      receiver: "none",
    });
  });

  it("maps Map.prototype.* keys to JS_MAP", () => {
    assert.deepEqual(deriveBuiltinSpec("Map.prototype.values", 0), {
      rule: "JS_MAP::values",
      mod: "JS_MAP",
      arity: 0,
      receiver: "arg",
    });
  });

  it("maps Math.* keys to JS_MATH", () => {
    assert.deepEqual(deriveBuiltinSpec("Math.foo", 1), {
      rule: "JS_MATH::foo",
      mod: "JS_MATH",
      arity: 1,
      receiver: "none",
    });
  });

  it("maps String.prototype.* keys to JS_STRING", () => {
    assert.deepEqual(deriveBuiltinSpec("String.prototype.foo", 1), {
      rule: "JS_STRING::foo",
      mod: "JS_STRING",
      arity: 1,
      receiver: "arg",
    });
  });

  it("lowers toUpperCase to kebab-case", () => {
    assert.deepEqual(deriveBuiltinSpec("String.prototype.toUpperCase", 0), {
      rule: "JS_STRING::to-upper-case",
      mod: "JS_STRING",
      arity: 0,
      receiver: "arg",
    });
  });

  it("lowers indexOf to kebab-case", () => {
    assert.deepEqual(deriveBuiltinSpec("String.prototype.indexOf", 1), {
      rule: "JS_STRING::index-of",
      mod: "JS_STRING",
      arity: 1,
      receiver: "arg",
    });
  });

  it("lowers lastIndexOf to kebab-case", () => {
    assert.deepEqual(deriveBuiltinSpec("String.prototype.lastIndexOf", 1), {
      rule: "JS_STRING::last-index-of",
      mod: "JS_STRING",
      arity: 1,
      receiver: "arg",
    });
  });

  it("suffixes reserved max rule name", () => {
    assert.deepEqual(deriveBuiltinSpec("Math.max", 2), {
      rule: "JS_MATH::max-of",
      mod: "JS_MATH",
      arity: 2,
      receiver: "none",
    });
  });

  it("suffixes reserved min rule name", () => {
    assert.deepEqual(deriveBuiltinSpec("Math.min", 2), {
      rule: "JS_MATH::min-of",
      mod: "JS_MATH",
      arity: 2,
      receiver: "none",
    });
  });

  it("composes the full spec for existing entries", () => {
    assert.deepEqual(deriveBuiltinSpec("Math.abs", 1), {
      rule: "JS_MATH::abs",
      mod: "JS_MATH",
      arity: 1,
      receiver: "none",
    });
    assert.deepEqual(deriveBuiltinSpec("String.prototype.indexOf", 1), {
      rule: "JS_STRING::index-of",
      mod: "JS_STRING",
      arity: 1,
      receiver: "arg",
    });
  });

  it("composes the full spec for new entries", () => {
    assert.deepEqual(deriveBuiltinSpec("String.prototype.replace", 2), {
      rule: "JS_STRING::replace",
      mod: "JS_STRING",
      arity: 2,
      receiver: "arg",
    });
    assert.deepEqual(deriveBuiltinSpec("Array.from", 1), {
      rule: "JS_ARRAY::from",
      mod: "JS_ARRAY",
      arity: 1,
      receiver: "none",
    });
    assert.deepEqual(deriveBuiltinSpec("Map.prototype.entries", 0), {
      rule: "JS_MAP::entries",
      mod: "JS_MAP",
      arity: 0,
      receiver: "arg",
    });
  });
});

describe("builtins dispatch", () => {
  it("Math.max maps to JS_MATH::max-of in JS_MATH", () => {
    const spec = lookupFromSource(`
      function f(a: number, b: number) { return Math.max(a, b); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_MATH::max-of",
      mod: "JS_MATH",
      arity: 2,
      receiver: "none",
    });
  });

  it("Math.min maps to JS_MATH::min-of", () => {
    const spec = lookupFromSource(`
      function f(a: number, b: number) { return Math.min(a, b); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_MATH::min-of",
      mod: "JS_MATH",
      arity: 2,
      receiver: "none",
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
      receiver: "none",
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
      receiver: "arg",
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
      receiver: "arg",
    });
  });

  it("s.endsWith(q) resolves to JS_STRING::ends-with", () => {
    const spec = lookupFromSource(`
      function f(s: string, q: string) { return s.endsWith(q); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_STRING::ends-with",
      mod: "JS_STRING",
      arity: 1,
      receiver: "arg",
    });
  });

  it("s.includes(t) resolves to JS_STRING::includes", () => {
    const spec = lookupFromSource(`
      function f(s: string, t: string) { return s.includes(t); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_STRING::includes",
      mod: "JS_STRING",
      arity: 1,
      receiver: "arg",
    });
  });

  it("s.lastIndexOf(t) resolves to JS_STRING::last-index-of", () => {
    const spec = lookupFromSource(`
      function f(s: string, t: string) { return s.lastIndexOf(t); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_STRING::last-index-of",
      mod: "JS_STRING",
      arity: 1,
      receiver: "arg",
    });
  });

  it("s.startsWith(p) resolves to JS_STRING::starts-with", () => {
    const spec = lookupFromSource(`
      function f(s: string, p: string) { return s.startsWith(p); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_STRING::starts-with",
      mod: "JS_STRING",
      arity: 1,
      receiver: "arg",
    });
  });

  it("s.toLowerCase() resolves to JS_STRING::to-lower-case", () => {
    const spec = lookupFromSource(`
      function f(s: string) { return s.toLowerCase(); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_STRING::to-lower-case",
      mod: "JS_STRING",
      arity: 0,
      receiver: "arg",
    });
  });

  it("s.trim() resolves to JS_STRING::trim", () => {
    const spec = lookupFromSource(`
      function f(s: string) { return s.trim(); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_STRING::trim",
      mod: "JS_STRING",
      arity: 0,
      receiver: "arg",
    });
  });

  it("Array.from resolves to JS_ARRAY::from", () => {
    const spec = lookupFromSource(`
      function f(xs: number[]) { return Array.from(xs); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_ARRAY::from",
      mod: "JS_ARRAY",
      arity: 1,
      receiver: "none",
    });
  });

  it("Map.prototype.values resolves to JS_MAP::values", () => {
    const spec = lookupFromSource(`
      function f(m: Map<string, number>) { return m.values(); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_MAP::values",
      mod: "JS_MAP",
      arity: 0,
      receiver: "arg",
    });
  });

  it("Map.prototype.entries resolves to JS_MAP::entries", () => {
    const spec = lookupFromSource(`
      function f(m: Map<string, number>) { return m.entries(); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_MAP::entries",
      mod: "JS_MAP",
      arity: 0,
      receiver: "arg",
    });
  });

  it("Map.prototype.keys resolves to JS_MAP::keys", () => {
    const spec = lookupFromSource(`
      function f(m: Map<string, number>) { return m.keys(); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_MAP::keys",
      mod: "JS_MAP",
      arity: 0,
      receiver: "arg",
    });
  });

  it("String.prototype.replace resolves for string arguments", () => {
    const spec = lookupFromSource(`
      function f(s: string) { return s.replace("a", "b"); }
    `);
    assert.deepEqual(spec, {
      rule: "JS_STRING::replace",
      mod: "JS_STRING",
      arity: 2,
      receiver: "arg",
    });
  });

  it("String.prototype.replace with a RegExp first argument falls through", () => {
    const spec = lookupFromSource(`
      function f(s: string) { return s.replace(/a/g, "b"); }
    `);
    assert.equal(spec, null);
  });

  it("user-shadowed Array identifier does not match Array.from", () => {
    const spec = lookupFromSource(`
      function f(xs: number[]) {
        const Array = { from: (ys: number[]) => ys };
        return Array.from(xs);
      }
    `);
    assert.equal(spec, null);
  });

  it("Array.from with arity != 1 falls through", () => {
    assert.equal(
      lookupFromSource(`
        function f() { return Array.from(); }
      `),
      null,
    );
    assert.equal(
      lookupFromSource(`
        function f(xs: number[]) { return Array.from(xs, (x) => x); }
      `),
      null,
    );
  });

  it("Map iterator methods with arguments fall through", () => {
    const spec = lookupFromSource(`
      function f(m: Map<string, number>) { return m.values("unexpected"); }
    `);
    assert.equal(spec, null);
  });

  it("non-Map .values() calls fall through", () => {
    const spec = lookupFromSource(`
      interface Box { values(): number[]; }
      function f(box: Box) { return box.values(); }
    `);
    assert.equal(spec, null);
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
    assert.match(text, /min-of a: Int, b: Int => Int\./);
    assert.match(text, /abs x: Int => Int\./);
  });

  it("loadBuiltinDepModule('JS_STRING') returns the on-disk text", () => {
    const text = loadBuiltinDepModule("JS_STRING");
    assert.match(text, /^module JS_STRING\./m);
    assert.match(text, /to-upper-case s: String => String\./);
    assert.match(text, /index-of s: String, t: String => Int\./);
    assert.match(text, /to-lower-case s: String => String\./);
    assert.match(text, /trim s: String => String\./);
    assert.match(text, /includes s: String, t: String => Bool\./);
    assert.match(text, /starts-with s: String, prefix: String => Bool\./);
    assert.match(text, /ends-with s: String, suffix: String => Bool\./);
    assert.match(text, /last-index-of s: String, t: String => Int\./);
    assert.match(
      text,
      /replace s: String, from: String, to: String => String\./,
    );
  });

  it("loadBuiltinDepModule('JS_ARRAY') returns the on-disk text", () => {
    const text = loadBuiltinDepModule("JS_ARRAY");
    assert.match(text, /^module JS_ARRAY\./m);
    assert.match(text, /from xs: \[Int\] => \[Int\]\./);
    assert.match(
      text,
      /from-entries xs: \[String \* Int\] => \[String \* Int\]\./,
    );
  });

  it("loadBuiltinDepModule('JS_MAP') returns the on-disk text", () => {
    const text = loadBuiltinDepModule("JS_MAP");
    assert.match(text, /^module JS_MAP\./m);
    assert.match(text, /StringToIntMap\./);
    assert.match(text, /values m: StringToIntMap => \[Int\]\./);
    assert.match(text, /entries m: StringToIntMap => \[String \* Int\]\./);
    assert.match(text, /keys m: StringToIntMap => \[String\]\./);
  });

  it("caches the loaded module text across calls", () => {
    const a = loadBuiltinDepModule("JS_MATH");
    const b = loadBuiltinDepModule("JS_MATH");
    assert.equal(a, b);
  });
});

describe("builtins dispatch emission", () => {
  it("Math.max emits JS_MATH::max-of and imports JS_MATH", async () => {
    const output = await emitFromSource(
      "export function f(a: number, b: number) { return Math.max(a, b); }",
      "f",
    );
    assert.match(output, /^import JS_MATH\.$/m);
    assert.match(output, /JS_MATH::max-of/);
  });

  it("s.toUpperCase emits JS_STRING::to-upper-case and imports JS_STRING", async () => {
    const output = await emitFromSource(
      "export function f(s: string) { return s.toUpperCase(); }",
      "f",
    );
    assert.match(output, /^import JS_STRING\.$/m);
    assert.match(output, /JS_STRING::to-upper-case/);
  });

  it("Array.from emits JS_ARRAY::from and imports JS_ARRAY", async () => {
    const output = await emitFromSource(
      "export function f(xs: number[]) { return Array.from(xs); }",
      "f",
    );
    assert.match(output, /^import JS_ARRAY\.$/m);
    assert.match(output, /JS_ARRAY::from/);
  });

  it("m.values emits JS_MAP::values and imports JS_MAP", async () => {
    const output = await emitFromSource(
      "export function f(m: Map<string, number>) { return m.values(); }",
      "f",
    );
    assert.match(output, /^import JS_MAP\.$/m);
    assert.match(output, /JS_MAP::values/);
  });

  it("string-arg replace emits JS_STRING::replace", async () => {
    const output = await emitFromSource(
      'export function f(s: string) { return s.replace("a", "b"); }',
      "f",
    );
    assert.match(output, /^import JS_STRING\.$/m);
    assert.match(output, /JS_STRING::replace/);
  });
});

describe("hand-written js-stdlib modules typecheck", () => {
  for (const name of [
    "JS_ARRAY",
    "JS_MATH",
    "JS_MAP",
    "JS_STRING",
    "TS_PRELUDE",
  ] satisfies DepModuleName[]) {
    it(`samples/js-stdlib/${name}.pant typechecks standalone via wasm`, async () => {
      await assertWasmTypeChecks(loadBuiltinDepModule(name));
    });
  }
});
