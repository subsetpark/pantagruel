import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";
import {
  ambientModuleName,
  createSourceFile,
  createSourceFileFromSource,
  emitAmbientModule,
  extractAmbientFunctions,
} from "../src/extract.js";
import { assertWasmTypeChecks, loadAst } from "../src/pant-wasm.js";
import { IntStrategy } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

describe("extract", () => {
  it("extractAmbientFunctions returns one entry per `declare function` decl", () => {
    const source = `
      declare function neg(x: number): number;
      declare function double(x: number): number;
      declare function id(x: number): number;
    `;
    const sf = createSourceFileFromSource(source, "small-ambient.ts");
    const decls = extractAmbientFunctions(sf, IntStrategy);
    assert.equal(decls.length, 3);
    assert.deepEqual(
      decls.map((d) => d.tsName),
      ["neg", "double", "id"],
    );
    for (const d of decls) {
      assert.equal(d.declaration.kind, "rule");
      assert.ok(d.tsName.length > 0);
    }
  });

  it("extractAmbientFunctions skips non-declare exported functions", () => {
    const source = `
      declare function neg(x: number): number;
      export function double(x: number): number {
        return x * 2;
      }
      export function id(x: number): number {
        return x;
      }
    `;
    const sf = createSourceFileFromSource(source, "mixed.ts");
    const decls = extractAmbientFunctions(sf, IntStrategy);
    assert.deepEqual(
      decls.map((d) => d.tsName),
      ["neg"],
    );
  });

  it("emitAmbientModule for expressions-calls.ts produces EXPRESSIONS_CALLS_AMBIENT", () => {
    const filePath = resolve(
      import.meta.dirname,
      "fixtures/constructs/expressions-calls.ts",
    );
    const sf = createSourceFile(filePath);
    assert.equal(ambientModuleName(sf), "EXPRESSIONS_CALLS_AMBIENT");
    const decls = extractAmbientFunctions(sf, IntStrategy);
    const text = emitAmbientModule(sf, decls);
    assert.match(text, /^module EXPRESSIONS_CALLS_AMBIENT\.\n/u);
  });

  it("emitAmbientModule produces a standalone module that typechecks", async () => {
    const source = `
      declare function neg(x: number): number;
      declare function double(x: number): number;
      declare function noArgs(): number;
    `;
    const sf = createSourceFileFromSource(source, "neg-double.ts");
    const decls = extractAmbientFunctions(sf, IntStrategy);
    const text = emitAmbientModule(sf, decls);
    await assertWasmTypeChecks(text);
  });

  it("the same source file produces the same ambient module name from any consumer", () => {
    const source = `
      declare function f(x: number): number;
      export function consumerA(x: number): number { return f(x); }
      export function consumerB(x: number): number { return f(x); }
    `;
    const sf1 = createSourceFileFromSource(source, "shared.ts");
    const sf2 = createSourceFileFromSource(source, "shared.ts");
    assert.equal(ambientModuleName(sf1), "SHARED_AMBIENT");
    assert.equal(ambientModuleName(sf1), ambientModuleName(sf2));
  });
});
