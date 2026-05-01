import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";
import { Project } from "ts-morph";
import {
  ambientModuleName,
  createSourceFile,
  createSourceFileFromSource,
  emitAmbientModule,
  extractAmbientFunctions,
  extractReferencedTypes,
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

  it("extractAmbientFunctions dedupes overloaded `declare function`s", () => {
    const source = `
      declare function f(x: number): number;
      declare function f(x: string): string;
      declare function g(x: number): number;
    `;
    const sf = createSourceFileFromSource(source, "overload.ts");
    const decls = extractAmbientFunctions(sf, IntStrategy);
    assert.deepEqual(
      decls.map((d) => d.tsName),
      ["f", "g"],
    );
    const text = emitAmbientModule(sf, decls);
    const fHeadCount = text
      .split("\n")
      .filter((l) => l.startsWith("f ")).length;
    assert.equal(fHeadCount, 1, "exactly one rule head per overloaded name");
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

  describe("extractReferencedTypes — cross-file", () => {
    function buildTwoFileProject(consumerSource: string, depSource: string) {
      const project = new Project({
        compilerOptions: {
          target: 99, // ESNext
          module: 99, // NodeNext
          moduleResolution: 99,
          strict: true,
        },
        useInMemoryFileSystem: true,
      });
      project.createSourceFile("dep.ts", depSource);
      return project.createSourceFile("consumer.ts", consumerSource);
    }

    it("follows imports into a sibling file for an interface reference", () => {
      const consumer = buildTwoFileProject(
        `
          import type { Profile } from "./dep.js";
          export function getName(p: Profile): string { return p.name; }
        `,
        `
          export interface Profile {
            name: string;
            age: number;
          }
        `,
      );
      const types = extractReferencedTypes(consumer, "getName");
      const names = types.interfaces.map((i) => i.name);
      assert.deepEqual(names, ["Profile"]);
    });

    it("follows transitive imports through a nested interface field", () => {
      const project = new Project({
        compilerOptions: {
          target: 99,
          module: 99,
          moduleResolution: 99,
          strict: true,
        },
        useInMemoryFileSystem: true,
      });
      project.createSourceFile(
        "leaf.ts",
        `export interface Leaf { value: string; }`,
      );
      project.createSourceFile(
        "mid.ts",
        `
          import type { Leaf } from "./leaf.js";
          export interface Mid { leaf: Leaf; }
        `,
      );
      const consumer = project.createSourceFile(
        "consumer.ts",
        `
          import type { Mid } from "./mid.js";
          export function read(m: Mid): string { return m.leaf.value; }
        `,
      );
      const types = extractReferencedTypes(consumer, "read");
      const names = types.interfaces.map((i) => i.name).sort();
      assert.deepEqual(names, ["Leaf", "Mid"]);
    });

    it("skips interface fields whose type resolves only to TS-lib declarations", () => {
      // `SynthCell.sourceFile?: ts.SourceFile` in the real ts2pant src/
      // is the canonical case: `ts.SourceFile` is declared only in
      // typescript.d.ts (filtered as an external library). The accessor
      // rule for `sourceFile` would dangle, so the extractor drops the
      // field. Run against the actual source rather than a synthetic
      // fixture because the in-memory filesystem doesn't carry the real
      // `typescript` package, which makes synthetic foreign-symbol
      // setup unreliable.
      const filePath = resolve(
        import.meta.dirname,
        "../src/translate-types.ts",
      );
      const sf = createSourceFile(filePath);
      const types = extractReferencedTypes(sf, "cellIsUsed");
      const synthCell = types.interfaces.find((i) => i.name === "SynthCell");
      assert.ok(synthCell, "SynthCell interface should be extracted");
      const fieldNames = synthCell.properties.map((p) => p.name);
      assert.ok(
        !fieldNames.includes("sourceFile"),
        `sourceFile should be filtered as foreign-only; got ${fieldNames.join(", ")}`,
      );
    });
  });
});
