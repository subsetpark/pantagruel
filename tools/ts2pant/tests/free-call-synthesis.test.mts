import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { Project } from "ts-morph";
import { createSourceFileFromSource } from "../src/extract.js";
import { buildPantDocument } from "../src/pipeline.js";
import { IntStrategy } from "../src/translate-types.js";
import { emitAndCheck, emitDocument } from "./helpers.mjs";

async function emitChecked(source: string, functionName = "consumer") {
  const sf = createSourceFileFromSource(source, "free-call-synthesis.ts");
  const doc = await buildPantDocument({
    sourceFile: sf,
    functionName,
    strategy: IntStrategy,
  });
  return emitAndCheck(doc);
}

async function emitUnchecked(source: string, functionName = "consumer") {
  const sf = createSourceFileFromSource(source, "free-call-synthesis.ts");
  const doc = await buildPantDocument({
    sourceFile: sf,
    functionName,
    strategy: IntStrategy,
  });
  return emitDocument(doc);
}

describe("free-call synthesis", () => {
  it("synthesizes an ambient single-arg head", async () => {
    const output = await emitChecked(`
      declare function neg(x: number): number;
      export function consumer(x: number): number { return neg(x); }
    `);
    assert.match(output, /^neg x1: Int => Int\.$/mu);
    assert.match(output, /^consumer x: Int => Int\.$/mu);
    assert.match(output, /^consumer x = neg x\.$/mu);
  });

  it("synthesizes an ambient multi-arg head", async () => {
    const output = await emitChecked(`
      declare function max(a: number, b: number): number;
      export function consumer(a: number, b: number): number {
        return max(a, b);
      }
    `);
    assert.match(output, /^max1 a1: Int, b1: Int => Int\.$/mu);
  });

  it("synthesizes an ambient zero-arity head", async () => {
    const output = await emitChecked(`
      declare function now(): number;
      export function consumer(): number { return now(); }
    `);
    assert.match(output, /^now\s+=> Int\.$/mu);
    assert.match(output, /^consumer = now\.$/mu);
  });

  it("synthesizes nested free-call heads", async () => {
    const output = await emitChecked(`
      declare function max(a: number, b: number): number;
      declare function clamp(x: number, lo: number, hi: number): number;
      export function consumer(x: number, a: number, b: number): number {
        return clamp(x, 0, max(a, b));
      }
    `);
    assert.match(output, /^max1 a1: Int, b1: Int => Int\.$/mu);
    assert.match(output, /^clamp x1: Int, lo: Int, hi: Int => Int\.$/mu);
  });

  it("maps any params to Opaque", async () => {
    const output = await emitChecked(`
      declare function opaqueValue(): any;
      declare function consumeAny(x: any): number;
      export function consumer(): number { return consumeAny(opaqueValue()); }
    `);
    assert.match(output, /^Opaque\.$/mu);
    assert.match(output, /^opaque-value\s+=> Opaque\.$/mu);
    assert.match(output, /^consume-any x: Opaque => Int\.$/mu);
  });

  it("maps unknown returns to Opaque", async () => {
    const output = await emitChecked(`
      declare function mystery(): unknown;
      declare function consumeUnknown(x: unknown): number;
      export function consumer(): number { return consumeUnknown(mystery()); }
    `);
    assert.match(output, /^Opaque\.$/mu);
    assert.match(output, /^mystery\s+=> Opaque\.$/mu);
    assert.match(output, /^consume-unknown x: Opaque => Int\.$/mu);
  });

  it("skips void/action callees", async () => {
    const output = await emitUnchecked(`
      declare function log(x: number): void;
      export function consumer(x: number): number {
        log(x);
        return x;
      }
    `);
    assert.doesNotMatch(output, /^log x: Int/mu);
    assert.doesNotMatch(output, /^~> Log/mu);
  });

  it("does not synthesize builtin property callees", async () => {
    const output = await emitUnchecked(`
      export function consumer(a: number, b: number): number {
        return Math.max(a, b);
      }
    `);
    assert.doesNotMatch(output, /^max/mu);
    assert.doesNotMatch(output, /^max-of/mu);
  });

  it("keeps in-project free callees to one declaration", async () => {
    const output = await emitChecked(`
      function helper(x: number): number { return x + 1; }
      export function consumer(x: number): number { return helper(x); }
    `);
    const helperHeads = output
      .split("\n")
      .filter((line) => line === "helper x1: Int => Int.");
    assert.equal(helperHeads.length, 1);
  });

  it("deduplicates aliased imported free callees", async () => {
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
      "dep.ts",
      `export declare function score(x: number): number;`,
    );
    const sf = project.createSourceFile(
      "consumer.ts",
      `
        import { score, score as rate } from "./dep.js";
        export function consumer(x: number): number {
          return score(x) + rate(x);
        }
      `,
    );
    const doc = await buildPantDocument({
      sourceFile: sf,
      functionName: "consumer",
      strategy: IntStrategy,
    });
    const output = await emitAndCheck(doc);
    const scoreHeads = output
      .split("\n")
      .filter((line) => line === "score x1: Int => Int.");
    assert.equal(scoreHeads.length, 1);
    assert.match(output, /^consumer x = score x \+ score x\.$/mu);
  });

  it("synthesizes a callable variable declaration", async () => {
    const output = await emitChecked(`
      declare const ext: (x: number) => number;
      export function consumer(x: number): number { return ext(x); }
    `);
    assert.match(output, /^ext x1: Int => Int\.$/mu);
  });
});
