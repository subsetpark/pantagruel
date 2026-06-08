// @archlint.module test
// @archlint.domain ts2pant.brand-precondition

import assert from "node:assert/strict";
import * as fc from "fast-check";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { before, describe, it } from "node:test";
import ts from "typescript";
import {
  BRAND_PREDICATES,
  recognizeBrandedPrecondition,
} from "../src/brand-precondition.js";
import { emitDocument } from "../src/emit.js";
import { createSourceFile, getChecker } from "../src/extract.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { buildPantDocument } from "../src/pipeline.js";
import { IntStrategy } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

function withSource<T>(source: string, use: (ctx: SourceCtx) => T): T {
  const dir = mkdtempSync(join(process.cwd(), ".tmp-brand-"));
  const file = join(dir, "fixture.ts");
  writeFileSync(file, source);
  try {
    const sourceFile = createSourceFile(file);
    return use({
      sourceFile: sourceFile.compilerNode,
      checker: getChecker(sourceFile),
    });
  } finally {
    rmSync(dir, { force: true, recursive: true });
  }
}

interface SourceCtx {
  sourceFile: ts.SourceFile;
  checker: ts.TypeChecker;
}

function paramType(ctx: SourceCtx, name: string): ts.Type {
  let found: ts.ParameterDeclaration | undefined;
  ts.forEachChild(ctx.sourceFile, function visit(node) {
    if (
      ts.isFunctionDeclaration(node) &&
      node.name?.text === name &&
      node.parameters[0]
    ) {
      found = node.parameters[0];
      return;
    }
    ts.forEachChild(node, visit);
  });
  if (!found) {
    throw new Error(`No first parameter for ${name}`);
  }
  return ctx.checker.getTypeAtLocation(found);
}

describe("BRAND_PREDICATES", () => {
  it("maps the curated allowlist to Pant predicates", () => {
    const ast = getAst();
    const expected = new Map([
      ["positive", "x > 0"],
      ["Positive", "x > 0"],
      ["negative", "x < 0"],
      ["Negative", "x < 0"],
      ["nonNegative", "x >= 0"],
      ["NonNegative", "x >= 0"],
      ["nonPositive", "x <= 0"],
      ["NonPositive", "x <= 0"],
      ["int", "integral x"],
      ["Int", "integral x"],
      ["integer", "integral x"],
      ["Integer", "integral x"],
      ["nonEmpty", "#x > 0"],
      ["NonEmpty", "#x > 0"],
      ["NonEmptyString", "#x > 0"],
    ]);

    assert.deepEqual([...BRAND_PREDICATES.keys()], [...expected.keys()]);
    for (const [brand, rendered] of expected) {
      assert.equal(ast.strExpr(BRAND_PREDICATES.get(brand)!("x")), rendered);
    }
  });
});

describe("recognizeBrandedPrecondition", () => {
  it("maps recognized Brand.Brand intersections to predicates", () => {
    withSource(
      `
        import type * as Brand from "effect/Brand";
        type Positive = number & Brand.Brand<"Positive">;
        type NonNegative = number & Brand.Brand<"NonNegative">;
        type NonEmptyString = string & Brand.Brand<"NonEmptyString">;
        type PositiveInt = number & Brand.Brand<"Positive"> & Brand.Brand<"Int">;
        export function positive(x: Positive) { return x; }
        export function nonNegative(x: NonNegative) { return x; }
        export function nonEmpty(x: NonEmptyString) { return x; }
        export function positiveInt(x: PositiveInt) { return x; }
      `,
      (ctx) => {
        const ast = getAst();
        assert.equal(
          ast.strExpr(
            recognizeBrandedPrecondition(paramType(ctx, "positive"), "x")!,
          ),
          "x > 0",
        );
        assert.equal(
          ast.strExpr(
            recognizeBrandedPrecondition(paramType(ctx, "nonNegative"), "x")!,
          ),
          "x >= 0",
        );
        assert.equal(
          ast.strExpr(
            recognizeBrandedPrecondition(paramType(ctx, "nonEmpty"), "x")!,
          ),
          "#x > 0",
        );
        assert.equal(
          ast.strExpr(
            recognizeBrandedPrecondition(paramType(ctx, "positiveInt"), "x")!,
          ),
          "x > 0 and integral x",
        );
      },
    );
  });

  it("returns null for unrecognized and unbranded types", () => {
    withSource(
      `
        import type * as Brand from "effect/Brand";
        type Custom = number & Brand.Brand<"Custom">;
        export function custom(x: Custom) { return x; }
        export function plain(x: number) { return x; }
      `,
      (ctx) => {
        assert.equal(
          recognizeBrandedPrecondition(paramType(ctx, "custom"), "x"),
          null,
        );
        assert.equal(
          recognizeBrandedPrecondition(paramType(ctx, "plain"), "x"),
          null,
        );
      },
    );
  });

  it("recognizes generated allowlisted Effect brand names", () => {
    fc.assert(
      fc.property(
        fc.constantFrom("Positive", "NonNegative", "NonEmptyString", "Int"),
        (brand) => {
          withSource(
            `
              import type * as Brand from "effect/Brand";
              type Branded = number & Brand.Brand<"${brand}">;
              export function subject(x: Branded) { return x; }
            `,
            (ctx) => {
              const result = recognizeBrandedPrecondition(
                paramType(ctx, "subject"),
                "x",
              );

              assert.notEqual(result, null);
              assert.equal(typeof getAst().strExpr(result!), "string");
            },
          );
        },
      ),
      { numRuns: 8 },
    );
  });
});

describe("brand-precondition @pant integration stubs", () => {
  it("emits a recognized brand predicate as a precondition", async () => {
    const dir = mkdtempSync(join(process.cwd(), ".tmp-brand-"));
    const file = join(dir, "fixture.ts");
    writeFileSync(
      file,
      `
        import type * as Brand from "effect/Brand";
        type Positive = number & Brand.Brand<"Positive">;
        export function positive(x: Positive): number { return x; }
      `,
    );
    try {
      const doc = await buildPantDocument({
        sourceFile: createSourceFile(file),
        functionName: "positive",
        strategy: IntStrategy,
        noBody: true,
      });
      const output = emitDocument(doc);
      assert.match(output, /positive x: Int, x > 0 => Int\./u);
    } finally {
      rmSync(dir, { force: true, recursive: true });
    }
  });

  it("emits no precondition for unrecognized or absent brands", async () => {
    const dir = mkdtempSync(join(process.cwd(), ".tmp-brand-"));
    const file = join(dir, "fixture.ts");
    writeFileSync(
      file,
      `
        import type * as Brand from "effect/Brand";
        type Custom = number & Brand.Brand<"Custom">;
        export function custom(x: Custom): number { return x; }
        export function plain(x: number): number { return x; }
      `,
    );
    try {
      for (const functionName of ["custom", "plain"]) {
        const doc = await buildPantDocument({
          sourceFile: createSourceFile(file),
          functionName,
          strategy: IntStrategy,
          noBody: true,
        });
        const output = emitDocument(doc);
        assert.match(output, new RegExp(`${functionName} x: Int => Int\\.`));
      }
    } finally {
      rmSync(dir, { force: true, recursive: true });
    }
  });
});
