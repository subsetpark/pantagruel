// @archlint.module test
// @archlint.domain ts2pant.effect-error-channel

import assert from "node:assert/strict";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { before, describe, it } from "node:test";
import * as fc from "fast-check";
import ts from "typescript";
import {
  type EffectErrorMode,
  extractEffectErrorModes,
  recognizeErrorYield,
  recoverErrorModeGuard,
} from "../src/effect-error-channel.js";
import { emitDocument } from "../src/emit.js";
import { createSourceFile, getChecker } from "../src/extract.js";
import { loadAst } from "../src/pant-wasm.js";
import { buildPantDocument } from "../src/pipeline.js";
import { IntStrategy } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

function withSource<T>(source: string, use: (ctx: SourceCtx) => T): T {
  const dir = mkdtempSync(join(process.cwd(), ".tmp-effect-error-"));
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

function functionReturnType(ctx: SourceCtx, name: string): ts.Type {
  const fn = findFunction(ctx.sourceFile, name);
  const signature = ctx.checker.getSignatureFromDeclaration(fn);
  if (!signature) {
    throw new Error(`No signature for ${name}`);
  }
  return ctx.checker.getReturnTypeOfSignature(signature);
}

function findFunction(
  sourceFile: ts.SourceFile,
  name: string,
): ts.FunctionDeclaration {
  let found: ts.FunctionDeclaration | undefined;
  ts.forEachChild(sourceFile, function visit(node) {
    if (ts.isFunctionDeclaration(node) && node.name?.text === name) {
      found = node;
      return;
    }
    ts.forEachChild(node, visit);
  });
  if (!found) {
    throw new Error(`No function named ${name}`);
  }
  return found;
}

function findFirstYield(sourceFile: ts.SourceFile): ts.YieldExpression {
  let found: ts.YieldExpression | undefined;
  ts.forEachChild(sourceFile, function visit(node) {
    if (!found && ts.isYieldExpression(node)) {
      found = node;
      return;
    }
    ts.forEachChild(node, visit);
  });
  if (!found) {
    throw new Error("No yield expression found");
  }
  return found;
}

function findIf(sourceFile: ts.SourceFile, index = 0): ts.IfStatement {
  const found: ts.IfStatement[] = [];
  ts.forEachChild(sourceFile, function visit(node) {
    if (ts.isIfStatement(node)) {
      found.push(node);
    }
    ts.forEachChild(node, visit);
  });
  const stmt = found[index];
  if (!stmt) {
    throw new Error(`No if statement at index ${index}`);
  }
  return stmt;
}

function toPantName(name: string): string {
  return name.replace(/[A-Z]/gu, (c) => `-${c.toLowerCase()}`);
}

describe("extractEffectErrorModes", () => {
  it("enumerates single and union E-channel modes", () => {
    withSource(
      `
        import { Effect } from "effect";
        class FooError {}
        class BarError {}
        export function single(): Effect.Effect<number, FooError, never> {
          return Effect.succeed(1) as Effect.Effect<number, FooError, never>;
        }
        export function union(): Effect.Effect<number, FooError | BarError, never> {
          return Effect.succeed(1) as Effect.Effect<number, FooError | BarError, never>;
        }
      `,
      (ctx) => {
        assert.deepEqual(
          extractEffectErrorModes(
            functionReturnType(ctx, "single"),
            ctx.checker,
          )?.map((mode) => mode.name),
          ["FooError"],
        );
        assert.deepEqual(
          extractEffectErrorModes(
            functionReturnType(ctx, "union"),
            ctx.checker,
          )?.map((mode) => mode.name),
          ["FooError", "BarError"],
        );
      },
    );
  });

  it("returns null for non-Effect and never E-channel returns", () => {
    withSource(
      `
        import { Effect } from "effect";
        export function plain(): number { return 1; }
        export function infallible(): Effect.Effect<number, never, never> {
          return Effect.succeed(1);
        }
      `,
      (ctx) => {
        assert.equal(
          extractEffectErrorModes(
            functionReturnType(ctx, "plain"),
            ctx.checker,
          ),
          null,
        );
        assert.equal(
          extractEffectErrorModes(
            functionReturnType(ctx, "infallible"),
            ctx.checker,
          ),
          null,
        );
      },
    );
  });
});

describe("recognizeErrorYield", () => {
  it("matches yield* new ErrorClass()", () => {
    withSource(
      `
        class FooError {}
        function* gen() {
          yield* new FooError("bad");
        }
      `,
      (ctx) => {
        assert.equal(
          recognizeErrorYield(findFirstYield(ctx.sourceFile))?.errorName,
          "FooError",
        );
      },
    );
  });

  it("rejects non-asterisk yields and non-constructor expressions", () => {
    withSource(
      `
        class FooError {}
        function* gen() {
          yield new FooError();
          yield* makeError();
        }
      `,
      (ctx) => {
        const yields: ts.YieldExpression[] = [];
        ts.forEachChild(ctx.sourceFile, function visit(node) {
          if (ts.isYieldExpression(node)) {
            yields.push(node);
          }
          ts.forEachChild(node, visit);
        });
        assert.equal(recognizeErrorYield(yields[0]!), null);
        assert.equal(recognizeErrorYield(yields[1]!), null);
      },
    );
  });
});

describe("recoverErrorModeGuard", () => {
  const modes: EffectErrorMode[] = [
    { name: "FooError", type: undefined as unknown as ts.Type },
  ];

  it("matches braced and braceless if-yield guards", () => {
    withSource(
      `
        class FooError {}
        declare function sideEffect(): boolean;
        function* gen(x: number) {
          if (x > 0) { yield* new FooError(); }
          if (x === 0) yield* new FooError();
          if (sideEffect()) { yield* new FooError(); }
        }
      `,
      (ctx) => {
        assert.equal(
          recoverErrorModeGuard(findIf(ctx.sourceFile, 0), modes, ctx.checker)
            ?.mode.name,
          "FooError",
        );
        assert.equal(
          recoverErrorModeGuard(findIf(ctx.sourceFile, 1), modes, ctx.checker)
            ?.mode.name,
          "FooError",
        );
        assert.equal(
          recoverErrorModeGuard(findIf(ctx.sourceFile, 2), modes, ctx.checker),
          null,
        );
      },
    );
  });

  it("rejects unmatched error constructors", () => {
    withSource(
      `
        class BarError {}
        function* gen(x: number) {
          if (x > 0) { yield* new BarError(); }
        }
      `,
      (ctx) => {
        assert.equal(
          recoverErrorModeGuard(findIf(ctx.sourceFile), modes, ctx.checker),
          null,
        );
      },
    );
  });

  it("recognizes generated Effect error mode declarations and guards", () => {
    fc.assert(
      fc.property(fc.constantFrom("FooError", "BarError"), (modeName) => {
        withSource(
          `
            import { Effect } from "effect";
            class ${modeName} {}
            export function subject(x: number): Effect.Effect<number, ${modeName}, never> {
              return Effect.gen(function* () {
                if (x > 0) { yield* new ${modeName}(); }
                return x;
              });
            }
          `,
          (ctx) => {
            const modes = extractEffectErrorModes(
              functionReturnType(ctx, "subject"),
              ctx.checker,
            );
            const yieldExpr = findFirstYield(ctx.sourceFile);
            const guard = recoverErrorModeGuard(
              findIf(ctx.sourceFile),
              modes ?? [],
              ctx.checker,
            );

            assert.deepEqual(
              modes?.map((mode) => mode.name),
              [modeName],
            );
            assert.equal(recognizeErrorYield(yieldExpr)?.errorName, modeName);
            assert.equal(guard?.mode.name, modeName);
          },
        );
      }),
      { numRuns: 8 },
    );
  });
});

describe("effect-error-channel @pant integration stubs", () => {
  it("emits a negated-conjunction precondition when all modes are recovered", async () => {
    const dir = mkdtempSync(join(process.cwd(), ".tmp-effect-error-"));
    const file = join(dir, "fixture.ts");
    writeFileSync(
      file,
      `
        import { Data, Effect } from "effect";
        class FooError extends Data.TaggedError("FooError")<{}> {}
        class BarError extends Data.TaggedError("BarError")<{}> {}
        export function recovered(x: number): Effect.Effect<number, FooError | BarError, never> {
          return Effect.gen(function* () {
            if (x < 0) { yield* new FooError(); }
            if (x > 10) { yield* new BarError(); }
            return x;
          });
        }
      `,
    );
    try {
      const doc = await buildPantDocument({
        sourceFile: createSourceFile(file),
        functionName: "recovered",
        strategy: IntStrategy,
        noBody: true,
      });
      const output = emitDocument(doc);
      assert.match(output, /recovered x: Int, ~\(x < 0\) and ~\(x > 10\) =>/u);
    } finally {
      rmSync(dir, { force: true, recursive: true });
    }
  });

  it("emits no precondition for incomplete, impure, or unmatched recovery", async () => {
    const dir = mkdtempSync(join(process.cwd(), ".tmp-effect-error-"));
    const file = join(dir, "fixture.ts");
    writeFileSync(
      file,
      `
        import { Data, Effect } from "effect";
        class FooError extends Data.TaggedError("FooError")<{}> {}
        class BarError extends Data.TaggedError("BarError")<{}> {}
        declare function blocked(): boolean;
        export function partial(x: number): Effect.Effect<number, FooError | BarError, never> {
          return Effect.gen(function* () {
            if (x < 0) { yield* new FooError(); }
            return x;
          });
        }
        export function impure(x: number): Effect.Effect<number, FooError, never> {
          return Effect.gen(function* () {
            if (blocked()) { yield* new FooError(); }
            return x;
          });
        }
        export function unmatched(x: number): Effect.Effect<number, FooError, never> {
          return Effect.gen(function* () {
            if (x < 0) { yield* new BarError(); }
            return x;
          });
        }
        export function unmatchedShape(x: number): Effect.Effect<number, FooError, never> {
          return Effect.gen(function* () {
            yield* new FooError();
            return x;
          });
        }
      `,
    );
    try {
      for (const functionName of [
        "partial",
        "impure",
        "unmatched",
        "unmatchedShape",
      ]) {
        const pantName = toPantName(functionName);
        const doc = await buildPantDocument({
          sourceFile: createSourceFile(file),
          functionName,
          strategy: IntStrategy,
          noBody: true,
        });
        const declaration = emitDocument(doc)
          .split("\n")
          .find((line) => line.startsWith(`${pantName} `));
        assert.ok(declaration, `missing declaration for ${functionName}`);
        assert.equal(
          declaration.split("=>", 1)[0]!.trim(),
          `${pantName} x: Int`,
        );
      }
    } finally {
      rmSync(dir, { force: true, recursive: true });
    }
  });
});
