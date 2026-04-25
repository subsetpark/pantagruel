import assert from "node:assert/strict";
import { readdirSync } from "node:fs";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";
import { createSourceFile } from "../src/extract.js";
import { loadAst } from "../src/pant-wasm.js";
import { buildDocumentFromSourceFile, emitDocument } from "./helpers.mjs";

before(async () => {
  await loadAst();
});

/**
 * Per-stage IR equivalence gate.
 *
 * Translates each anchor function with `TS2PANT_USE_IR=0` (legacy
 * pipeline) and `TS2PANT_USE_IR=1` (IR pipeline), then asserts the
 * emitted strings are identical. This is the smoke-test gate; the
 * fuller cutover gate runs `pant --ast` on both outputs and structurally
 * compares the resulting OCaml AST. Snapshot equality is brittle for
 * binder-allocation orderings; for now string equality suffices because
 * Stage 1's IRWrap fallback is a true no-op.
 *
 * As stages migrate recognizers from legacy to IR, this test set grows
 * to cover the migrated forms. By Stage 8 (pure-path cutover) it covers
 * the entire pure path.
 */

const CONSTRUCTS_DIR = resolve(import.meta.dirname, "fixtures/constructs");

/**
 * Anchor fixtures the IR pipeline should produce identical output for at
 * the current stage. The list grows monotonically as stages migrate.
 */
const ANCHORS: Array<{ file: string; functions: string[] }> = [
  {
    file: "expressions-ir-anchor.ts",
    functions: ["effectiveValue"],
  },
  // Stages 2 (`?.`) + 3 (`??`): the entire expressions-nullish.ts fixture.
  {
    file: "expressions-nullish.ts",
    functions: [
      "defaultToZero",
      "preferLeft",
      "maybeBalance",
      "nonNullDefault",
      "maybeOwnerId",
      "maybeOwnerIdOptional",
    ],
  },
  // Stage 3: optional-parameter list-lift + `??` default
  {
    file: "expressions-optional-default.ts",
    functions: ["makePoint", "makeConfig", "usesOptionalDirectly"],
  },
];

async function emitWithFlag(
  filePath: string,
  funcName: string,
  useIR: boolean,
): Promise<string> {
  // Mutate the env var, build, then restore — the underlying
  // translateBody reads `process.env.TS2PANT_USE_IR` per-call.
  const g = globalThis as { process?: { env?: Record<string, string> } };
  const env = g.process?.env;
  if (!env) {
    throw new Error("ir-equivalence test requires Node process.env");
  }
  const prior = env["TS2PANT_USE_IR"];
  env["TS2PANT_USE_IR"] = useIR ? "1" : "0";
  try {
    const sf = createSourceFile(filePath);
    const doc = await buildDocumentFromSourceFile(sf, funcName);
    return emitDocument(doc);
  } finally {
    if (prior === undefined) {
      delete env["TS2PANT_USE_IR"];
    } else {
      env["TS2PANT_USE_IR"] = prior;
    }
  }
}

describe("Stage 1 IR-equivalence: legacy and IR produce identical output", () => {
  for (const { file, functions } of ANCHORS) {
    const filePath = resolve(CONSTRUCTS_DIR, file);
    for (const funcName of functions) {
      it(`${file} > ${funcName}`, async () => {
        const legacy = await emitWithFlag(filePath, funcName, false);
        const ir = await emitWithFlag(filePath, funcName, true);
        assert.equal(
          ir,
          legacy,
          `IR pipeline output differs from legacy for ${funcName}`,
        );
      });
    }
  }

  it("anchor file fixture exists and has the expected exports", () => {
    const files = readdirSync(CONSTRUCTS_DIR);
    assert.ok(
      files.includes("expressions-ir-anchor.ts"),
      "expressions-ir-anchor.ts must exist",
    );
  });
});
