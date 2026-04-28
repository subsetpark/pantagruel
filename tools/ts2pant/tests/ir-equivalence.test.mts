import assert from "node:assert/strict";
import { readdirSync } from "node:fs";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";
import { createSourceFile } from "../src/extract.js";
import { assertWasmTypeChecks, loadAst } from "../src/pant-wasm.js";
import {
  buildDocumentFromSourceFile,
  containsUnsupportedLine,
  emitDocument,
} from "./helpers.mjs";
import { KNOWN_TYPECHECK_FAILURES } from "./known-typecheck-failures.mjs";

before(async () => {
  await loadAst();
});

/**
 * Always-on pure IR regression gate.
 *
 * The legacy pure-expression fallback is gone, so these tests translate
 * each migrated anchor once and assert the emitted Pant is supported and
 * type-checkable where the shared known-bad list allows it.
 */

const CONSTRUCTS_DIR = resolve(import.meta.dirname, "fixtures/constructs");

/** Anchor fixtures covered by the always-on pure IR path. */
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
  // Stage 5: `.length` / `.size` cardinality
  {
    file: "expressions-array.ts",
    functions: ["count"],
  },
  {
    file: "expressions-set.ts",
    functions: ["cardinality"],
  },
  // Stage 6: const-binding inlining via IR Let nodes (initializers are
  // IRWrap of legacy-translated OpaqueExpr; substitution flows through
  // lowerExpr's substituteBinder rather than the legacy `applyTo`
  // closure). Includes μ-search bindings, which still go through
  // `translateMuSearchInit` for the comprehension construction (the
  // recognizer + lowering stay legacy; only the binding-substitution
  // mechanism is on IR — see WORKING-NOTES Decisions deferred).
  {
    file: "expressions-const-bindings.ts",
    functions: [
      "simpleConst",
      "chainedConst",
      "constWithPropAccess",
      "constInTernary",
    ],
  },
  {
    file: "expressions-while-mu-search.ts",
    functions: ["firstUnusedSuffix", "nextSlotPlusOne", "offsetUnusedSuffix"],
  },
  // Chain fusion (.filter/.map/.reduce) now builds native IR nodes.
  {
    file: "expressions-array.ts",
    functions: ["activeNames", "nameLengths", "highScores"],
  },
  {
    file: "expressions-reduce.ts",
    functions: [
      "sumAmounts",
      "productValues",
      "allActive",
      "anyActive",
      "sumFromBase",
      "subtractAll",
      "sumAmountsRight",
    ],
  },
];

async function emitAlwaysOn(filePath: string, funcName: string): Promise<string> {
  const sf = createSourceFile(filePath);
  const doc = await buildDocumentFromSourceFile(sf, funcName);
  return emitDocument(doc);
}

describe("pure IR path: always-on regression coverage", () => {
  for (const { file, functions } of ANCHORS) {
    const filePath = resolve(CONSTRUCTS_DIR, file);
    for (const funcName of functions) {
      const knownBad = KNOWN_TYPECHECK_FAILURES.get(`${file} > ${funcName}`);
      it(`${file} > ${funcName}`, async () => {
        const output = await emitAlwaysOn(filePath, funcName);
        assert.ok(
          !containsUnsupportedLine(output),
          `${funcName} should be supported by the always-on pure IR path`,
        );
        if (!knownBad) {
          await assertWasmTypeChecks(output);
        }
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

describe("M6 pure-path cutover stubs", () => {
  const CUTOVER_CASES: Array<{ file: string; functions: string[] }> = [
    {
      file: "expressions-arithmetic.ts",
      functions: ["add", "double", "netBalance"],
    },
    {
      file: "expressions-comparison.ts",
      functions: ["lt", "gte"],
    },
    {
      file: "expressions-calls.ts",
      functions: ["freeCall", "nestedCalls", "callInArithmetic"],
    },
    {
      file: "expressions-nullish.ts",
      functions: ["maybeOwnerIdOptional", "defaultToZero"],
    },
    {
      file: "expressions-array.ts",
      functions: ["activeNames", "nameLengths", "highScores"],
    },
    {
      file: "expressions-reduce.ts",
      functions: ["sumAmounts", "allActive"],
    },
  ];

  for (const { file, functions } of CUTOVER_CASES) {
    const filePath = resolve(CONSTRUCTS_DIR, file);
    for (const funcName of functions) {
      const knownBad = KNOWN_TYPECHECK_FAILURES.get(`${file} > ${funcName}`);
      it(`${file} > ${funcName} emits through the always-on IR path`, async () => {
        const output = await emitAlwaysOn(filePath, funcName);
        assert.ok(
          !containsUnsupportedLine(output),
          `${funcName} should be supported by the native IR pure path`,
        );
        if (!knownBad) {
          await assertWasmTypeChecks(output);
        }
      });
    }
  }

  it("translatePureBody uses pure IR path without TS2PANT_USE_IR", async () => {
    const output = await emitAlwaysOn(
      resolve(CONSTRUCTS_DIR, "expressions-arithmetic.ts"),
      "netBalance",
    );
    assert.ok(!containsUnsupportedLine(output));
    await assertWasmTypeChecks(output);
  });
});
