import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { emitDocument, runCheck } from "../../src/emit.js";
import {
  assertPantTypeChecks,
  buildDocument as buildDocumentFromPath,
  emitAndCheck,
  getPantBin,
  PROJECT_ROOT,
} from "../helpers.mjs";

function solverAvailable(): boolean {
  try {
    execFileSync("z3", ["-version"], { stdio: "ignore" });
    return true;
  } catch {
    return false;
  }
}

// Dogfood: translate ts2pant's own source files with ts2pant itself.
// Depth-first — one target module at a time, smallest function first.
// Each passing case here is a concrete self-translation baseline; failing
// cases are tracked in the project roadmap as diagnostics driving the next
// feature to build.

const SRC = resolve(import.meta.dirname, "../../src");
const FIXTURES = resolve(import.meta.dirname, "../fixtures/constructs");
const PANT_TIMEOUT_MS = Number(
  process.env.TS2PANT_DOGFOOD_TIMEOUT_MS ?? 30_000,
);

describe("dogfood: src/name-registry.ts", () => {
  const filePath = resolve(SRC, "name-registry.ts");

  it("isUsed — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "isUsed");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
  });

  it("emptyNameRegistry — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "emptyNameRegistry");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
  });
});

describe("dogfood: src/translate-types.ts", () => {
  const filePath = resolve(SRC, "translate-types.ts");

  it("emptyMapSynth — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "emptyMapSynth");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output);
    // `MapSynth.byKV: ReadonlyMap<string, MapSynthEntry>` → Stage A
    // encoding: a binary membership predicate alongside the value rule.
    // Accessor rule names are qualified with the owning domain so two
    // interfaces with a same-named field produce distinct arity-1 rules
    // under positional coherence.
    t.assert.match(
      output,
      /map-synth--by-kv-key m\d*: MapSynth, k: String => Bool\./u,
    );
    t.assert.match(
      output,
      /map-synth--by-kv m\d*: MapSynth, k: String, map-synth--by-kv-key m\d* k => MapSynthEntry\./u,
    );
    // `return { byKV: new Map(), emitted: new Set() }` → empty-map
    // initializer emits Stage A membership-negation assertion.
    t.assert.match(
      output,
      /all k\d*: String \| ~\(map-synth--by-kv-key empty-map-synth k\d*\)\./u,
    );
    // `collectNamedTypes` recursion must follow `ReadonlyMap<K, V>` into V:
    // `MapSynthEntry` is only reachable through `byKV`'s value type.
    t.assert.match(output, /^MapSynthEntry\.$/mu);
  });

  it("emptyRecordSynth — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "emptyRecordSynth");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output);
    // Same three invariants on the sibling RecordSynth type. Paired with
    // the MapSynth case so a regression in any one of ReadonlyMap-field
    // Stage A encoding, empty-map initializer emission, or
    // collectNamedTypes recursion surfaces on a focused assertion rather
    // than only on a compound snapshot.
    t.assert.match(
      output,
      /record-synth--by-shape-key r\d*: RecordSynth, k: String => Bool\./u,
    );
    t.assert.match(
      output,
      /all k\d*: String \| ~\(record-synth--by-shape-key empty-record-synth k\d*\)\./u,
    );
    t.assert.match(output, /^RecordSynthEntry\.$/mu);
  });

  it("emptyOpaqueSynth — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "emptyOpaqueSynth");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // The opaque synth starts inert: no registered ids, no domain need,
    // and no prior domain emission. These are the local invariants that
    // make the any/unknown fallback an on-use vocabulary rather than a
    // globally injected domain.
    t.assert.match(
      output,
      /all k\d*: String \| ~\(opaque-synth--by-id-key empty-opaque-synth k\d*\)\./u,
    );
    t.assert.match(
      output,
      /opaque-synth--needs-domain empty-opaque-synth = false\./u,
    );
    t.assert.match(
      output,
      /opaque-synth--domain-emitted empty-opaque-synth = false\./u,
    );
  });

  it("lookupMapKV — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "lookupMapKV");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Body composes two pieces: same-file helper extraction for
    // `mapSynthKey(kType, vType)`, and `synth.byKV.get(...)` lowering to
    // the partial-rule value application against the synthesized MapSynth
    // domain.
    t.assert.match(
      output,
      /map-synth-key k-type1: String, v-type1: String => String\./u,
    );
    t.assert.match(
      output,
      /lookup-map-kv synth k-type v-type = map-synth--by-kv synth \(map-synth-key k-type v-type\)\./u,
    );
    // Map-partial signature alignment: the body emits a bare `V`
    // (Pant's Map encoding is unboxed under the partial-rule guard),
    // so the signature's return type narrows from `[MapSynthEntry]` to
    // bare `MapSynthEntry`. Without `bodyIsBareMapGet`'s
    // narrow, the equation type-mismatches against the list-lifted
    // declaration. Don't anchor on `^...$` because the synth-decl run
    // emits other `=> MapSynthEntry` rules that share the suffix.
    t.assert.match(
      output,
      /lookup-map-kv synth: MapSynth, k-type: String, v-type: String => MapSynthEntry\./u,
    );
  });

  it("mapSynthKey — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "mapSynthKey");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Immutable `let separator = "|"` routes through local-binding SSA
    // and stays as a body equation before the formatted key equation.
    t.assert.match(output, /^separator = "\|"\.$/mu);
    t.assert.match(
      output,
      /map-synth-key k-type v-type = k-type \+ separator \+ v-type\./u,
    );
  });

  it("isUnsupportedUnknown — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "isUnsupportedUnknown");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Module-level `const UNSUPPORTED_UNKNOWN = "__unsupported_unknown__"`
    // emits as a 0-arity rule + value equation (extractModuleConsts).
    // Reference-filtered: the sibling `UNSUPPORTED_ANONYMOUS_RECORD` /
    // `UNSUPPORTED_UNKNOWN_REASON` consts in the same module are NOT
    // pulled in because `isUnsupportedUnknown`'s body doesn't reference
    // them.
    t.assert.match(output, /unsupported-unknown\s+=> String\./u);
    t.assert.match(
      output,
      /unsupported-unknown = "__unsupported_unknown__"\./u,
    );
    t.assert.doesNotMatch(output, /unsupported-anonymous-record/u);
    t.assert.doesNotMatch(output, /unsupported-unknown-reason/u);
    // Body equation rewrites the identifier reference through
    // `paramNameMap` so `UNSUPPORTED_UNKNOWN` resolves to the kebab'd
    // 0-arity rule, not the raw TS-cased name.
    t.assert.match(
      output,
      /is-unsupported-unknown pant-type = \(pant-type = unsupported-unknown\)\./u,
    );
  });

  it("manglePantTypeToFragment — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "manglePantTypeToFragment");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
  });

  it("fieldRuleName — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "fieldRuleName");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Body calls a sibling `toPantTermName` (declared in the same
    // file) twice, with `+` between String operands. The cross-file
    // function extractor pulls in the `to-pant-term-name` rule head;
    // without it the consumer's body would dangle on the call.
    t.assert.match(
      output,
      /to-pant-term-name [a-z]+\d*: String => String\./u,
    );
  });

  it("lookupRecordShape — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "lookupRecordShape");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Same-file sibling call (`recordShapeKey(fields)`) gets a rule
    // head emitted via `extractReferencedFunctions`. Map.get on the
    // RecordSynth Stage A encoding still narrows to bare V.
    t.assert.match(
      output,
      /record-shape-key fields\d*: \[RecordSynthField\] => String\./u,
    );
    t.assert.match(
      output,
      /lookup-record-shape synth: RecordSynth, fields\d*: \[RecordSynthField\] => RecordSynthEntry\./u,
    );
  });

  it("lookupOpaqueValue — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "lookupOpaqueValue");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Opaque value lookup is the sibling of the Map/Record lookup helpers:
    // the optional Map.get return narrows to the guarded value rule that
    // backs OpaqueSynth.byId.
    t.assert.match(
      output,
      /lookup-opaque-value synth id = opaque-synth--by-id synth id\./u,
    );
  });

  it("recordFieldShapeKey — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "recordFieldShapeKey");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Exercises field accessor lowering plus immutable-let SSA for the
    // record-shape delimiter used by `recordShapeKey`.
    t.assert.match(output, /^separator = ":"\.$/mu);
    t.assert.match(
      output,
      /record-field-shape-key field = record-synth-field--name field \+ separator \+ record-synth-field--type field\./u,
    );
  });

  it("cellIsUsed — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "cellIsUsed");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Combines cross-file types (NameRegistry from name-registry.ts
    // reached via SynthCell.registry) with same-file sibling fn
    // extraction (`toPantTermName`). The body is a single Set
    // membership test against the kebab'd term name.
    t.assert.match(output, /^NameRegistry\.$/mu);
    t.assert.match(
      output,
      /to-pant-term-name [a-z]+\d*: String => String\./u,
    );
    t.assert.match(
      output,
      /cell-is-used cell name = \(to-pant-term-name name in name-registry--used \(synth-cell--registry cell\)\)\./u,
    );
  });

  it("cellLookupRecord — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "cellLookupRecord");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Cascade narrow: the body returns `lookupRecordShape(...)`.
    // `lookupRecordShape`'s body is itself a bare Map.get, so its
    // signature narrows from `[RecordSynthEntry]` to bare V. The
    // cascade then narrows `cellLookupRecord` too — without it, the
    // bare V from the call wouldn't match the list-lifted return
    // declared from `RecordSynthEntry | undefined`.
    t.assert.match(
      output,
      /cell-lookup-record cell: SynthCell, fields\d*: \[RecordSynthField\] => RecordSynthEntry\./u,
    );
  });

  it("cellLookupOpaqueValue — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "cellLookupOpaqueValue");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // This is the opaque-synth dogfood case for the M1 free-call floor:
    // the body calls a bare helper (`lookupOpaqueValue`) and the consumer
    // document now gets a typed EUF rule head instead of dangling.
    t.assert.match(
      output,
      /lookup-opaque-value synth: OpaqueSynth, id\d*: String => OpaqueSynthEntry\./u,
    );
    t.assert.match(
      output,
      /cell-lookup-opaque-value cell id = lookup-opaque-value \(synth-cell--opaque-synth cell\) id\./u,
    );
  });

  it("depModuleNameForFile — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "depModuleNameForFile");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
  });

  it("prefixedDigitStem — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "prefixedDigitStem");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // The digit-leading filename repair path is now factored into a
    // let-backed helper, so the emitted equation preserves the stable
    // "F_" prefix as a versioned local binding.
    t.assert.match(output, /^prefix = "F_"\.$/mu);
    t.assert.match(
      output,
      /prefixed-digit-stem stem = prefix \+ stem\./u,
    );
  });

  it("tupleDepModuleName — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "tupleDepModuleName");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Dep module names all end with the tuple-module suffix; the helper
    // is intentionally tiny but verifies immutable-let lowering on a
    // production naming invariant.
    t.assert.match(output, /^suffix = "_TUPLES"\.$/mu);
    t.assert.match(
      output,
      /tuple-dep-module-name safe-stem = safe-stem \+ suffix\./u,
    );
  });

  it("emptyTupleSynth — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "emptyTupleSynth");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Cross-file sibling: `emptyNameRegistry()` from name-registry.ts.
    // Symbol resolution follows the import alias to the actual
    // declaration. Body is an object-literal record return whose
    // `ctorRegistry` field is the call.
    t.assert.match(
      output,
      /empty-name-registry\s+=> NameRegistry\./u,
    );
    t.assert.match(
      output,
      /tuple-synth--ctor-registry empty-tuple-synth = empty-name-registry\./u,
    );
  });
});

describe("dogfood: DU totality", () => {
  const hasSolver = solverAvailable();

  it(
    "emitted DU document contains its totality body proposition",
    { skip: !hasSolver ? "z3 not available" : undefined },
    async () => {
      const doc = await buildDocumentFromPath(
        resolve(FIXTURES, "expressions-discriminant-narrowing.ts"),
        "getRadius",
      );
      const output = await emitAndCheck(doc);
      assert.match(
        output,
        /^all (s\d*): Shape \| shape--kind \1 = "circle" or shape--kind \1 = "square"\.$/mu,
      );
      const result = runCheck(output, {
        projectRoot: PROJECT_ROOT,
        pantBin: getPantBin(),
      });
      assert.match(result.output, /Invariants are jointly satisfiable/u);
      assert.ok(result.passed, `pant --check failed:\n${result.output}`);
    },
  );
});

// `@pant <proposition>` JSDoc annotations on dogfood functions become
// entailment goals in the emitted document's `check` block. This suite
// runs `pant --check` (z3-backed) against each annotated function to
// verify the body actually entails its asserted properties — the
// translation pipeline alone (the `translates and type-checks` suite
// above) only verifies the document parses and the equation is
// well-typed; entailment is what makes annotations load-bearing.
describe("dogfood: @pant annotations entail", () => {
  const hasSolver = solverAvailable();

  const annotated: Array<{ file: string; fn: string; minChecks: number }> = [
    { file: "name-registry.ts", fn: "isUsed", minChecks: 2 },
    { file: "name-registry.ts", fn: "emptyNameRegistry", minChecks: 1 },
    { file: "translate-types.ts", fn: "isUnsupportedUnknown", minChecks: 2 },
    { file: "translate-types.ts", fn: "manglePantTypeToFragment", minChecks: 2 },
    { file: "translate-types.ts", fn: "emptyMapSynth", minChecks: 1 },
    { file: "translate-types.ts", fn: "emptyRecordSynth", minChecks: 1 },
    { file: "translate-types.ts", fn: "emptyOpaqueSynth", minChecks: 3 },
    { file: "translate-types.ts", fn: "lookupMapKV", minChecks: 1 },
    { file: "translate-types.ts", fn: "mapSynthKey", minChecks: 1 },
    { file: "translate-types.ts", fn: "fieldRuleName", minChecks: 1 },
    { file: "translate-types.ts", fn: "lookupRecordShape", minChecks: 1 },
    { file: "translate-types.ts", fn: "lookupOpaqueValue", minChecks: 1 },
    { file: "translate-types.ts", fn: "recordFieldShapeKey", minChecks: 1 },
    { file: "translate-types.ts", fn: "cellIsUsed", minChecks: 2 },
    { file: "translate-types.ts", fn: "cellLookupRecord", minChecks: 1 },
    { file: "translate-types.ts", fn: "cellLookupOpaqueValue", minChecks: 1 },
    { file: "translate-types.ts", fn: "depModuleNameForFile", minChecks: 2 },
    { file: "translate-types.ts", fn: "prefixedDigitStem", minChecks: 1 },
    { file: "translate-types.ts", fn: "tupleDepModuleName", minChecks: 1 },
    { file: "translate-types.ts", fn: "emptyTupleSynth", minChecks: 1 },
    {
      file: resolve(FIXTURES, "expressions-discriminant-narrowing.ts"),
      fn: "getRadius",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-discriminant-narrowing.ts"),
      fn: "dispatchOnKind",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-discriminant-narrowing.ts"),
      fn: "circleOnly",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-discriminant-definedness.ts"),
      fn: "getRadius",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-discriminant-definedness.ts"),
      fn: "getOther",
      minChecks: 2,
    },
    {
      file: resolve(FIXTURES, "expressions-discriminant-definedness.ts"),
      fn: "dispatch",
      minChecks: 2,
    },
    {
      file: resolve(FIXTURES, "expressions-discriminant-definedness.ts"),
      fn: "circleOrZero",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-discriminant-corpus.ts"),
      fn: "litWidth",
      minChecks: 3,
    },
    {
      file: resolve(FIXTURES, "expressions-discriminant-corpus.ts"),
      fn: "ownerOf",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-discriminated-union-nested.ts"),
      fn: "nestedVariantFieldNarrowed",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-discriminated-union-nested.ts"),
      fn: "mapValueNarrowed",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-discriminated-union-nested.ts"),
      fn: "arrayElementNarrowed",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "branded-param-preconditions.ts"),
      fn: "positiveBranded",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "branded-param-preconditions.ts"),
      fn: "nonNegativeBranded",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "branded-param-preconditions.ts"),
      fn: "nonEmptyBranded",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-nullish-narrowing.ts"),
      fn: "strictNullRead",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-nullish-narrowing.ts"),
      fn: "looseNullRead",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-nullish-narrowing.ts"),
      fn: "strictUndefinedRead",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-nullish-narrowing.ts"),
      fn: "longFormRead",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-nullish-narrowing.ts"),
      fn: "earlyReturnComplement",
      minChecks: 1,
    },
    {
      file: resolve(FIXTURES, "expressions-predicate-narrowing.ts"),
      fn: "predicateValue",
      minChecks: 2,
    },
  ];

  for (const { file, fn, minChecks } of annotated) {
    it(
      `${fn} — annotations are entailed by the body`,
      { skip: !hasSolver ? "z3 not available" : undefined },
      async () => {
        const doc = await buildDocumentFromPath(resolve(SRC, file), fn);
        const output = await emitAndCheck(doc);
        const result = runCheck(output, {
          projectRoot: PROJECT_ROOT,
          pantBin: getPantBin(),
        });
        // `OK: Invariants are jointly satisfiable` always lands as the
        // first OK; entailment results follow as one OK per check
        // proposition. Filter out the satisfiability gate so the
        // count reflects user-written annotations.
        const entailments = result.checks.filter((c) =>
          c.message.startsWith("OK: Entailed:") ||
          c.message.startsWith("FAIL: Not entailed:"),
        );
        assert.ok(
          entailments.length >= minChecks,
          `expected ≥ ${minChecks} entailment results, got ${entailments.length}`,
        );
        assert.ok(
          result.passed,
          `pant --check failed:\n${result.output}`,
        );
        assert.ok(entailments.every((c) => c.passed));
      },
    );
  }
});

describe("dogfood: branded-parameter preconditions", () => {
  const hasSolver = solverAvailable();

  it("recognized brand predicates are checkable in skeleton declarations", {
    skip: !hasSolver ? "z3 not available" : undefined,
  }, async () => {
    for (const fn of [
      "positiveBranded",
      "nonNegativeBranded",
      "nonEmptyBranded",
    ]) {
      const doc = await buildDocumentFromPath(
        resolve(FIXTURES, "branded-param-preconditions.ts"),
        fn,
        { noBody: true },
      );
      const output = emitDocument(doc);
      assert.doesNotMatch(output, /> UNSUPPORTED:/u);
      const result = runCheck(output, {
        projectRoot: PROJECT_ROOT,
        pantBin: getPantBin(),
      });
      assert.ok(
        result.passed,
        `pant --check failed for ${fn}:\n${result.output}`,
      );
    }
  });

  it("brand and Effect guards are AND-combined", {
    skip: !hasSolver ? "z3 not available" : undefined,
  }, async () => {
    const doc = await buildDocumentFromPath(
      resolve(FIXTURES, "branded-param-preconditions.ts"),
      "brandAndEffect",
      { noBody: true },
    );
    const output = emitDocument(doc);
    assert.match(
      output,
      /brand-and-effect x: Int, ~\(x < 10\) and x > 0 => Effect\./u,
    );
    const result = runCheck(withOpaqueEffectDomain(output), {
      projectRoot: PROJECT_ROOT,
      pantBin: getPantBin(),
    });
    assert.ok(result.passed, `pant --check failed:\n${result.output}`);
  });

  it("unrecognized and unbranded parameters keep the unguarded base type", {
    skip: !hasSolver ? "z3 not available" : undefined,
  }, async () => {
    for (const fn of ["unrecognizedBrand", "unbrandedControl"]) {
      const doc = await buildDocumentFromPath(
        resolve(FIXTURES, "branded-param-preconditions.ts"),
        fn,
        { noBody: true },
      );
      const output = emitDocument(doc);
      const decl = output
        .split("\n")
        .find((line) => line.startsWith(toPantFixtureName(fn)));
      assert.equal(decl?.trim(), `${toPantFixtureName(fn)} x: Int => Int.`);
      const result = runCheck(output, {
        projectRoot: PROJECT_ROOT,
        pantBin: getPantBin(),
      });
      assert.ok(
        result.passed,
        `pant --check failed for ${fn}:\n${result.output}`,
      );
    }
  });
});

describe("dogfood: Effect error-channel preconditions", () => {
  const hasSolver = solverAvailable();

  it(
    "fully recovered Effect-generator preconditions are checkable",
    { skip: !hasSolver ? "z3 not available" : undefined },
    async () => {
      for (const fn of ["singleError", "multiError", "combinedWithThrow"]) {
        const doc = await buildDocumentFromPath(
          resolve(FIXTURES, "effect-gen-error-guards.ts"),
          fn,
          { noBody: true },
        );
        const output = emitDocument(doc);
        assert.doesNotMatch(output, /> UNSUPPORTED:/u);
        const result = runCheck(withOpaqueEffectDomain(output), {
          projectRoot: PROJECT_ROOT,
          pantBin: getPantBin(),
        });
        assert.ok(
          result.passed,
          `pant --check failed for ${fn}:\n${result.output}`,
        );
      }
    },
  );

  it(
    "partial and impure Effect-generator recovery bail without a guard",
    { skip: !hasSolver ? "z3 not available" : undefined },
    async () => {
      for (const fn of ["partialRecovery", "impureCond", "unmatchedShape"]) {
        const doc = await buildDocumentFromPath(
          resolve(FIXTURES, "effect-gen-error-guards.ts"),
          fn,
          { noBody: true },
        );
        const output = emitDocument(doc);
        const decl = output
          .split("\n")
          .find((line) => line.startsWith(toPantFixtureName(fn)));
        assert.ok(decl, `missing declaration for ${fn}`);
        assert.equal(
          decl.split("=>", 1)[0]!.trim(),
          `${toPantFixtureName(fn)} x: Int`,
        );
      }
    },
  );
});

function toPantFixtureName(name: string): string {
  return name.replace(/[A-Z]/gu, (c) => `-${c.toLowerCase()}`);
}

function withOpaqueEffectDomain(output: string): string {
  return output.replace(/\n\n/u, "\n\nEffect.\n");
}

describe("dogfood: DU definedness", () => {
  const hasSolver = solverAvailable();

  it(
    "un-narrowed read is not entailed",
    { skip: !hasSolver ? "z3 not available" : undefined },
    async () => {
      const doc = await buildDocumentFromPath(
        resolve(FIXTURES, "expressions-discriminant-definedness.ts"),
        "readUnchecked",
      );
      const output = await emitAndCheck(doc);
      const result = runCheck(output, {
        projectRoot: PROJECT_ROOT,
        pantBin: getPantBin(),
      });
      const entailments = result.checks.filter((c) =>
        c.message.startsWith("OK: Entailed:") ||
        c.message.startsWith("FAIL: Not entailed:"),
      );
      assert.ok(
        entailments.length >= 1,
        `expected at least one entailment result, got ${entailments.length}`,
      );
      assert.equal(result.passed, false, result.output);
      assert.ok(entailments.some((c) => !c.passed), result.output);
    },
  );

  it(
    "nested DU un-narrowed read is not entailed",
    { skip: !hasSolver ? "z3 not available" : undefined },
    async () => {
      const doc = await buildDocumentFromPath(
        resolve(FIXTURES, "expressions-discriminated-union-nested.ts"),
        "recordFieldUnchecked",
      );
      const output = await emitAndCheck(doc);
      const result = runCheck(output, {
        projectRoot: PROJECT_ROOT,
        pantBin: getPantBin(),
      });
      const entailments = result.checks.filter((c) =>
        c.message.startsWith("OK: Entailed:") ||
        c.message.startsWith("FAIL: Not entailed:"),
      );
      assert.ok(
        entailments.length >= 1,
        `expected at least one entailment result, got ${entailments.length}`,
      );
      assert.equal(result.passed, false, result.output);
      assert.ok(entailments.some((c) => !c.passed), result.output);
    },
  );
});

describe("dogfood: nullish definedness", () => {
  const hasSolver = solverAvailable();

  it(
    "optional read in null branch is not entailed",
    { skip: !hasSolver ? "z3 not available" : undefined },
    async () => {
      const doc = await buildDocumentFromPath(
        resolve(FIXTURES, "expressions-nullish-narrowing.ts"),
        "nullBranchControl",
      );
      const output = await emitAndCheck(doc);
      const result = runCheck(output, {
        projectRoot: PROJECT_ROOT,
        pantBin: getPantBin(),
      });
      const entailments = result.checks.filter((c) =>
        c.message.startsWith("OK: Entailed:") ||
        c.message.startsWith("FAIL: Not entailed:") ||
        c.message.startsWith("UNKNOWN: Entailed:"),
      );
      assert.ok(
        entailments.length >= 1 || /UNKNOWN: Entailed:/u.test(result.output),
        `expected at least one entailment result, got ${entailments.length}`,
      );
      assert.ok(
        entailments.some((c) => !c.passed) ||
          result.checks.some((c) => c.message.startsWith("UNKNOWN: Entailed:")) ||
          /UNKNOWN: Entailed:/u.test(result.output),
        result.output,
      );
    },
  );
});

describe("dogfood: predicate definedness", () => {
  const hasSolver = solverAvailable();

  it(
    "intractable type-predicate read is not discharged",
    { skip: !hasSolver ? "z3 not available" : undefined },
    async () => {
      const doc = await buildDocumentFromPath(
        resolve(FIXTURES, "expressions-predicate-narrowing.ts"),
        "predicateCircleRadius",
      );
      const output = await emitAndCheck(doc);
      const result = runCheck(output, {
        projectRoot: PROJECT_ROOT,
        pantBin: getPantBin(),
      });
      const entailments = result.checks.filter((c) =>
        c.message.startsWith("OK: Entailed:") ||
        c.message.startsWith("FAIL: Not entailed:"),
      );
      assert.ok(
        entailments.length >= 1,
        `expected at least one entailment result, got ${entailments.length}`,
      );
      assert.equal(result.passed, false, result.output);
      assert.ok(entailments.some((c) => !c.passed), result.output);
    },
  );
});
