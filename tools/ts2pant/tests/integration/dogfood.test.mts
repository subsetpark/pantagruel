import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { runCheck } from "../../src/emit.js";
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

  it("lookupMapKV — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "lookupMapKV");
    const output = await emitAndCheck(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output, PANT_TIMEOUT_MS);
    // Body composes two pieces that landed across recent PRs: the
    // template literal `` `${kType}|${vType}` `` lowers to a `+` chain
    // (TS_PRELUDE wiring in the same module is unused here because both
    // operands are String), and `synth.byKV.get(...)` lowers to the
    // partial-rule value application against the synthesized
    // MapSynth domain.
    t.assert.match(
      output,
      /lookup-map-kv synth k-type v-type = map-synth--by-kv synth \(k-type \+ "\|" \+ v-type\)\./u,
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
    { file: "translate-types.ts", fn: "isUnsupportedUnknown", minChecks: 2 },
    { file: "translate-types.ts", fn: "emptyMapSynth", minChecks: 1 },
    { file: "translate-types.ts", fn: "cellIsUsed", minChecks: 2 },
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
