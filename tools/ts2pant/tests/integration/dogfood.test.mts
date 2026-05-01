import { resolve } from "node:path";
import { describe, it } from "node:test";
import {
  assertPantTypeChecks,
  buildDocument as buildDocumentFromPath,
  emitAndCheck,
} from "../helpers.mjs";

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
});
