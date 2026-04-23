import { execFileSync } from "node:child_process";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import {
  buildDocument as buildDocumentFromPath,
  emitDocument,
} from "./helpers.mjs";

// Dogfood: translate ts2pant's own source files with ts2pant itself.
// Depth-first — one target module at a time, smallest function first.
// Each passing case here is a concrete self-translation baseline; failing
// cases are tracked in the project roadmap as diagnostics driving the next
// feature to build.

const SRC = resolve(import.meta.dirname, "../src");
const PROJECT_ROOT = resolve(import.meta.dirname, "../../..");
const PANT_TIMEOUT_MS = Number(
  process.env.TS2PANT_DOGFOOD_TIMEOUT_MS ?? 30_000,
);

function assertPantTypeChecks(output: string): void {
  execFileSync("dune", ["exec", "pant", "--"], {
    cwd: PROJECT_ROOT,
    input: output,
    encoding: "utf-8",
    timeout: PANT_TIMEOUT_MS,
    killSignal: "SIGKILL",
  });
}

describe("dogfood: src/name-registry.ts", () => {
  const filePath = resolve(SRC, "name-registry.ts");

  it("isUsed — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "isUsed");
    const output = emitDocument(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output);
  });

  it("emptyNameRegistry — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "emptyNameRegistry");
    const output = emitDocument(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output);
  });
});

describe("dogfood: src/translate-types.ts", () => {
  const filePath = resolve(SRC, "translate-types.ts");

  it("emptyMapSynth — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "emptyMapSynth");
    const output = emitDocument(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output);
    // `MapSynth.byKV: ReadonlyMap<string, MapSynthEntry>` → Stage A
    // encoding: a binary membership predicate alongside the value rule.
    t.assert.match(output, /byKVKey m\d*: MapSynth, k: String => Bool\./u);
    t.assert.match(
      output,
      /byKV m\d*: MapSynth, k: String, byKVKey m\d* k => MapSynthEntry\./u,
    );
    // `return { byKV: new Map(), emitted: new Set() }` → empty-map
    // initializer emits Stage A membership-negation assertion.
    t.assert.match(
      output,
      /all k\d*: String \| ~\(byKVKey emptyMapSynth k\d*\)\./u,
    );
    // `collectNamedTypes` recursion must follow `ReadonlyMap<K, V>` into V:
    // `MapSynthEntry` is only reachable through `byKV`'s value type.
    t.assert.match(output, /^MapSynthEntry\.$/mu);
  });

  it("emptyRecordSynth — translates and type-checks", async (t) => {
    const doc = await buildDocumentFromPath(filePath, "emptyRecordSynth");
    const output = emitDocument(doc);
    t.assert.snapshot(output);
    assertPantTypeChecks(output);
    // Same three invariants on the sibling RecordSynth type. Paired with
    // the MapSynth case so a regression in any one of ReadonlyMap-field
    // Stage A encoding, empty-map initializer emission, or
    // collectNamedTypes recursion surfaces on a focused assertion rather
    // than only on a compound snapshot.
    t.assert.match(
      output,
      /byShapeKey r\d*: RecordSynth, k: String => Bool\./u,
    );
    t.assert.match(
      output,
      /all k\d*: String \| ~\(byShapeKey emptyRecordSynth k\d*\)\./u,
    );
    t.assert.match(output, /^RecordSynthEntry\.$/mu);
  });
});
