import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import { loadAst } from "../src/pant-wasm.js";
import type { PantDocument } from "../src/types.js";
import { containsUnsupportedLine, emitAndCheck } from "./helpers.mjs";

before(async () => {
  await loadAst();
});

/**
 * Build a minimal PantDocument fixture programmatically. Avoids the
 * full ts-morph translation pipeline so the helpers under test stay
 * the only moving part.
 */
function buildDoc(opts: {
  moduleName: string;
  declarations?: PantDocument["declarations"];
  propositions: PantDocument["propositions"];
  bundleModules?: Map<string, string>;
}): PantDocument {
  return {
    moduleName: opts.moduleName,
    imports: [],
    declarations: opts.declarations ?? [],
    propositions: opts.propositions,
    checks: [],
    bundleModules: opts.bundleModules,
  };
}

describe("helpers > containsUnsupportedLine", () => {
  it("matches a `> UNSUPPORTED:` line at start-of-line", () => {
    const text = "module Foo.\n\n---\n\n> UNSUPPORTED: bar.\n";
    assert.equal(containsUnsupportedLine(text), true);
  });

  it("does not match `> UNSUPPORTED:` mid-line", () => {
    const text = "module Foo.\n\n---\n\nfoo > UNSUPPORTED: bar.\n";
    assert.equal(containsUnsupportedLine(text), false);
  });

  it("returns false on a clean document", () => {
    const text = "module Foo.\n\n---\n\ntrue.\n";
    assert.equal(containsUnsupportedLine(text), false);
  });
});

describe("helpers > emitAndCheck", () => {
  it("skips wasm typecheck on `> UNSUPPORTED:`", async () => {
    // The propositions list contains an "unsupported" PropResult, so
    // `emitDocument` writes a `> UNSUPPORTED:` marker. The reason text
    // ("not real Pant") is also not Pantagruel-valid as a free-standing
    // proposition — if `emitAndCheck` did NOT skip the typecheck, the
    // wasm checker would reject and throw. The test passing without a
    // throw is the deliberate-rejection skip working.
    const doc = buildDoc({
      moduleName: "Skipped",
      propositions: [{ kind: "unsupported", reason: "not real Pant" }],
    });

    const output = await emitAndCheck(doc);

    assert.match(output, /> UNSUPPORTED: not real Pant/u);
    assert.ok(
      containsUnsupportedLine(output),
      "snapshot should still capture the marker line",
    );
  });

  it("still typechecks documents without UNSUPPORTED", async () => {
    // A clean doc with a single domain declaration and a trivial
    // `true.` proposition should round-trip through the wasm checker
    // without complaint.
    const doc = buildDoc({
      moduleName: "Clean",
      declarations: [{ kind: "domain", name: "Item" }],
      propositions: [{ kind: "raw", text: "true" }],
    });

    const output = await emitAndCheck(doc);

    assert.ok(!containsUnsupportedLine(output));
    assert.match(output, /module Clean\./u);
  });

  it("passes deps to checkPantBundle when bundleModules is set", async () => {
    // The wasm bridge parses every dep entry up front (see
    // `parse_deps` in wasm/pant_wasm.ml), so a syntactically broken
    // dep surfaces as a parse error from `assertWasmTypeChecks` even
    // though the consumer module never imports it. If `emitAndCheck`
    // did NOT forward `bundleModules`, the broken dep would never be
    // parsed and this test would mistakenly succeed.
    const brokenDep = "this is not pantagruel at all";
    const doc = buildDoc({
      moduleName: "Consumer",
      propositions: [{ kind: "raw", text: "true" }],
      bundleModules: new Map([["BROKEN_DEP", brokenDep]]),
    });

    await assert.rejects(
      () => emitAndCheck(doc),
      /pant typecheck failed/u,
      "broken dep in bundleModules must surface as a typecheck failure",
    );
  });
});
