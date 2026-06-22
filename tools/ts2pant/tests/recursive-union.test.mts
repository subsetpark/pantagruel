// @archlint.module test
// @archlint.domain ts2pant.recursive-union

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";

import { emitDocument } from "../src/emit.js";
import { loadAst } from "../src/pant-wasm.js";
import { buildDocument, emitAndCheck } from "./helpers.mjs";

const FIXTURE = resolve(
  import.meta.dirname,
  "fixtures/recursive-union/cases.ts",
);

describe("recursive discriminated union", () => {
  before(async () => {
    await loadAst();
  });

  it("translates a self-referential union to a domain with self-referential accessors", async () => {
    // `Tree`'s `node` variant has `left`/`right`: `Tree`. DU synthesis once
    // recursed forever (stack overflow), then refused; it now reserves the
    // `Tree` domain before mapping fields so the recursive fields resolve to
    // it. The accessor `tree--left … => Tree` is a total uninterpreted
    // function — a sound EUF encoding — and the emitted Pant type-checks.
    const output = await emitAndCheck(await buildDocument(FIXTURE, "treeKind"));
    assert.doesNotMatch(output, /UNSUPPORTED/u);
    assert.match(output, /^Tree\.$/mu);
    assert.match(
      output,
      /tree--left s: Tree, tree--kind s = "node" => Tree\./u,
    );
    assert.match(
      output,
      /tree--value s: Tree, tree--kind s = "leaf" => Int\./u,
    );
    // The body resolves against the same `Tree` domain, not a duplicate.
    assert.match(output, /tree-kind t = tree--kind t\./u);
    assert.doesNotMatch(output, /Tree1|Tree2/u);
  });

  // `treeKind` reads the discriminant; `leafValue` reads a leaf field under
  // `kind === "leaf"` narrowing; `leftChildKind` reads the *self-referential*
  // `node.left: Tree` field under `kind === "node"` narrowing, then its
  // discriminant — exercising the recursive accessor `tree--left … => Tree`.
  for (const fn of ["treeKind", "leafValue", "leftChildKind"]) {
    it(`emits and type-checks @pant on \`${fn}\` (recursive union)`, async () => {
      await emitAndCheck(await buildDocument(FIXTURE, fn));
    });
  }

  it("reads a self-referential field under narrowing", async () => {
    const output = await emitAndCheck(
      await buildDocument(FIXTURE, "leftChildKind"),
    );
    // The `node.left` read resolves through the recursive accessor.
    assert.match(output, /tree--kind \(tree--left t\)/u);
  });

  it("a non-recursive discriminated union still registers (control)", async () => {
    const output = emitDocument(await buildDocument(FIXTURE, "shapeKind"));
    assert.doesNotMatch(output, /UNSUPPORTED/u);
    assert.match(output, /^Shape\.$/mu);
    assert.match(output, /shape--kind s: Shape => String\./u);
  });
});
