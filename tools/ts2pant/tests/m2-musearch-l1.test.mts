/**
 * Regression tests for the M2 L1 μ-search pipeline (workstream M2).
 *
 * After M2 patch 3 (cutover), the L1 path is the only μ-search path —
 * legacy `translateMuSearchInitLegacy` is deleted and the
 * `TS2PANT_USE_L1_MUSEARCH` flag is gone. These tests assert:
 *
 * 1. **All five `+1` spellings produce byte-identical output** —
 *    `i++`, `++i`, `i += 1`, `i = i + 1`, `i = 1 + i` collapse to
 *    one canonical L1 `Assign(Var(c), BinOp(add, Var(c), Lit(1)))`
 *    that lowers to `min over each j: T, j >= INIT, ¬P(j) | j`.
 *    This is the M2 architectural promise.
 * 2. **Existing μ-search shapes still translate** — `i++` step, post-
 *    loop counter use, const-binding interleave.
 * 3. **Conservative rejections fire** — compound while body,
 *    non-`+1` step, predicate not referencing counter.
 */

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import { createSourceFileFromSource } from "../src/extract.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { translateBody } from "../src/translate-body.js";
import { IntStrategy } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

function translate(
  source: string,
  name: string,
): { unsupported: string | null; pant: string | null } {
  const sourceFile = createSourceFileFromSource(source);
  const props = translateBody({
    sourceFile,
    functionName: name,
    strategy: IntStrategy,
  });
  if (props.length === 0) {
    return { unsupported: "no propositions", pant: null };
  }
  const p = props[0]!;
  if (p.kind === "unsupported") {
    return { unsupported: p.reason, pant: null };
  }
  if (p.kind !== "equation") {
    return { unsupported: `non-equation: ${p.kind}`, pant: null };
  }
  return { unsupported: null, pant: getAst().strExpr(p.rhs) };
}

// ---------------------------------------------------------------------------
// Existing μ-search shapes still translate
// ---------------------------------------------------------------------------

describe("M2 μ-search L1: existing shapes translate", () => {
  it("`i++` step produces canonical min-over-each", () => {
    const source = `
      function firstUnused(used: ReadonlySet<number>): number {
        let i = 1;
        while (used.has(i)) i++;
        return i;
      }
    `;
    const { unsupported, pant } = translate(source, "firstUnused");
    assert.equal(unsupported, null);
    assert.match(pant!, /min over each j\d*: Int/);
  });

  it("μ-search counter referenced post-loop", () => {
    const source = `
      function nextSlotPlusOne(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) i++;
        return i + 1;
      }
    `;
    const { unsupported, pant } = translate(source, "nextSlotPlusOne");
    assert.equal(unsupported, null);
    assert.match(pant!, /min over each j\d*: Int/);
    assert.match(pant!, /\+ 1/);
  });

  it("μ-search interleaved with const binding", () => {
    const source = `
      function offset(used: ReadonlySet<number>, base: number): number {
        const n = base * 2;
        let i = 1;
        while (used.has(i)) i++;
        return n + i;
      }
    `;
    const { unsupported } = translate(source, "offset");
    assert.equal(unsupported, null);
  });
});

// ---------------------------------------------------------------------------
// All five `+1` spellings produce byte-identical canonical output
// ---------------------------------------------------------------------------

describe("M2 μ-search L1: all five +1 spellings produce identical Pant", () => {
  function translateForm(stepSource: string): string {
    const source = `
      function findSuffix(used: ReadonlySet<number>): number {
        let i = 1;
        while (used.has(i)) { ${stepSource}; }
        return i;
      }
    `;
    const { unsupported, pant } = translate(source, "findSuffix");
    if (unsupported) {
      throw new Error(`unsupported: ${unsupported}`);
    }
    return pant!;
  }

  it("all five spellings produce identical L1 output", () => {
    // Each translation gets a fresh `UniqueSupply` and `SourceFile`, so
    // binder allocation is deterministic per call. The five spellings
    // must produce byte-identical Pant — the M2 architectural promise.
    const baseline = translateForm("i++");
    assert.match(baseline, /min over each j\d*: Int/);
    assert.match(baseline, /~\(j\d* in used\)/);

    assert.equal(translateForm("++i"), baseline);
    assert.equal(translateForm("i += 1"), baseline);
    assert.equal(translateForm("i = i + 1"), baseline);
    assert.equal(translateForm("i = 1 + i"), baseline);
  });
});

// ---------------------------------------------------------------------------
// Conservative rejections
// ---------------------------------------------------------------------------

describe("M2 μ-search L1: rejection cases", () => {
  it("compound while body rejects", () => {
    const source = `
      function compound(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) {
          i++;
          i++;
        }
        return i;
      }
    `;
    const { unsupported } = translate(source, "compound");
    assert.notEqual(unsupported, null);
  });

  it("`i += 2` step rejects (non-canonical)", () => {
    const source = `
      function nonUnit(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) {
          i += 2;
        }
        return i;
      }
    `;
    const { unsupported } = translate(source, "nonUnit");
    assert.notEqual(unsupported, null);
    assert.match(unsupported!, /canonical|step/);
  });

  it("predicate not referencing counter rejects", () => {
    const source = `
      function noRef(used: ReadonlySet<number>, flag: boolean): number {
        let i = 0;
        while (flag) {
          i++;
        }
        return i;
      }
    `;
    const { unsupported } = translate(source, "noRef");
    assert.notEqual(unsupported, null);
  });
});
