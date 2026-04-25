/**
 * Integration tests for the M2 L1 μ-search pipeline (workstream M2
 * patch 2).
 *
 * Sets `TS2PANT_USE_L1_MUSEARCH=1` so the L1 path is exercised
 * end-to-end. Asserts:
 *
 * 1. **Byte-equality with legacy** for the existing μ-search shapes
 *    (`i++`, `++i`, unbraced body, post-loop counter use, +const-binding
 *    interleave). Translating the same source under flag-off vs flag-on
 *    produces identical Pant. This is the cutover gate for M2 patch 3.
 *
 * 2. **L1 path accepts the broader `+1` spellings** (`i += 1`,
 *    `i = i + 1`, `i = 1 + i`) and produces the same canonical
 *    `min over each j: Int, j >= 1, ~(j in used) | j` form.
 *
 * 3. **L1 recognizer rejection** for non-canonical shapes that the
 *    TS-AST recognizer accepts (none today; the TS recognizer is
 *    narrower than L1 in M2 patch 2 — patch 3 will widen TS-AST and
 *    rely on the L1 recognizer for canonical-shape rejection).
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

function withFlag<T>(flag: string, value: string, fn: () => T): T {
  const prev = process.env[flag];
  process.env[flag] = value;
  try {
    return fn();
  } finally {
    if (prev === undefined) {
      delete process.env[flag];
    } else {
      process.env[flag] = prev;
    }
  }
}

function withoutFlag<T>(flag: string, fn: () => T): T {
  const prev = process.env[flag];
  delete process.env[flag];
  try {
    return fn();
  } finally {
    if (prev !== undefined) {
      process.env[flag] = prev;
    }
  }
}

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

function translateBoth(source: string, name: string) {
  const legacy = withoutFlag("TS2PANT_USE_L1_MUSEARCH", () =>
    translate(source, name),
  );
  const l1 = withFlag("TS2PANT_USE_L1_MUSEARCH", "1", () =>
    translate(source, name),
  );
  return { legacy, l1 };
}

// ---------------------------------------------------------------------------
// 1. Byte-equality with legacy on existing shapes (cutover gate)
// ---------------------------------------------------------------------------

describe("M2 μ-search L1: byte-equality with legacy", () => {
  it("`i++` step produces identical output", () => {
    const source = `
      function firstUnused(used: ReadonlySet<number>): number {
        let i = 1;
        while (used.has(i)) i++;
        return i;
      }
    `;
    const { legacy, l1 } = translateBoth(source, "firstUnused");
    assert.equal(legacy.unsupported, null);
    assert.equal(l1.unsupported, null);
    assert.equal(legacy.pant, l1.pant);
    assert.match(l1.pant!, /min over each j\d*: Int/);
  });

  it("`++i` step produces identical output", () => {
    const source = `
      function firstUnused(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) ++i;
        return i;
      }
    `;
    const { legacy, l1 } = translateBoth(source, "firstUnused");
    assert.equal(legacy.unsupported, null);
    assert.equal(l1.unsupported, null);
    assert.equal(legacy.pant, l1.pant);
  });

  it("μ-search counter referenced post-loop", () => {
    const source = `
      function nextSlotPlusOne(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) i++;
        return i + 1;
      }
    `;
    const { legacy, l1 } = translateBoth(source, "nextSlotPlusOne");
    assert.equal(legacy.unsupported, null);
    assert.equal(l1.unsupported, null);
    assert.equal(legacy.pant, l1.pant);
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
    const { legacy, l1 } = translateBoth(source, "offset");
    assert.equal(legacy.unsupported, null);
    assert.equal(l1.unsupported, null);
    assert.equal(legacy.pant, l1.pant);
  });
});

// ---------------------------------------------------------------------------
// 2. L1 accepts broader +1 spellings; all produce the canonical form
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
    const { l1 } = translateBoth(source, "findSuffix");
    if (l1.unsupported) {
      throw new Error(`unsupported: ${l1.unsupported}`);
    }
    return l1.pant!;
  }

  it("all five spellings produce identical L1 output", () => {
    // Each translation gets a fresh `UniqueSupply` and a fresh
    // `SourceFile`, so binder allocation is deterministic per call.
    // The five spellings must produce byte-identical Pant — that's the
    // M2 architectural promise.
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
// 3. Existing rejection cases remain rejected
// ---------------------------------------------------------------------------

describe("M2 μ-search L1: rejection cases", () => {
  it("compound while body still rejects", () => {
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
    const { legacy, l1 } = translateBoth(source, "compound");
    assert.notEqual(legacy.unsupported, null);
    assert.notEqual(l1.unsupported, null);
  });

  it("`i += 2` step rejects under both modes", () => {
    const source = `
      function nonUnit(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) {
          i += 2;
        }
        return i;
      }
    `;
    const { legacy, l1 } = translateBoth(source, "nonUnit");
    // Legacy (with the broadened isPlusOneStep at TS-AST recognizer)
    // also rejects this — `i += 2` is not a +1 step.
    assert.notEqual(legacy.unsupported, null);
    assert.notEqual(l1.unsupported, null);
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
    const { legacy, l1 } = translateBoth(source, "noRef");
    assert.notEqual(legacy.unsupported, null);
    assert.notEqual(l1.unsupported, null);
  });
});
