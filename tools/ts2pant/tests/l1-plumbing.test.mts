/**
 * Regression tests for the L1 conditional pipeline (workstream M1).
 *
 * After M1 patch 3 (hard-rule cutover), all conditional value forms —
 * if-with-returns, ternary chains, switch w/o fall-through, Bool-typed
 * `&&`/`||` — flow through the L1 builder. These tests verify the
 * resulting Pant for representative shapes and lock in the rejection
 * reasons for cases the L1 builder conservatively refuses (workstream
 * policy 3(b)).
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
    return { unsupported: `non-equation kind: ${p.kind}`, pant: null };
  }
  const ast = getAst();
  return { unsupported: null, pant: ast.strExpr(p.rhs) };
}

// ---------------------------------------------------------------------------
// L1 capabilities — switch, multi-arm if-chains, flat ternary chains
// ---------------------------------------------------------------------------

describe("L1: switch (clean) translates", () => {
  it("switch with literal numeric cases and default → flat cond", () => {
    const source = `
      function classify(x: number): number {
        switch (x) {
          case 0: return 100;
          case 1: return 200;
          default: return 300;
        }
      }
    `;
    const { unsupported, pant } = translate(source, "classify");
    assert.equal(unsupported, null);
    assert.equal(
      pant,
      "cond x = 0 => 100, x = 1 => 200, true => 300",
    );
  });

  it("switch with string cases", () => {
    const source = `
      function tag(s: string): number {
        switch (s) {
          case "a": return 1;
          case "b": return 2;
          default: return 0;
        }
      }
    `;
    const { unsupported } = translate(source, "tag");
    assert.equal(unsupported, null);
  });

  it("switch with only default collapses to the default value", () => {
    const source = `
      function constant(x: number): number {
        switch (x) {
          default: return 42;
        }
      }
    `;
    const { unsupported, pant } = translate(source, "constant");
    assert.equal(unsupported, null);
    assert.equal(pant, "42");
  });
});

describe("L1: flat-cond canonicalization", () => {
  it("right-leaning ternary chain flattens to one cond", () => {
    const source = `
      function bucket(n: number): number {
        return n < 0 ? 0 : n < 10 ? 1 : n < 100 ? 2 : 3;
      }
    `;
    const { unsupported, pant } = translate(source, "bucket");
    assert.equal(unsupported, null);
    assert.equal(
      pant,
      "cond n < 0 => 0, n < 10 => 1, n < 100 => 2, true => 3",
    );
  });

  it("if/else-if/else chain flattens to one cond", () => {
    const source = `
      function classify(n: number): number {
        if (n < 0) {
          return -1;
        } else if (n === 0) {
          return 0;
        } else {
          return 1;
        }
      }
    `;
    const { unsupported, pant } = translate(source, "classify");
    assert.equal(unsupported, null);
    assert.equal(
      pant,
      "cond n < 0 => -1, n = 0 => 0, true => 1",
    );
  });

  it("early-return arm + ternary terminal merges into one flat cond", () => {
    const source = `
      function nested(n: number): number {
        if (n < 0) {
          return -1;
        }
        return n === 0 ? 0 : 1;
      }
    `;
    const { unsupported, pant } = translate(source, "nested");
    assert.equal(unsupported, null);
    assert.equal(
      pant,
      "cond n < 0 => -1, n = 0 => 0, true => 1",
    );
  });
});

// ---------------------------------------------------------------------------
// L1 conservative-refusal — locked rejection reasons (policy 3(b))
// ---------------------------------------------------------------------------

describe("L1: conservative-refusal rejection cases", () => {
  it("switch with default not last rejects with specific reason", () => {
    const source = `
      function badOrder(x: number): number {
        switch (x) {
          default: return 0;
          case 0: return 1;
        }
      }
    `;
    const { unsupported } = translate(source, "badOrder");
    assert.notEqual(unsupported, null);
    assert.match(unsupported!, /default must be the last/);
  });

  it("switch fall-through (case body not ending in return) rejects", () => {
    const source = `
      function fallthrough(x: number): number {
        switch (x) {
          case 0:
          case 1: return 1;
          default: return 0;
        }
      }
    `;
    const { unsupported } = translate(source, "fallthrough");
    assert.notEqual(unsupported, null);
    assert.match(unsupported!, /case must end with `return EXPR`/);
  });

  it("switch break-only case rejects", () => {
    const source = `
      function breakOnly(x: number): number {
        switch (x) {
          case 0: { break; }
          default: { return 0; }
        }
      }
    `;
    const { unsupported } = translate(source, "breakOnly");
    assert.notEqual(unsupported, null);
  });

  it("non-literal switch case label rejects", () => {
    const source = `
      function nonLiteral(x: number, k: number): number {
        switch (x) {
          case k: return 1;
          default: return 0;
        }
      }
    `;
    const { unsupported } = translate(source, "nonLiteral");
    assert.notEqual(unsupported, null);
    assert.match(unsupported!, /literal/);
  });

  it("non-Bool && stays on the legacy operator path (degraded but accepted)", () => {
    // Non-Bool `&&` / `||` is *not* an L1 conditional form — `isL1ConditionalForm`
    // requires both operands Bool-typed. So `a && b` with `a: number` falls
    // through to the legacy `translateOperator` path and lowers to `a and b`
    // as a binop. Translation succeeds.
    const source = `
      function combine(a: number, b: number): number {
        return a && b;
      }
    `;
    const { unsupported, pant } = translate(source, "combine");
    assert.equal(unsupported, null);
    assert.match(pant!, /and/);
  });
});

// ---------------------------------------------------------------------------
// Forms that already produced flat output before cutover are unchanged
// ---------------------------------------------------------------------------

describe("L1: forms unchanged from pre-cutover", () => {
  it("single ternary lowers to single-arm cond", () => {
    const source = `
      function abs(n: number): number {
        return n >= 0 ? n : -n;
      }
    `;
    const { unsupported, pant } = translate(source, "abs");
    assert.equal(unsupported, null);
    assert.equal(pant, "cond n >= 0 => n, true => -n");
  });

  it("single-arm early-return + plain terminal lowers correctly", () => {
    const source = `
      function piecewise(n: number): number {
        if (n < 0) {
          return -1;
        }
        return n;
      }
    `;
    const { unsupported, pant } = translate(source, "piecewise");
    assert.equal(unsupported, null);
    assert.equal(pant, "cond n < 0 => -1, true => n");
  });

  it("two-branch if/else translates to single-arm cond", () => {
    const source = `
      function abs2(n: number): number {
        if (n >= 0) {
          return n;
        } else {
          return -n;
        }
      }
    `;
    const { unsupported, pant } = translate(source, "abs2");
    assert.equal(unsupported, null);
    assert.equal(pant, "cond n >= 0 => n, true => -n");
  });

  it("Bool-typed && translates to binop", () => {
    const source = `
      function bothPositive(a: number, b: number): boolean {
        return a > 0 && b > 0;
      }
    `;
    const { unsupported, pant } = translate(source, "bothPositive");
    assert.equal(unsupported, null);
    assert.equal(pant, "a > 0 and b > 0");
  });

  it("Bool-typed || translates to binop", () => {
    const source = `
      function eitherPositive(a: number, b: number): boolean {
        return a > 0 || b > 0;
      }
    `;
    const { unsupported, pant } = translate(source, "eitherPositive");
    assert.equal(unsupported, null);
    assert.equal(pant, "a > 0 or b > 0");
  });
});
