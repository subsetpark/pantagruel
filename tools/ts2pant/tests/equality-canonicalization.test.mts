/**
 * M4 P3 — strict equality canonicalizes through L1; loose equality rejects.
 *
 * The dispatcher in `translate-body.ts` routes `===`/`!==` through Layer 1
 * `BinOp(eq | neq, ...)` (then `lowerL1Expr` → `lowerExpr`); any
 * `==`/`!=` returns an `unsupported` `BodyResult`. Patch 2's nullish
 * recognizer consumes `x == null` / `x != null` before this dispatcher
 * fires; the rejection here covers everything else.
 */

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import { createSourceFileFromSource } from "../src/extract.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { translateBody } from "../src/translate-body.js";
import { IntStrategy } from "../src/translate-types.js";
import type { PropResult } from "../src/types.js";

before(async () => {
  await loadAst();
});

const LOOSE_EQ_REASON =
  "loose equality (== / !=) is unsupported; use === / !==";

function translateOne(source: string, functionName: string): PropResult[] {
  const sourceFile = createSourceFileFromSource(source);
  return translateBody({
    sourceFile,
    functionName,
    strategy: IntStrategy,
  });
}

function expectEquationStr(props: PropResult[]): string {
  assert.equal(props.length, 1);
  const prop = props[0]!;
  assert.equal(prop.kind, "equation");
  if (prop.kind !== "equation") {
    throw new Error("unreachable");
  }
  return getAst().strExpr(prop.rhs);
}

function expectUnsupported(props: PropResult[]): string {
  assert.equal(props.length, 1);
  const prop = props[0]!;
  assert.equal(prop.kind, "unsupported");
  if (prop.kind !== "unsupported") {
    throw new Error("unreachable");
  }
  return prop.reason;
}

describe("equality-canonicalization", () => {
  it("=== builds binop(eq) via L1", () => {
    const props = translateOne(
      `export function f(a: number, b: number): boolean {
        return a === b;
      }`,
      "f",
    );
    assert.equal(expectEquationStr(props), "a = b");
  });

  it("!== builds binop(neq) via L1", () => {
    const props = translateOne(
      `export function f(a: number, b: number): boolean {
        return a !== b;
      }`,
      "f",
    );
    assert.equal(expectEquationStr(props), "a ~= b");
  });

  it("== (non-nullish) rejects with unsupported reason", () => {
    const props = translateOne(
      `export function f(a: number, b: number): boolean {
        return a == b;
      }`,
      "f",
    );
    assert.equal(expectUnsupported(props), LOOSE_EQ_REASON);
  });

  it("!= (non-nullish) rejects with unsupported reason", () => {
    const props = translateOne(
      `export function f(a: number, b: number): boolean {
        return a != b;
      }`,
      "f",
    );
    assert.equal(expectUnsupported(props), LOOSE_EQ_REASON);
  });

  it("=== preserves operand sub-expression translation", () => {
    // The LHS `a + 1` and RHS `b * 2` must reach the L1 binop as their
    // already-translated forms — the from-l2 wrap is the M5 territory
    // mentioned in the workstream.
    const props = translateOne(
      `export function f(a: number, b: number): boolean {
        return a + 1 === b * 2;
      }`,
      "f",
    );
    assert.equal(expectEquationStr(props), "a + 1 = b * 2");
  });

  it("!== inside a return expression composes with cond", () => {
    // The if-with-return prelude builds a cond whose first arm's
    // value is the translated `a !== b`. Verifies that the L1
    // canonicalization integrates with the M1 cond pipeline.
    const props = translateOne(
      `export function f(a: number, b: number): boolean {
        if (a < 0) return a !== b;
        return a === b;
      }`,
      "f",
    );
    assert.equal(
      expectEquationStr(props),
      "cond a < 0 => a ~= b, true => a = b",
    );
  });
});
