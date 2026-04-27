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
import { translateSignature } from "../src/translate-signature.js";
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

// ---------------------------------------------------------------------------
// Signature / guard path: loose equality must reject cleanly rather than
// fall through to `ast.var(expr.getText())`, which would emit raw source
// text as a Pant variable name (conservative-refusal policy 3(b)).
// ---------------------------------------------------------------------------

describe("equality-canonicalization (signature/guard path)", () => {
  it("assertion guard with == drops the guard rather than emit raw text", () => {
    const source = `
      function assert(condition: unknown, msg?: string): asserts condition {
        if (!condition) throw new Error(msg ?? "Assertion failed");
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        assert(amount == 0, "must be zero");
        account.balance = account.balance + amount;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const result = translateSignature(sourceFile, "deposit", IntStrategy);
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    // The guard must NOT be the raw text `amount == 0` — bail cleanly.
    assert.equal(result.declaration.guard, undefined);
  });

  it("if-throw guard with != drops the guard rather than emit raw text", () => {
    const source = `
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        if (amount != 0) { throw new Error("nonzero"); }
        account.balance = account.balance + amount;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const result = translateSignature(sourceFile, "deposit", IntStrategy);
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(result.declaration.guard, undefined);
  });

  it("if-throw guard with strict !== still produces a guard", () => {
    // Sanity check that the bail logic is operator-specific — strict
    // inequality continues to work in guard extraction.
    const source = `
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        if (amount !== 0) { throw new Error("nonzero"); }
        account.balance = account.balance + amount;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const result = translateSignature(sourceFile, "deposit", IntStrategy);
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(
      getAst().strExpr(result.declaration.guard!),
      "~(amount ~= 0)",
    );
  });

  it("nested == inside a strict-eq guard also bails", () => {
    // The throw is inside translateExpr's recursion; outer translateExpr
    // catches it through the wrapped entry-point caller.
    const source = `
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        if ((amount == 0) === false) { throw new Error("guarded"); }
        account.balance = account.balance + amount;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const result = translateSignature(sourceFile, "deposit", IntStrategy);
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(result.declaration.guard, undefined);
  });
});
