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
import {
  containsUnsupportedOperator,
  translateSignature,
} from "../src/translate-signature.js";
import ts from "typescript";
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

  it("== wrapped in `as`/non-null assertion still rejects (no raw-text fallback)", () => {
    // Without wrapper-unwrapping in translateExpr, `(amount == 0) as boolean`
    // would bypass the loose-equality rejection in the binary branch and
    // fall through to `ast.var(expr.getText())` — emitting raw source as
    // a Pant variable name. Verify the wrappers route back through the
    // binary-expression rejection.
    const sourceAs = `
      function assert(condition: unknown): asserts condition {
        if (!condition) throw new Error();
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        assert((amount == 0) as boolean);
        account.balance = account.balance + amount;
      }
    `;
    const sf1 = createSourceFileFromSource(sourceAs);
    const r1 = translateSignature(sf1, "deposit", IntStrategy);
    assert.equal(r1.declaration.kind, "action");
    if (r1.declaration.kind !== "action") {
      return;
    }
    assert.equal(r1.declaration.guard, undefined);

    // The non-null assertion variant uses a non-nullish loose-eq
    // (`amount != 0`) so Patch 2's nullish recognizer doesn't consume
    // it — the test still verifies that the `!` wrapper routes the
    // rejection back to the binary-expression handler instead of
    // falling through to a raw-text fallback.
    const sourceBang = `
      function assert(condition: unknown): asserts condition {
        if (!condition) throw new Error();
      }
      interface Account { value: number; }
      function setIfNonZero(account: Account, amount: number): void {
        assert((amount != 0)!);
        account.value = amount;
      }
    `;
    const sf2 = createSourceFileFromSource(sourceBang);
    const r2 = translateSignature(sf2, "setIfNonZero", IntStrategy);
    assert.equal(r2.declaration.kind, "action");
    if (r2.declaration.kind !== "action") {
      return;
    }
    assert.equal(r2.declaration.guard, undefined);
  });

  it("nested == inside a strict-eq guard also bails", () => {
    // The unsupported result propagates through translateExpr's
    // internal recursion (binop branch) up to the entry-point caller.
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

  // ------------------------------------------------------------------
  // Predicate alignment: isFollowableGuardCall / isGuardStatement must
  // match scanBodyForGuards on translatability. If they diverge, the
  // body filter silently drops a guard statement that signature
  // extraction also fails to translate — losing the runtime check on
  // both sides.
  // ------------------------------------------------------------------

  it("followable helper with == in if-throw guard isn't silently dropped", () => {
    // The helper's `if (amount == 0) throw` cannot translate; the
    // helper must NOT be classified as followable, so the call stays
    // in the body where the body translator surfaces the rejection.
    const source = `
      function ensureValid(amount: number): void {
        if (amount == 0) { throw new Error("zero"); }
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        ensureValid(amount);
        account.balance = account.balance + amount;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const result = translateSignature(sourceFile, "deposit", IntStrategy);
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    // No guard inlined from the helper.
    assert.equal(result.declaration.guard, undefined);
    // Body translation must reject (statement preserved + non-guard
    // call rejected) rather than silently dropping the helper call.
    const props = translateBody({
      sourceFile,
      functionName: "deposit",
      strategy: IntStrategy,
    });
    assert.ok(
      props.some((p) => p.kind === "unsupported"),
      `expected an unsupported prop; saw: ${JSON.stringify(props.map((p) => p.kind))}`,
    );
  });

  it("followable helper with == in assertion arg isn't silently dropped", () => {
    const source = `
      function assert(condition: unknown): asserts condition {
        if (!condition) throw new Error();
      }
      function ensureValid(amount: number): void {
        assert(amount == 0);
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        ensureValid(amount);
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
    const props = translateBody({
      sourceFile,
      functionName: "deposit",
      strategy: IntStrategy,
    });
    assert.ok(
      props.some((p) => p.kind === "unsupported"),
      `expected an unsupported prop; saw: ${JSON.stringify(props.map((p) => p.kind))}`,
    );
  });

  // ------------------------------------------------------------------
  // containsUnsupportedOperator — the syntactic walk that aligns the
  // predicate side (classifyGuardIf, isGuardStatement, isFollowableGuardCall)
  // with translateExpr's actual rejection list.
  // ------------------------------------------------------------------

  it("containsUnsupportedOperator detects loose equality at any depth", () => {
    function exprOf(src: string): ts.Expression {
      const sf = createSourceFileFromSource(`const _x = ${src};`);
      const stmt = sf.compilerNode.statements[0] as ts.VariableStatement;
      const decl = stmt.declarationList.declarations[0]!;
      return decl.initializer!;
    }
    assert.equal(containsUnsupportedOperator(exprOf("a == b")), true);
    assert.equal(containsUnsupportedOperator(exprOf("a != b")), true);
    assert.equal(containsUnsupportedOperator(exprOf("(a == b)")), true);
    assert.equal(containsUnsupportedOperator(exprOf("!(a == b)")), true);
    assert.equal(containsUnsupportedOperator(exprOf("a + (b == c)")), true);
    assert.equal(containsUnsupportedOperator(exprOf("f(a == b)")), true);
    assert.equal(
      containsUnsupportedOperator(exprOf("a === b ? a == b : false")),
      true,
    );
    // Negative cases — strict equality and other operators are accepted.
    assert.equal(containsUnsupportedOperator(exprOf("a === b")), false);
    assert.equal(containsUnsupportedOperator(exprOf("a !== b")), false);
    assert.equal(containsUnsupportedOperator(exprOf("a + b")), false);
    assert.equal(containsUnsupportedOperator(exprOf("a.b.c")), false);
  });

  it("followable helper with translatable guards still works", () => {
    // Sanity: the predicate gating only rejects untranslatable guards.
    // A standard helper continues to follow.
    const source = `
      function ensureValid(amount: number): void {
        if (amount <= 0) { throw new Error("nonpositive"); }
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        ensureValid(amount);
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
      "~(amount <= 0)",
    );
  });
});
