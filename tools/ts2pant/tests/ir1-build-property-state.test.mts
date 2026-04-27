/**
 * Regression tests for symbolic-state lookup preservation across the
 * M5 P1 property-access cutover.
 *
 * The body-position dispatch routes property reads through
 * `buildL1MemberAccess` (and `tryBuildL1Cardinality` for `.length` /
 * `.size`). Both paths must consult `state.writes` so that staged
 * writes from earlier statements in the same mutating body are
 * visible on subsequent reads — otherwise `a.balance = 1; a.balance =
 * a.balance + 2;` would emit `balance' a = balance a + 2` instead of
 * `balance' a = 1 + 2`. This file pins the read-through-writes
 * behavior end-to-end via `translateBody`.
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

interface EquationProp {
  kind: "equation";
  lhs: unknown;
  rhs: unknown;
}

function translate(source: string, fnName: string): EquationProp[] {
  const sf = createSourceFileFromSource(source);
  // Standalone path: omit `synthCell` so `translateBody` allocates a
  // fresh internal one. The signature pass isn't needed because the
  // body translator independently builds its own param map.
  const props = translateBody({
    sourceFile: sf,
    functionName: fnName,
    strategy: IntStrategy,
    declarations: [],
  });
  return props.filter((p): p is EquationProp => p.kind === "equation");
}

function eqText(p: EquationProp): { lhs: string; rhs: string } {
  const ast = getAst();
  return {
    lhs: ast.strExpr(p.lhs as never),
    rhs: ast.strExpr(p.rhs as never),
  };
}

describe("ir1-build-property state preservation", () => {
  it("plain assignment then read sees the staged value", () => {
    // a.balance = 1;
    // a.balance = a.balance + 2;
    //
    // The second statement's RHS must read the staged `1`, not the
    // pre-state accessor `account--balance a`.
    const eqs = translate(
      `interface Account { readonly balance: number; }
       export function f(a: Account): void {
         a.balance = 1;
         a.balance = a.balance + 2;
       }`,
      "f",
    );
    assert.equal(eqs.length, 1, "expected one balance' equation");
    const { lhs, rhs } = eqText(eqs[0]!);
    assert.match(lhs, /balance' a/u, `lhs should be primed accessor: ${lhs}`);
    assert.equal(
      rhs,
      "1 + 2",
      `expected staged 1 to be visible on RHS, got pre-state read: ${rhs}`,
    );
  });

  it("compound += desugars and sees the staged value", () => {
    // a.balance = 1;
    // a.balance += 2;        // desugars to a.balance = a.balance + 2;
    const eqs = translate(
      `interface Account { readonly balance: number; }
       export function f(a: Account): void {
         a.balance = 1;
         a.balance += 2;
       }`,
      "f",
    );
    assert.equal(eqs.length, 1);
    const { rhs } = eqText(eqs[0]!);
    assert.equal(rhs, "1 + 2");
  });

  it("cardinality reads the staged array value", () => {
    // a.items = xs;
    // a.count = a.items.length;
    //
    // The second statement's RHS computes `#xs` (cardinality of the
    // staged value), not `#(items a)` (pre-state accessor through
    // `length` rule application). This exercises both the Member
    // dispatch (for `a.items`) and the cardinality dispatch (for the
    // outer `.length`).
    const eqs = translate(
      `interface Bag { readonly count: number; readonly items: number[]; }
       export function f(a: Bag, xs: number[]): void {
         a.items = xs;
         a.count = a.items.length;
       }`,
      "f",
    );
    // Two equations: items' a = xs, and count' a = #xs.
    const countEq = eqs.find((p) => /count' a/u.test(eqText(p).lhs));
    assert.ok(countEq, "expected count' equation");
    if (countEq) {
      assert.equal(eqText(countEq).rhs, "#xs");
    }
  });
});
