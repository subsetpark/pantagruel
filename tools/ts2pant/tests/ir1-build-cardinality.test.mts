/**
 * Unit tests for the M5 Patch 1 cardinality dispatch
 * (`tryBuildL1Cardinality` in `src/ir1-build.ts`).
 *
 * Cardinality is a deliberate non-Member dispatch: Pant's primitive for
 * list cardinality is `#x` (`Unop(card, x)`), not a `length` / `size`
 * rule application. Routing through Member would emit `length arr` —
 * an EUF rule application distinct from `#arr`. The build pass tries
 * cardinality recognition before Member dispatch for the six
 * list-shaped TS types.
 *
 * Coverage list: `Array.length`, `ReadonlyArray.length`, `Set.size`,
 * `ReadonlySet.size`, `Map.size`, `ReadonlyMap.size`. User-typed
 * `.length` on a non-list interface and optional-chain `.length`
 * fall through to Member.
 */

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import {
  type L1BuildContext,
  tryBuildL1Cardinality,
  tryBuildL1PureSubExpression,
} from "../src/ir1-build.js";
import type { IR1Expr } from "../src/ir1.js";
import { loadAst } from "../src/pant-wasm.js";
import {
  type UniqueSupply,
} from "../src/translate-body.js";
import {
  cellRegisterName,
  IntStrategy,
  newSynthCell,
  toPantTermName,
} from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

interface AccessSetup {
  node: ts.Expression;
  ctx: L1BuildContext;
}

/**
 * Parse a function declaration whose body is a single
 * `return EXPR;` where `EXPR` is the property access we want to
 * exercise. Builds the production-style param scope.
 */
function setup(source: string): AccessSetup {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements.find(ts.isFunctionDeclaration);
  if (!fn || !fn.body) {
    throw new Error("setup: expected function declaration with a body");
  }
  const synthCell = newSynthCell();
  const paramNames = new Map<string, string>();
  for (const p of fn.parameters) {
    if (ts.isIdentifier(p.name)) {
      paramNames.set(
        p.name.text,
        cellRegisterName(synthCell, toPantTermName(p.name.text)),
      );
    }
  }
  const supply: UniqueSupply = { n: 0, synthCell };
  const ctx: L1BuildContext = {
    checker,
    strategy: IntStrategy,
    paramNames,
    state: undefined,
    supply,
  };
  const stmt = fn.body.statements[0];
  if (!stmt || !ts.isReturnStatement(stmt) || !stmt.expression) {
    throw new Error("setup: expected return statement");
  }
  if (
    !ts.isPropertyAccessExpression(stmt.expression) &&
    !ts.isElementAccessExpression(stmt.expression)
  ) {
    throw new Error(
      `setup: expected property access, got ${ts.SyntaxKind[stmt.expression.kind]}`,
    );
  }
  return { node: stmt.expression, ctx };
}

function expectCardUnop(result: IR1Expr | null): void {
  assert.notEqual(result, null, "expected Unop(card, _), got null");
  if (result === null) return;
  assert.equal(result.kind, "unop");
  if (result.kind === "unop") {
    assert.equal(result.op, "card");
  }
}

describe("ir1-build-cardinality", () => {
  it("Array.length builds Unop(card)", () => {
    const { node, ctx } = setup(
      `function f(xs: number[]): number { return xs.length; }`,
    );
    expectCardUnop(tryBuildL1Cardinality(node, ctx));
  });

  it("ReadonlyArray.length builds Unop(card)", () => {
    const { node, ctx } = setup(
      `function f(xs: ReadonlyArray<number>): number { return xs.length; }`,
    );
    expectCardUnop(tryBuildL1Cardinality(node, ctx));
  });

  it("Set.size builds Unop(card)", () => {
    const { node, ctx } = setup(
      `function f(xs: Set<number>): number { return xs.size; }`,
    );
    expectCardUnop(tryBuildL1Cardinality(node, ctx));
  });

  it("ReadonlySet.size builds Unop(card)", () => {
    const { node, ctx } = setup(
      `function f(xs: ReadonlySet<number>): number { return xs.size; }`,
    );
    expectCardUnop(tryBuildL1Cardinality(node, ctx));
  });

  it("Map.size builds Unop(card)", () => {
    const { node, ctx } = setup(
      `function f(m: Map<string, number>): number { return m.size; }`,
    );
    expectCardUnop(tryBuildL1Cardinality(node, ctx));
  });

  it("ReadonlyMap.size builds Unop(card)", () => {
    const { node, ctx } = setup(
      `function f(m: ReadonlyMap<string, number>): number { return m.size; }`,
    );
    expectCardUnop(tryBuildL1Cardinality(node, ctx));
  });

  it('string-literal "length" and "size" build Unop(card)', () => {
    const length = setup(
      `function f(xs: number[]): number { return xs["length"]; }`,
    );
    const size = setup(
      `function f(xs: Set<number>): number { return xs["size"]; }`,
    );
    expectCardUnop(tryBuildL1PureSubExpression(length.node, length.ctx));
    expectCardUnop(tryBuildL1PureSubExpression(size.node, size.ctx));
  });

  it("user-typed .length on a non-list interface falls through to Member", () => {
    // `User.length` is a regular field, not array cardinality.
    // `tryBuildL1Cardinality` must return null here so the Member
    // dispatch handles it. A regression that matched on field name
    // alone would mis-emit `#u` for a structural field read.
    const { node, ctx } = setup(
      `interface User { readonly length: number; }
       function f(u: User): number { return u.length; }`,
    );
    assert.equal(tryBuildL1Cardinality(node, ctx), null);
  });

  it("optional-chain .length is not short-circuited (preserves ?. semantics)", () => {
    // `xs?.length` short-circuits to undefined when xs is null;
    // collapsing to `#xs` would silently change semantics. The
    // recognizer must reject so the optional-chain handler in
    // `translateBodyExpr` builds the correct functor-lift.
    const { node, ctx } = setup(
      `function f(xs: number[] | null): number | undefined { return xs?.length; }`,
    );
    assert.equal(tryBuildL1Cardinality(node, ctx), null);
  });
});
