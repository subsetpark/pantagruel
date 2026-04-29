/**
 * Unit tests for the M5 Patch 1 universal L1-layering paren-stripping
 * invariant. The principle: `(e)` and `e` build to identical L1 trees
 * for every L1 form, so downstream recognizers don't re-implement the
 * strip themselves.
 *
 * Each test runs a paren-wrapped expression and an unwrapped reference
 * through the L1 build pipeline (or its surface-level entry point) and
 * asserts the lowered OpaqueExpr texts match. We compare lowered texts
 * rather than IR shape because the lowered text is the observable
 * byte-for-byte canonical form, immune to internal IR shape drift.
 *
 * Type-erasure wrappers (`as T`, `!`, `<T>x`, `satisfies`) are NOT
 * paren-equivalent — they change the TS-checker type at the AST node
 * and are intentionally left to the standard build path. This file
 * does not test paren-stripping THROUGH those wrappers.
 */

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { lowerExpr } from "../src/ir-emit.js";
import {
  buildL1Conditional,
  isL1Unsupported,
  lowerL1ToOpaque,
} from "../src/ir1-build.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
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

interface CondSetup {
  expr: ts.ConditionalExpression;
  ctx: {
    checker: ts.TypeChecker;
    strategy: typeof IntStrategy;
    paramNames: ReadonlyMap<string, string>;
    state: undefined;
    supply: UniqueSupply;
  };
}

/**
 * Parse a function declaration and extract its single
 * `ConditionalExpression` from `return (cond) ? a : b;`. Builds the
 * production-style param-name allocation through `synthCell` so the
 * test exercises the real Pant-name scope.
 */
function setupCond(source: string): CondSetup {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements.find(ts.isFunctionDeclaration);
  if (!fn || !fn.body) {
    throw new Error("setupCond: expected function declaration with a body");
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
  const ctx = {
    checker,
    strategy: IntStrategy,
    paramNames,
    state: undefined,
    supply,
  };
  const stmt = fn.body.statements[0];
  if (!stmt || !ts.isReturnStatement(stmt) || !stmt.expression) {
    throw new Error("setupCond: expected return statement with expression");
  }
  let expr: ts.Expression = stmt.expression;
  while (ts.isParenthesizedExpression(expr)) {
    // For tests that wrap the entire ternary in parens, descend so the
    // returned candidate is the inner ConditionalExpression. The test
    // then re-wraps and compares the build outputs at a known level.
    expr = expr.expression;
  }
  if (!ts.isConditionalExpression(expr)) {
    throw new Error(
      `setupCond: expected ConditionalExpression at body root, got ${ts.SyntaxKind[expr.kind]}`,
    );
  }
  return { expr, ctx };
}

/** Lower an L1 build result and stringify the OpaqueExpr. */
function lowerToText(
  result: ReturnType<typeof buildL1Conditional>,
): string {
  if (isL1Unsupported(result)) {
    throw new Error(
      `expected L1 expression, got unsupported: ${result.unsupported}`,
    );
  }
  return getAst().strExpr(lowerL1ToOpaque(result));
}

describe("ir1-build-parens", () => {
  it("parenthesized binop strips to identical L1 binop", () => {
    // `(x + 1) > 0` and `x + 1 > 0` build to identical L1 conds when
    // each appears in a value-position ternary guard. The conditional
    // dispatcher unwraps the guard via `buildSubExpr`, which strips
    // parens at the L1 build entry. A regression that didn't strip
    // would route the paren-wrapped binop differently — observable
    // as a divergent lowered text.
    const bare = setupCond(
      `function f(x: number): number {
         return x + 1 > 0 ? 1 : 0;
       }`,
    );
    const paren = setupCond(
      `function f(x: number): number {
         return (x + 1) > 0 ? 1 : 0;
       }`,
    );
    assert.equal(
      lowerToText(buildL1Conditional(paren.expr, paren.ctx)),
      lowerToText(buildL1Conditional(bare.expr, bare.ctx)),
    );
  });

  it("nested parens around var build identical L1 var", () => {
    // `((x))` and `x` are operationally indistinguishable; both reach
    // the same Var lowering through the cond dispatcher's sub-expr
    // path. Use a ternary with a Bool param so the value-position
    // arm receives the (parenthesized or bare) var directly.
    const bare = setupCond(
      `function f(x: number, b: boolean): number {
         return b ? x : 0;
       }`,
    );
    const paren = setupCond(
      `function f(x: number, b: boolean): number {
         return b ? ((x)) : 0;
       }`,
    );
    assert.equal(
      lowerToText(buildL1Conditional(paren.expr, paren.ctx)),
      lowerToText(buildL1Conditional(bare.expr, bare.ctx)),
    );
  });

  it("parenthesized conditional strips to identical L1 cond", () => {
    // The conditional dispatcher itself strips parens at entry, so
    // `(b ? a : c)` and `b ? a : c` produce identical lowered output
    // when handed to `buildL1Conditional`. Constructing an outer
    // ternary that wraps the inner gives both forms a comparable
    // structural anchor.
    const bare = setupCond(
      `function f(b: boolean, c: boolean, x: number, y: number): number {
         return b ? (c ? x : y) : 0;
       }`,
    );
    const paren = setupCond(
      `function f(b: boolean, c: boolean, x: number, y: number): number {
         return b ? ((c ? x : y)) : 0;
       }`,
    );
    assert.equal(
      lowerToText(buildL1Conditional(paren.expr, paren.ctx)),
      lowerToText(buildL1Conditional(bare.expr, bare.ctx)),
    );
  });
});
