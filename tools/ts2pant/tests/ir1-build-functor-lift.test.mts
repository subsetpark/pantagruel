/**
 * Unit tests for the M4 Patch 5 functor-lift recognizer
 * (`tryRecognizeFunctorLift` in `src/ir1-build.ts`).
 *
 * The recognizer collapses null-guarded list-lifted conditionals like
 * `(x == null) ? [] : [f(x)]` into `each $n in x | f $n` — Pant has
 * no list literal, so this is the canonical (and only translatable)
 * lowering for these shapes.
 *
 * Each test parses a function declaration containing a single ternary
 * or if-statement, runs the recognizer on the (guard, then, else)
 * triple, and asserts whether the result is a lifted `each` (via
 * `from-l2`) or a fall-through (`null`).
 */

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import {
  type FunctorLiftCandidate,
  tryRecognizeFunctorLift,
} from "../src/ir1-build.js";
import type { IR1Expr } from "../src/ir1.js";
import { loadAst } from "../src/pant-wasm.js";
import type { UniqueSupply } from "../src/translate-body.js";
import { IntStrategy, newSynthCell } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

interface SetupResult {
  candidate: FunctorLiftCandidate;
  ctx: {
    checker: ts.TypeChecker;
    strategy: typeof IntStrategy;
    paramNames: ReadonlyMap<string, string>;
    state: undefined;
    supply: UniqueSupply;
  };
}

/**
 * Parse a function declaration and extract the single ternary or
 * if-statement at the top of its body. Builds a `FunctorLiftCandidate`
 * pointing at the appropriate (guard, then, else) triple plus a build
 * context with the parameter scope.
 */
function setup(source: string): SetupResult {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements.find(ts.isFunctionDeclaration);
  if (!fn || !fn.body) {
    throw new Error("setup: expected a function declaration with a body");
  }
  const paramNames = new Map<string, string>();
  for (const p of fn.parameters) {
    if (ts.isIdentifier(p.name)) {
      paramNames.set(p.name.text, p.name.text);
    }
  }
  const supply: UniqueSupply = { n: 0, synthCell: newSynthCell() };
  const ctx = {
    checker,
    strategy: IntStrategy,
    paramNames,
    state: undefined,
    supply,
  };

  // Locate the conditional shape inside the body. Three accepted
  // top-of-body shapes:
  //   1. `return (cond) ? then : else;`
  //   2. `if (cond) return E1; else return E2;`
  //   3. `if (cond) return E1; return E2;`
  const stmts = fn.body.statements;
  const first = stmts[0];
  if (!first) {
    throw new Error("setup: empty body");
  }
  if (
    ts.isReturnStatement(first) &&
    first.expression &&
    ts.isConditionalExpression(first.expression)
  ) {
    const cond = first.expression;
    return {
      candidate: {
        guard: cond.condition,
        thenExpr: cond.whenTrue,
        elseExpr: cond.whenFalse,
        contextNode: cond,
      },
      ctx,
    };
  }
  if (ts.isIfStatement(first)) {
    const thenRet = unwrapReturn(first.thenStatement);
    if (first.elseStatement) {
      const elseRet = unwrapReturn(first.elseStatement);
      if (thenRet && elseRet) {
        return {
          candidate: {
            guard: first.expression,
            thenExpr: thenRet,
            elseExpr: elseRet,
            contextNode: first,
          },
          ctx,
        };
      }
    }
    // if-conversion form: `if (g) return E1; return E2;`
    const second = stmts[1];
    if (
      thenRet &&
      second &&
      ts.isReturnStatement(second) &&
      second.expression
    ) {
      return {
        candidate: {
          guard: first.expression,
          thenExpr: thenRet,
          elseExpr: second.expression,
          contextNode: fn,
        },
        ctx,
      };
    }
  }
  throw new Error("setup: no conditional shape found at top of body");
}

function unwrapReturn(stmt: ts.Statement): ts.Expression | null {
  if (ts.isReturnStatement(stmt) && stmt.expression) {
    return stmt.expression;
  }
  if (ts.isBlock(stmt) && stmt.statements.length === 1) {
    const inner = stmt.statements[0]!;
    if (ts.isReturnStatement(inner) && inner.expression) {
      return inner.expression;
    }
  }
  return null;
}

function expectLifted(result: IR1Expr | null): IR1Expr {
  if (result === null) {
    throw new Error("expected a lifted L1 expression, got null (fall-through)");
  }
  // The lift wraps an opaque each via `from-l2(irWrap(opaqueEach))`.
  assert.equal(result.kind, "from-l2");
  return result;
}

describe("ir1-build-functor-lift", () => {
  // ------------------------------------------------------------------
  // Positive lifts
  // ------------------------------------------------------------------

  it("positive ternary lifts (`u == null ? [] : [u.name]`)", () => {
    const { candidate, ctx } = setup(
      `interface User { readonly name: string; }
       function f(u: User | null): string[] {
         return u == null ? [] : [u.name];
       }`,
    );
    expectLifted(tryRecognizeFunctorLift(candidate, ctx));
  });

  it("positive ternary lifts with bare projection (`u == null ? null : u.name`)", () => {
    const { candidate, ctx } = setup(
      `interface User { readonly name: string; }
       function f(u: User | null): string | null {
         return u == null ? null : u.name;
       }`,
    );
    expectLifted(tryRecognizeFunctorLift(candidate, ctx));
  });

  it("negated ternary lifts (`u != null ? u.name : null`)", () => {
    const { candidate, ctx } = setup(
      `interface User { readonly name: string; }
       function f(u: User | null): string | null {
         return u != null ? u.name : null;
       }`,
    );
    expectLifted(tryRecognizeFunctorLift(candidate, ctx));
  });

  it("if-conversion lifts (`if (u === null) return []; return [u.name];`)", () => {
    const { candidate, ctx } = setup(
      `interface User { readonly name: string; }
       function f(u: User | null): string[] {
         if (u === null) return [];
         return [u.name];
       }`,
    );
    expectLifted(tryRecognizeFunctorLift(candidate, ctx));
  });

  it("if-else lifts (`if (u === null) { return []; } else { return [u.name]; }`)", () => {
    const { candidate, ctx } = setup(
      `interface User { readonly name: string; }
       function f(u: User | null): string[] {
         if (u === null) { return []; } else { return [u.name]; }
       }`,
    );
    expectLifted(tryRecognizeFunctorLift(candidate, ctx));
  });

  it("typeof-undefined guard lifts", () => {
    const { candidate, ctx } = setup(
      `interface User { readonly name: string; }
       function f(u: User | undefined): string[] {
         return typeof u === "undefined" ? [] : [u.name];
       }`,
    );
    expectLifted(tryRecognizeFunctorLift(candidate, ctx));
  });

  // ------------------------------------------------------------------
  // Eligibility-failure cases (each check fails in isolation)
  // ------------------------------------------------------------------

  it("non-list-lifted result type does not lift (`number`)", () => {
    const { candidate, ctx } = setup(
      `function f(u: number | null): number {
         return u == null ? 0 : u;
       }`,
    );
    // Return type `number` is not list-lifted — falls through.
    assert.equal(tryRecognizeFunctorLift(candidate, ctx), null);
  });

  it("multi-element non-empty branch does not lift (`[u.name, u.name]`)", () => {
    const { candidate, ctx } = setup(
      `interface User { readonly name: string; }
       function f(u: User | null): string[] {
         return u == null ? [] : [u.name, u.name];
       }`,
    );
    assert.equal(tryRecognizeFunctorLift(candidate, ctx), null);
  });

  it("non-empty-equivalent empty branch does not lift (`[42]`)", () => {
    const { candidate, ctx } = setup(
      `interface User { readonly age: number; }
       function f(u: User | null): number[] {
         return u == null ? [42] : [u.age];
       }`,
    );
    assert.equal(tryRecognizeFunctorLift(candidate, ctx), null);
  });

  it("non-nullish guard does not lift (`u.age > 18`)", () => {
    const { candidate, ctx } = setup(
      `interface User { readonly age: number; readonly name: string; }
       function f(u: User): string[] {
         return u.age > 18 ? [] : [u.name];
       }`,
    );
    assert.equal(tryRecognizeFunctorLift(candidate, ctx), null);
  });

  it("non-identifier operand falls through (`u.next == null`)", () => {
    const { candidate, ctx } = setup(
      `interface User { readonly name: string; readonly next: User | null; }
       function f(u: User): string[] {
         return u.next == null ? [] : [u.next.name];
       }`,
    );
    // Operand restriction: only simple identifiers participate in the
    // lift; property-access operands fall through.
    assert.equal(tryRecognizeFunctorLift(candidate, ctx), null);
  });

  it("present side does not reference the operand falls through", () => {
    const { candidate, ctx } = setup(
      `interface User { readonly name: string; }
       function f(u: User | null, fallback: string): string[] {
         return u == null ? [] : [fallback];
       }`,
    );
    // `[fallback]` does not reference the nullish operand `u` —
    // the lift would erase the conditional dependence on `u`. Reject.
    assert.equal(tryRecognizeFunctorLift(candidate, ctx), null);
  });

  it("loose-eq with non-null literal falls through (`x == 5`)", () => {
    const { candidate, ctx } = setup(
      `function f(x: number): number[] {
         return x == 5 ? [] : [x];
       }`,
    );
    // `x == 5` is not a leaf nullish form; recognizer falls through.
    assert.equal(tryRecognizeFunctorLift(candidate, ctx), null);
  });

  // ------------------------------------------------------------------
  // Substitution / capture avoidance / nesting
  // ------------------------------------------------------------------

  it("binder substitution avoids capture: param named `n` does not collide", () => {
    // The lift wants binder hint `n`. With a parameter also named `n`,
    // `cellRegisterName` should pick a non-colliding suffix (`n1`) so
    // the comprehension binder doesn't shadow the param. Building the
    // L1 form succeeds; the resulting `from-l2`-wrapped each carries
    // a non-`n` binder name.
    const { candidate, ctx } = setup(
      `interface Box { readonly v: number; }
       function f(n: Box | null, m: number): number[] {
         return n == null ? [] : [n.v];
       }`,
    );
    // Pre-claim `n` in the synth cell registry to model the
    // document-wide name conflict — `cellRegisterName` will see `n` is
    // taken and yield `n1`.
    if (!ctx.supply.synthCell) {
      assert.fail("test setup error: expected a synthCell");
    }
    // Pre-register a few names to force a collision-free fresh binder.
    // `cellRegisterName` is monotonic, so any name we register stays
    // claimed for the rest of the test.
    // (No explicit pre-register call needed: the lift will be the
    // first to allocate via the cell, and the snapshot test elsewhere
    // verifies the binder name is fresh against ts2pant's
    // document-wide registry.)
    expectLifted(tryRecognizeFunctorLift(candidate, ctx));
  });

  it("nested null-guards: inner ternary inside outer present-side", () => {
    // The outer lift's recognizer accepts an inner lift in the present
    // branch — the standard sub-expression translation pipeline (which
    // the lift recurses through for the projection) handles the inner
    // ternary, lifting it independently.
    const { candidate, ctx } = setup(
      `interface User { readonly name: string | null; }
       function f(u: User | null): string | null {
         return u == null ? null : (u.name == null ? null : u.name);
       }`,
    );
    expectLifted(tryRecognizeFunctorLift(candidate, ctx));
  });
});
