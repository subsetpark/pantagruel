/**
 * Unit tests for the M4 Patch 5 functor-lift recognizer
 * (`tryRecognizeFunctorLift` in `src/ir1-build.ts`).
 *
 * The recognizer collapses null-guarded list-lifted conditionals like
 * `(x == null) ? [] : [f(x)]` into `each n in x | f n` — Pant has
 * no list literal, so this is the canonical (and only translatable)
 * lowering for these shapes. (The binder is plain `n` / `n1` /
 * etc., not the internal `$N` hygienic class — comprehension binders
 * must round-trip through Pant's parser, which rejects `$`.)
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
  lowerL1ToOpaque,
  tryRecognizeFunctorLift,
} from "../src/ir1-build.js";
import type { IR1Expr } from "../src/ir1.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import type { UniqueSupply } from "../src/translate-body.js";
import {
  cellRegisterName,
  IntStrategy,
  newSynthCell,
  toPantTermName,
} from "../src/translate-types.js";

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
  // Build paramNames the same way production does: each TS parameter
  // name is sanitized through `toPantTermName` and registered against
  // the document-wide NameRegistry via `cellRegisterName`. Keeping the
  // test's allocation path in lockstep with `translateSignature`'s
  // `paramNameMap` (translate-signature.ts:1241–1244) means the test
  // exercises the real substitution path — `maybeUser` parameter
  // canonicalizes to `maybe-user` in the lowered Pant, so the lift
  // must look up the same Pant name when targeting `substituteBinder`.
  const synthCell = newSynthCell();
  const paramNames = new Map<string, string>();
  for (const p of fn.parameters) {
    if (ts.isIdentifier(p.name)) {
      const pantName = cellRegisterName(synthCell, toPantTermName(p.name.text));
      paramNames.set(p.name.text, pantName);
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
  // Verify the kind discriminator AND the lowered shape — a regression
  // that returned a different wrapped L2 form (e.g., a `cond`) would
  // still have `kind === "from-l2"`, so lower the expression and
  // assert the emitted text contains an `each` comprehension.
  assert.equal(result.kind, "from-l2");
  const out = getAst().strExpr(lowerL1ToOpaque(result));
  assert.match(
    out,
    /\beach\s+\S+\s+in\b/,
    `expected functor-lift lowering to emit an each-comprehension, got: ${out}`,
  );
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

  it("lifts through transparent wrappers (`as`, `!`, `satisfies`)", () => {
    // Each of `as` / `!` / `satisfies` is runtime-transparent and must
    // not block recognition. A guard wrapped in `as boolean`, an
    // operand widened with `as`, or a present-side projection asserted
    // with `!` or `satisfies` should still recognize.
    const { candidate, ctx } = setup(
      `interface User { readonly name: string; }
       function f(u: User | null): string[] {
         return ((u as User | null) == null) as boolean
           ? ([] satisfies string[])
           : [u!.name];
       }`,
    );
    expectLifted(tryRecognizeFunctorLift(candidate, ctx));
  });

  // ------------------------------------------------------------------
  // Eligibility-failure cases (each check fails in isolation)
  // ------------------------------------------------------------------

  it("non-list-lifted result type does not lift (`number`)", () => {
    // Use `null as number` for the empty branch so check (b)
    // (`isEmptyEquivalent`) passes: the `null` keyword survives the
    // transparent unwrap. Without the cast, `0` is not empty-equivalent
    // and (b) would reject before (d) is consulted, masking a regression
    // in `isListLiftedAtNode`. With this isolation, (a)/(b)/(c) all pass
    // and only the result-type gate decides — function returns `number`,
    // which is neither array-typed nor nullable-union, so (d) fails and
    // the lift returns null.
    const { candidate, ctx } = setup(
      `function f(u: number | null): number {
         return u == null ? (null as number) : u;
       }`,
    );
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

  it("shadowed `undefined` empty branch falls through", () => {
    // `undefined` is not a reserved word — a parameter named
    // `undefined` is a legal binding that shadows the global. If
    // `isEmptyEquivalent` matched the identifier text alone, the
    // lift would silently rewrite the conditional to a comprehension,
    // dropping the shadowed value. To make the empty-branch check
    // (b) the deciding gate, the other three eligibility checks must
    // all pass:
    //   (a) guard `u == null` is a leaf nullish form on `u`
    //   (c) present branch `[u]` (after singleton unwrap) is `u`,
    //       which references the operand
    //   (d) the conditional's static result type is list-lifted —
    //       both branches type as `Box[]` (the shadowed `undefined`
    //       parameter has type `Box[]`), so the conditional itself
    //       is `Box[]` and `isListLiftedTsType` accepts it
    // Pre-fix this test would have green-lit the lift on the text
    // match alone; post-fix the checker resolves the `undefined`
    // identifier to `Box[]` (no Undefined flag) and (b) rejects.
    const { candidate, ctx } = setup(
      `interface Box { readonly v: number; }
       function f(u: Box | null, undefined: Box[]): Box[] {
         return u == null ? undefined : [u];
       }`,
    );
    assert.equal(tryRecognizeFunctorLift(candidate, ctx), null);
  });


  // ------------------------------------------------------------------
  // Substitution / capture avoidance / nesting
  // ------------------------------------------------------------------

  it("binder substitution avoids capture: param named `n` does not collide", () => {
    // The lift wants binder hint `n`. With a parameter also named `n`,
    // the production-style `setup()` already registers `n` against the
    // shared `synthCell` (mirroring `translateSignature`'s parameter
    // allocation). When the lift then calls `cellRegisterName(synthCell,
    // "n")` for the comprehension binder, the registry must emit a
    // suffixed alternative (`n1`) so the binder doesn't shadow the
    // param.
    //
    // Inspect the lowered output to confirm the binder name actually
    // rotated — proving the lift succeeded isn't sufficient because a
    // collision regression could still produce a working `each` whose
    // binder happens to alias the param at the Pant level (visible
    // only if you read the output string).
    const { candidate, ctx } = setup(
      `interface Box { readonly v: number; }
       function f(n: Box | null, m: number): number[] {
         return n == null ? [] : [n.v];
       }`,
    );
    if (!ctx.supply.synthCell) {
      throw new Error("test setup error: expected a synthCell");
    }
    const lifted = expectLifted(tryRecognizeFunctorLift(candidate, ctx));
    const out = getAst().strExpr(lowerL1ToOpaque(lifted));
    // Match `each <binder> in ...`, extract the binder name.
    const m = out.match(/\beach\s+(\S+)\s+in\b/);
    if (!m) {
      throw new Error(`could not parse binder from output: ${out}`);
    }
    const binder = m[1] as string;
    const paramPantName = ctx.paramNames.get("n");
    if (paramPantName === undefined) {
      throw new Error("test setup error: paramNames missing `n`");
    }
    assert.notEqual(
      binder,
      paramPantName,
      `binder ${binder} collides with param ${paramPantName} in: ${out}`,
    );
  });

  it("camelCase parameter substitution doesn't kebab-mangle the binder target", () => {
    // The operand's substitution name must match the spelling that
    // `translateBodyExpr` emits for the identifier — for parameters,
    // that's `paramNames.get(text)` (sanitized at signature translation
    // via `toPantTermName` + `cellRegisterName`); for bare references
    // it's the raw text. `setup()` mirrors production's allocation,
    // so `maybeUser` is registered as `maybe-user` in `paramNames`.
    // The lowered projection references `maybe-user`, and the lift's
    // substitution target must too — both paths look up
    // `paramNames.get(operandName)`, so any divergence between body
    // and lift lookup logic (e.g., body uses raw text but lift
    // sanitizes) would leave the projection's reference unsubstituted.
    //
    // Verify by counting occurrences of the sanitized operand name
    // `maybe-user` in the lowered output: exactly 1 (the iter source).
    // If substitution failed, the projection's reference would
    // persist, giving 2.
    const { candidate, ctx } = setup(
      `interface User { readonly name: string; }
       function f(maybeUser: User | null): string[] {
         return maybeUser == null ? [] : [maybeUser.name];
       }`,
    );
    const lifted = expectLifted(tryRecognizeFunctorLift(candidate, ctx));
    const ast = getAst();
    const out = ast.strExpr(lowerL1ToOpaque(lifted));
    const sanitizedName = ctx.paramNames.get("maybeUser");
    if (sanitizedName === undefined) {
      throw new Error("test setup error: paramNames missing maybeUser");
    }
    assert.equal(
      sanitizedName,
      "maybe-user",
      `expected setup to sanitize maybeUser → maybe-user, got ${sanitizedName}`,
    );
    // Token-bounded match instead of `out.split(sanitizedName)` so a
    // hypothetical longer token like `maybe-user-suffix` doesn't double-
    // count `maybe-user`. The negative-lookbehind/lookahead asserts the
    // surrounding chars aren't valid Pant identifier continuations
    // (alphanumerics, `_`, `-`, plus the trailing-`?`/`!` shapes Pant
    // accepts in identifiers).
    const escaped = sanitizedName.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    const tokenRx = new RegExp(
      `(?<![A-Za-z0-9?!_-])${escaped}(?![A-Za-z0-9?!_-])`,
      "g",
    );
    const occurrences = (out.match(tokenRx) ?? []).length;
    assert.equal(
      occurrences,
      1,
      `expected exactly one occurrence of '${sanitizedName}' (as iter source), got ${occurrences} in: ${out}`,
    );
  });

  it("nested null-guards: inner ternary inside outer present-side", () => {
    // The outer lift's recognizer accepts an inner lift in the present
    // branch — the standard sub-expression translation pipeline (which
    // the lift recurses through for the projection) handles the inner
    // ternary, lifting it independently.
    //
    // Both inner and outer need a simple-identifier operand: the outer
    // is on `u`, the inner is on `v` (passed as an argument to a call
    // that also references `u`, so the outer's present-side check (c)
    // is satisfied via `u.combine(...)`). The inner's empty branch is
    // `null`, present branch is `v.label` referencing the operand —
    // all four eligibility checks fire on the inner and it lifts too.
    //
    // Verify by counting `each` occurrences in the lowered output:
    // exactly 2 (one per lift). A regression that broke recursive
    // recognition would produce 1 (outer-only) and the assertion
    // would fail.
    const { candidate, ctx } = setup(
      `interface User {
         combine(arg: string | null): string;
       }
       interface Box { readonly label: string; }
       function f(u: User | null, v: Box | null): string | null {
         return u == null ? null : u.combine(v == null ? null : v.label);
       }`,
    );
    const lifted = expectLifted(tryRecognizeFunctorLift(candidate, ctx));
    const out = getAst().strExpr(lowerL1ToOpaque(lifted));
    const matches = out.match(/\beach\s+\S+\s+in\b/g) ?? [];
    assert.equal(
      matches.length,
      2,
      `expected both outer and inner null-guards to lower as lifts, got: ${out}`,
    );
  });
});
