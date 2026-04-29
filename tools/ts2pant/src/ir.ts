/**
 * Layer 2 Expression IR for ts2pant.
 *
 * This is a Pant-adapted variant of IRSC from Vekris, Cosman, Jhala
 * "Refinement Types for TypeScript" (PLDI 2016, arxiv:1604.02480). The IR
 * sits between Layer 1 (`ir1.ts`) and Pantagruel's `OpaqueExpr` emitter
 * (`ir-emit.ts`).
 *
 * **Expression-only.** Post-M3, L2 has no statement vocabulary —
 * mutating-body lowering bypasses L2 entirely (`ir1-lower-body.ts`
 * threads `SymbolicState` from `translate-body.ts` directly into
 * `PropResult[]`). The single-rooted `IRExpr` tree fits value-position
 * lowering (one `OpaqueExpr` out); a fold over the symbolic state fits
 * effect-position lowering (a list of equations + frame conditions
 * out). The two paths share L1 but diverge here. See
 * `workstreams/ts2pant-imperative-ir.md` § "Architectural Lessons".
 *
 * One deliberate divergence from IRSC (see CLAUDE.md §IR):
 *
 * - **No FieldAccess form.** ts2pant lowers `e.f` to `App(qualified-rule,
 *   [e])` at construction time via `qualifyFieldAccess`. Preserving that
 *   invariant prevents a class of cross-talk bugs where consumers must
 *   check both shapes.
 */

// --------------------------------------------------------------------------
// Literals
// --------------------------------------------------------------------------

/**
 * Literal values appearing in `Lit(...)`. Mirrors Pant's lit constructors:
 * `litNat` for non-negative integers, `litBool`, `litString`. Negative
 * integers and reals are emitted as `Unop(Neg, Lit(...))` /
 * `App(...)` rather than as native literal forms because Pant has no
 * dedicated negative-literal or real-literal constructor at the AST level.
 */
export type IRLiteral =
  | { kind: "nat"; value: number }
  | { kind: "bool"; value: boolean }
  | { kind: "string"; value: string };

// --------------------------------------------------------------------------
// Application heads
// --------------------------------------------------------------------------

/**
 * The function/operator at the head of an `App`. Four shapes:
 *
 * - `name` — a plain rule, function, or parameter reference. Lowers to
 *   `ast.app(ast.var(name), [...])`. Use `primed = true` for next-state
 *   references in mutating bodies (lowers to `ast.primed(name)`).
 *
 * - `binop` / `unop` — Pant's built-in operators. Lower to `ast.binop(op,
 *   l, r)` / `ast.unop(op, e)` directly. Centralising op identity here
 *   means a Pant op rename is one switch case in `ir-emit.ts`.
 *
 * - `expr` — an arbitrary expression head. Mirrors Pant's
 *   `ast.app(fn: OpaqueExpr, args: OpaqueExpr[])` signature, where `fn`
 *   can be any expression (e.g. list-indexing `(x 1)` is application
 *   of list `x` to argument `1`, with `x` being any expression — not
 *   necessarily a named identifier).
 */
export type IRHead =
  | { kind: "name"; name: string; primed?: boolean }
  | { kind: "binop"; op: IRBinop }
  | { kind: "unop"; op: IRUnop }
  | { kind: "expr"; expr: IRExpr };

export type IRBinop =
  | "and"
  | "or"
  | "impl"
  | "iff"
  | "eq"
  | "neq"
  | "lt"
  | "gt"
  | "le"
  | "ge"
  | "in"
  | "subset"
  | "add"
  | "sub"
  | "mul"
  | "div";

export type IRUnop = "not" | "neg" | "card";

/**
 * Combiners with a corresponding Pant binop, suitable for non-identity
 * `init` folding. `Comb(add, 5, ...)` lowers to `5 + (+ over each ...)`,
 * etc. min/max have no binop equivalent and so cannot carry `init`.
 */
export type IRFoldCombiner = "add" | "mul" | "and" | "or";

/**
 * Combiner for `Comb(...)`. Mirrors Pant's `combAdd`/`combMul`/`combAnd`/
 * `combOr`/`combMin`/`combMax`.
 */
export type IRCombiner = IRFoldCombiner | "min" | "max";

// --------------------------------------------------------------------------
// Expression forms (pure, value-position)
// --------------------------------------------------------------------------

/**
 * Pure value-position IR. Ten forms total. Each form's invariants are
 * documented inline; do not add forms ad-hoc — additions go through
 * CLAUDE.md §IR review.
 */
export type IRExpr =
  /** Variable / parameter reference. `primed = true` means next-state. */
  | { kind: "var"; name: string; primed?: boolean }
  /** Literal value (nat, bool, string). */
  | { kind: "lit"; value: IRLiteral }
  /**
   * Function or operator application. `head` selects the lowering
   * (named ref, binop, unop). Method calls lower with the receiver as
   * `args[0]`. Qualified field accessors (`e.f` → `App(name="qualified",
   * [e])`) lower the same way.
   */
  | { kind: "app"; head: IRHead; args: IRExpr[] }
  /**
   * Multi-armed conditional (value position). `otherwise` carries the
   * default branch when the source layer has one; lowering appends it as
   * Pant's canonical trailing `(true, default)` arm.
   */
  | { kind: "cond"; arms: Array<[IRExpr, IRExpr]>; otherwise?: IRExpr }
  /**
   * Hygienic let-binding. Substituted out at emission (Pant has no
   * `let`). Names come from the document-wide `UniqueSupply` /
   * `cellRegisterName` so they don't collide with parameters or
   * accessor rules.
   */
  | { kind: "let"; name: string; value: IRExpr; body: IRExpr }
  /**
   * List comprehension. `binder` ranges over `src`; `guards` filter;
   * `proj` is the projected expression. Lowers to `ast.each(...)`.
   */
  | {
      kind: "each";
      binder: string;
      binderType?: string;
      src: IRExpr;
      guards: IRExpr[];
      proj: IRExpr;
    }
  /**
   * Aggregate over a comprehension with a foldable combiner (add/mul/
   * and/or). `init` is optional with identity-elision (e.g. `0` for
   * `add`, `1` for `mul`) handled at lowering time, not by callers.
   * Lowers to `ast.eachComb(...)`, with `init` folded in via
   * `ast.binop` if non-identity.
   */
  | {
      kind: "comb";
      combiner: IRFoldCombiner;
      init?: IRExpr;
      each: IRExprEach;
    }
  /**
   * Aggregate over a comprehension with min/max. No `init` because Pant
   * has no `min`/`max` binop to fold a seed through; the type forbids
   * it so the lowering can't crash on a malformed node.
   */
  | { kind: "comb"; combiner: "min" | "max"; each: IRExprEach }
  /**
   * Aggregate over a *typed* comprehension with no source — `min over
   * each j: T, g₁, …, gₙ | proj`. Distinct from `Comb(_, Each)` because
   * `Each` requires a `src` (collection to iterate); `comb-typed`
   * iterates over a primitive type. The canonical lowering target for
   * μ-search (`min over each j: Int, j >= INIT, ¬P(j) | j`).
   *
   * Lowers to `ast.eachComb([param(binder, type)], guards, comb, proj)`.
   *
   * No `init` for the same reason as min/max `Comb` — Pant has no
   * fold operator to seed.
   */
  | {
      kind: "comb-typed";
      combiner: "min" | "max";
      binder: string;
      binderType: string;
      guards: IRExpr[];
      proj: IRExpr;
    }
  /**
   * Universal quantifier. Lowers to `ast.forall(...)`.
   */
  | {
      kind: "forall";
      binder: string;
      binderType: string;
      guard?: IRExpr;
      body: IRExpr;
    }
  /**
   * Existential quantifier. Lowers to `ast.exists(...)`.
   */
  | {
      kind: "exists";
      binder: string;
      binderType: string;
      guard?: IRExpr;
      body: IRExpr;
    };

/**
 * `IRExpr` constrained to be a comprehension. Used as the
 * operand of `Comb` so we statically forbid `Comb(_, _, …)` arguments
 * other than `each` — only an actual `each` form makes sense as a
 * combiner argument.
 */
export type IRExprEach = Extract<IRExpr, { kind: "each" }>;

// --------------------------------------------------------------------------
// Statement forms (effect, statement-position)
// --------------------------------------------------------------------------
// Constructors (terse helpers, all type-checked)
// --------------------------------------------------------------------------

export const irVar = (name: string, primed = false): IRExpr => ({
  kind: "var",
  name,
  primed,
});

/**
 * **Precondition**: `value` must be a non-negative safe integer.
 * Negative integers and reals lower as `Unop(neg, ...)` / `App(...)` —
 * see `IRLiteral`. Callers are responsible for the check; the constructor
 * does not validate at runtime, in keeping with the codebase's
 * trust-internal-code / validate-at-boundaries convention.
 * Caller sites today (all in `ir-build.ts`): the numeric-literal branch
 * guards `Number.isFinite(n) && Number.isInteger(n) && n >= 0`; the `??`
 * lowering uses literal `0` and `1`.
 */
export const irLitNat = (value: number): IRExpr => ({
  kind: "lit",
  value: { kind: "nat", value },
});

export const irLitBool = (value: boolean): IRExpr => ({
  kind: "lit",
  value: { kind: "bool", value },
});

export const irLitString = (value: string): IRExpr => ({
  kind: "lit",
  value: { kind: "string", value },
});

export const irApp = (head: IRHead, args: IRExpr[]): IRExpr => ({
  kind: "app",
  head,
  args,
});

export const irAppName = (name: string, args: IRExpr[]): IRExpr => ({
  kind: "app",
  head: { kind: "name", name },
  args,
});

export const irAppPrimed = (name: string, args: IRExpr[]): IRExpr => ({
  kind: "app",
  head: { kind: "name", name, primed: true },
  args,
});

/**
 * Application with an arbitrary expression head. Used for list-indexing
 * (`(x 1)` is `App(expr=x, [Lit(1)])`) and other cases where the
 * applied "function" is itself an expression rather than a named ref.
 */
export const irAppExpr = (head: IRExpr, args: IRExpr[]): IRExpr => ({
  kind: "app",
  head: { kind: "expr", expr: head },
  args,
});

export const irBinop = (op: IRBinop, lhs: IRExpr, rhs: IRExpr): IRExpr => ({
  kind: "app",
  head: { kind: "binop", op },
  args: [lhs, rhs],
});

export const irUnop = (op: IRUnop, arg: IRExpr): IRExpr => ({
  kind: "app",
  head: { kind: "unop", op },
  args: [arg],
});

export const irCond = (
  arms: Array<[IRExpr, IRExpr]>,
  otherwise?: IRExpr,
): IRExpr =>
  otherwise === undefined
    ? { kind: "cond", arms }
    : { kind: "cond", arms, otherwise };

export const irLet = (name: string, value: IRExpr, body: IRExpr): IRExpr => ({
  kind: "let",
  name,
  value,
  body,
});

export const irEach = (
  binder: string,
  src: IRExpr,
  guards: IRExpr[],
  proj: IRExpr,
  binderType?: string,
): IRExpr =>
  binderType !== undefined
    ? { kind: "each", binder, binderType, src, guards, proj }
    : { kind: "each", binder, src, guards, proj };

export function irComb(
  combiner: IRFoldCombiner,
  each: IRExprEach,
  init?: IRExpr,
): IRExpr;
export function irComb(combiner: "min" | "max", each: IRExprEach): IRExpr;
export function irComb(
  combiner: IRCombiner,
  each: IRExprEach,
  init?: IRExpr,
): IRExpr {
  if (combiner === "min" || combiner === "max") {
    return { kind: "comb", combiner, each };
  }
  return init !== undefined
    ? { kind: "comb", combiner, init, each }
    : { kind: "comb", combiner, each };
}

/**
 * Aggregate over a typed comprehension with no source. The canonical
 * lowering target for μ-search. See the `comb-typed` variant in
 * `IRExpr` for the semantics.
 */
export const irCombTyped = (
  combiner: "min" | "max",
  binder: string,
  binderType: string,
  guards: IRExpr[],
  proj: IRExpr,
): IRExpr => ({
  kind: "comb-typed",
  combiner,
  binder,
  binderType,
  guards,
  proj,
});

export const irForall = (
  binder: string,
  binderType: string,
  body: IRExpr,
  guard?: IRExpr,
): IRExpr =>
  guard !== undefined
    ? { kind: "forall", binder, binderType, guard, body }
    : { kind: "forall", binder, binderType, body };

export const irExists = (
  binder: string,
  binderType: string,
  body: IRExpr,
  guard?: IRExpr,
): IRExpr =>
  guard !== undefined
    ? { kind: "exists", binder, binderType, guard, body }
    : { kind: "exists", binder, binderType, body };

/**
 * Narrow a `Comb(...)` to its foldable variant. TypeScript does not
 * auto-narrow on a secondary discriminator across a same-kind union, so
 * callers that need to access `init` must route through this guard.
 */
export const isFoldComb = (
  e: Extract<IRExpr, { kind: "comb" }>,
): e is Extract<IRExpr, { kind: "comb"; combiner: IRFoldCombiner }> =>
  e.combiner !== "min" && e.combiner !== "max";
