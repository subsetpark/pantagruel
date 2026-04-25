/**
 * Intermediate Representation for ts2pant.
 *
 * This is a Pant-adapted variant of IRSC from Vekris, Cosman, Jhala
 * "Refinement Types for TypeScript" (PLDI 2016, arxiv:1604.02480). The IR
 * sits between the TypeScript AST and the Pantagruel emitter; surface-syntax
 * simplifiers (early-return if-conversion, optional chaining, nullish
 * coalescing, chain fusion, μ-search, etc.) land as TS→IR rewrites in
 * `ir-build.ts`, and the emitter in `ir-emit.ts` reads only IR forms.
 *
 * Two divergences from IRSC, both deliberate (see CLAUDE.md §IR):
 *
 * 1. **No FieldAccess form.** ts2pant lowers `e.f` to `App(qualified-rule,
 *    [e])` at construction time via `qualifyFieldAccess`. Preserving that
 *    invariant prevents a class of cross-talk bugs where consumers must
 *    check both shapes.
 *
 * 2. **Hybrid SSA scope.** IRSC uses SSA over program names for *all*
 *    assignments. We use ordinary `Let` (no φ) for const-bindings and
 *    `LetIf` only for branching mutation, with φ-vars as **write-keys**
 *    (rule-name + canonicalized receiver), not program names. Three
 *    reasons: the existing right-fold substitution closure is already a
 *    debugged let-elimination algorithm; Pant has no `let` in the output,
 *    so SSA-construct + SSA-destruct would double the substitution
 *    machinery; the mutating path's existing φ-merge is already keyed by
 *    write-keys.
 *
 * The IR has two layers: `IRExpr` (pure, value-position) and `IRStmt`
 * (effect, statement-position). IRSC merges these via `u⟨e⟩` hole
 * contexts; we keep them separate because Pantagruel's mutating output is
 * a list of equations + frame conditions, not a unit-returning expression.
 *
 * **Migration status**: see CLAUDE.md §IR "IR Migration Status".
 */

import type { OpaqueExpr } from "./pant-ast.js";

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
   * Multi-armed conditional (value position). Last arm canonically
   * `(true, default)`. Mirrors Pant's `cond` constructor and the
   * existing `ast.cond([[g, v], [true, d]])` invariant.
   */
  | { kind: "cond"; arms: Array<[IRExpr, IRExpr]> }
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
    }
  /**
   * **Migration-only escape hatch.** Carries an already-translated
   * `OpaqueExpr` so old-pipeline outputs are valid IR by construction.
   * Removed at the Stage 8 pure-path cutover.
   */
  | { kind: "ir-wrap"; expr: OpaqueExpr };

/**
 * `IRExpr` constrained to be a comprehension. Used as the
 * operand of `Comb` so we statically forbid `Comb(_, _, IRWrap(...))`
 * etc. — only an actual `each` form makes sense as a combiner argument.
 */
export type IRExprEach = Extract<IRExpr, { kind: "each" }>;

// --------------------------------------------------------------------------
// Statement forms (effect, statement-position)
// --------------------------------------------------------------------------

/**
 * Write target — what a `Write` modifies. A descriptor (not an expression)
 * because the Pant lowering for each kind is structurally different:
 *
 * - `property-field`: emits one primed equation `prop' obj = value`.
 * - `map-entry`: emits one tuple-keyed override on the value rule + one on
 *   the membership predicate (the "delete drops value override at same
 *   key" coupling lives in the lowering, not the IR).
 * - `set-member`: emits one element-keyed override on the membership rule;
 *   `op = "clear"` resets and uses `false` fallthrough.
 *
 * Stage 9 introduces this; Stage 1 only declares the type.
 */
export type IRWriteTarget =
  | {
      kind: "property-field";
      ruleName: string;
      objExpr: IRExpr;
    }
  | {
      kind: "map-entry";
      ruleName: string;
      keyPredName: string;
      ownerType: string;
      keyType: string;
      objExpr: IRExpr;
      keyExpr: IRExpr;
      op: "set" | "delete";
    }
  | {
      kind: "set-member";
      ruleName: string;
      ownerType: string;
      elemType: string;
      objExpr: IRExpr;
      elemExpr: IRExpr | null;
      op: "add" | "delete" | "clear";
    };

/**
 * Effect / statement-position IR. Four forms total. `Write` is statement-
 * only — never an `IRExpr` — because making it value-level reintroduces
 * the effect/value discriminant hazard at every traversal.
 */
export type IRStmt =
  /** Write to a target with a value (or null for `clear`-like ops). */
  | { kind: "write"; target: IRWriteTarget; value: IRExpr | null }
  /**
   * Branching mutation merge. **`phiVars` are write-keys**, not program
   * variable names — see ir.ts §"Hybrid SSA scope" rationale. The tuple
   * type enforces non-empty φ-vars at the type level, mirroring the
   * `LetIf` invariant from CLAUDE.md §IR — a `let-if` with no φ-vars
   * is structurally a value-position `Cond`, and the discrimination
   * between the two must hold statically.
   * Stage 9 introduces this; Stage 1 only declares the type.
   */
  | {
      kind: "let-if";
      phiVars: [string, ...string[]];
      cond: IRExpr;
      then: IRStmt[];
      else: IRStmt[];
      continuation: IRStmt[];
    }
  /** Statement sequencing. */
  | { kind: "seq"; stmts: IRStmt[] }
  /**
   * Universally-quantified Bool assertion exit form. Carries the
   * empty-Set / empty-Map initializer emissions that aren't equations.
   * Mirrors `PropResult`'s `kind: "assertion"`.
   */
  | {
      kind: "assert";
      quantifiers: Array<{ name: string; type: string }>;
      body: IRExpr;
    };

// --------------------------------------------------------------------------
// Body output
// --------------------------------------------------------------------------

/**
 * The result of translating a function body to IR. Not a single `IRExpr`
 * because surface bodies produce multiple propositions (record returns
 * decompose into one equation per field; mutating bodies produce
 * equations + frame conditions; empty-Set initializers produce
 * assertions).
 */
export interface IRBody {
  /**
   * Pure-path equations. For non-record returns, exactly one entry;
   * for record returns, one per declared return-type field; for
   * mutating bodies, one per modified rule.
   */
  equations: IREquation[];
  /** Universally-quantified assertions (empty-Set / empty-Map fields). */
  assertions: IRAssertExit[];
  /**
   * Mutating-path identity equations for unmodified rules. Stage 10
   * populates this; before then it's empty.
   */
  frames: IREquation[];
}

/**
 * One equation. `quantifiers` is empty for non-quantified equations;
 * non-empty for the `all m, k | R' m k = ...` shape from Map/Set
 * mutation.
 */
export interface IREquation {
  quantifiers: Array<{ name: string; type: string }>;
  guards?: IRExpr[];
  lhs: IRExpr;
  rhs: IRExpr;
}

export interface IRAssertExit {
  quantifiers: Array<{ name: string; type: string }>;
  guards?: IRExpr[];
  body: IRExpr;
}

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

export const irCond = (arms: Array<[IRExpr, IRExpr]>): IRExpr => ({
  kind: "cond",
  arms,
});

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

export const irWrap = (expr: OpaqueExpr): IRExpr => ({ kind: "ir-wrap", expr });

// IRStmt constructors. Stage 9 introduces the IRStmt layer; the helpers
// here mirror each variant's shape so callers don't drift into hand-built
// objects when statement-position forms start landing.

export const irStmtWrite = (
  target: IRWriteTarget,
  value: IRExpr | null,
): IRStmt => ({ kind: "write", target, value });

export const irStmtLetIf = (
  phiVars: [string, ...string[]],
  cond: IRExpr,
  thenBranch: IRStmt[],
  elseBranch: IRStmt[],
  continuation: IRStmt[],
): IRStmt => ({
  kind: "let-if",
  phiVars,
  cond,
  // biome-ignore lint/suspicious/noThenProperty: IRStmt ADT shape uses `then`/`else` for branching mutation merge; matches IRSC discipline.
  then: thenBranch,
  else: elseBranch,
  continuation,
});

export const irStmtSeq = (stmts: IRStmt[]): IRStmt => ({ kind: "seq", stmts });

export const irStmtAssert = (
  quantifiers: Array<{ name: string; type: string }>,
  body: IRExpr,
): IRStmt => ({ kind: "assert", quantifiers, body });

// Type guards (small, useful for migration code)

export const isIRWrap = (
  e: IRExpr,
): e is Extract<IRExpr, { kind: "ir-wrap" }> => e.kind === "ir-wrap";

/**
 * Narrow a `Comb(...)` to its foldable variant. TypeScript does not
 * auto-narrow on a secondary discriminator across a same-kind union, so
 * callers that need to access `init` must route through this guard.
 */
export const isFoldComb = (
  e: Extract<IRExpr, { kind: "comb" }>,
): e is Extract<IRExpr, { kind: "comb"; combiner: IRFoldCombiner }> =>
  e.combiner !== "min" && e.combiner !== "max";
