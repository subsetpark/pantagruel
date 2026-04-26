/**
 * Layer 1 Imperative Intermediate Representation for ts2pant.
 *
 * This is the IRSC-faithful imperative IR layer that sits between the
 * TypeScript AST and ts2pant's existing Layer 2 IR (`IRExpr`/`IRStmt` in
 * `ir.ts`). Layer 1 preserves TS's actual control-flow shape; normalization
 * passes against L1 collapse operationally-equivalent TS surface forms
 * (increment spellings, conditional families, iteration families) into a
 * small canonical vocabulary. Lowering passes target ONE canonical input
 * shape per construct.
 *
 * Reference: Vekris, Cosman, Jhala, *Refinement Types for TypeScript*
 * (PLDI 2016, arxiv:1604.02480). Their FRSC→IRSC SSA transformation is
 * the precedent. ts2pant's L1 is the FRSC analog (TS-shape preserved with
 * canonicalized expressions); L2 (`ir.ts`) is the IRSC analog (Pant-shape
 * pure expressions).
 *
 * **Vocabulary lock at M1.** The forms declared here are the locked Layer
 * 1 vocabulary. Forms can be *added* in later milestones (e.g.,
 * `IsNullish` primitive at M4) but the existing forms cannot be changed.
 * Forms not yet active (`foreach`, `for`, `throw`, `expr-stmt`,
 * statement-position `cond-stmt`) are declared at the type level; their
 * constructors throw `not-implemented` until the milestone that
 * introduces them lands. M1 activated `block`, `let`, `return`, and
 * expression-position `cond`; M2 adds `assign` and `while`.
 *
 * **No `IR1Wrap` form.** Layer 1 is *not* an escape-hatch layer — the
 * build pass either produces L1 or rejects with `unsupported`. An L1
 * wrap-anything form would re-introduce the cross-talk problem the IR is
 * meant to retire. The `from-l2` form is *not* a wrap-anything escape
 * hatch — it is a scoped delegation point for sub-expressions whose
 * normalization isn't this milestone's concern (e.g., guard expressions
 * inside an L1 conditional during M1, when expression normalization
 * itself is M2/M3 territory). By M3 the form shrinks; by M6 (legacy
 * cleanup) it is deleted.
 *
 * **Migration status**: see `workstreams/ts2pant-imperative-ir.md` and
 * `tools/ts2pant/CLAUDE.md` §"Imperative IR Workstream".
 */

import type { IRBinop, IRExpr, IRLiteral, IRUnop } from "./ir.js";

// --------------------------------------------------------------------------
// Literals, binops, unops
// --------------------------------------------------------------------------
//
// L1 reuses the L2 atom types directly — literal/binop/unop operator names
// have one source of truth across both layers. Lowering for `lit` is then
// a structural pass-through (same shape on both sides); binop/unop still
// need to wrap into L2's App-headed form during lowering.

export type IR1Literal = IRLiteral;
export type IR1Binop = IRBinop;
export type IR1Unop = IRUnop;

// --------------------------------------------------------------------------
// Expression forms (value-position)
// --------------------------------------------------------------------------

/**
 * Layer 1 expressions. Seven forms preserve TS-shape canonicalization:
 *
 * - `var`, `lit` — literal references
 * - `binop`, `unop` — arithmetic/logical/comparison operators
 * - `app` — function or method application (callee is itself an IR1Expr)
 * - `member` — property access; canonicalized so `obj.f` and `obj["f"]`
 *   both build as `Member(obj, "f")` (the build-time canonicalization will
 *   land in M5, but the form is locked here)
 * - `cond` — multi-arm value-position conditional (M1 canonical form for
 *   if-with-returns, ternary chains, switch w/o fall-through, &&/||
 *   when Bool-typed)
 *
 * Plus one transitional form:
 *
 * - `from-l2` — scoped delegation to an already-built Layer 2 IRExpr.
 *   See module doc for lifetime; this is *not* an IRWrap-style escape
 *   hatch.
 */
export type IR1Expr =
  /** Variable / parameter reference. `primed = true` means next-state. */
  | { kind: "var"; name: string; primed?: boolean }
  /** Literal value (nat, bool, string). */
  | { kind: "lit"; value: IR1Literal }
  /** Binary operator application. */
  | { kind: "binop"; op: IR1Binop; lhs: IR1Expr; rhs: IR1Expr }
  /** Unary operator application. */
  | { kind: "unop"; op: IR1Unop; arg: IR1Expr }
  /** Function or method application. Callee is itself an L1 expression. */
  | { kind: "app"; callee: IR1Expr; args: IR1Expr[] }
  /**
   * Property access. Canonicalized form for `obj.f` and `obj["f"]`.
   * Lowering qualifies `name` to a rule (today's `qualifyFieldAccess`).
   */
  | { kind: "member"; receiver: IR1Expr; name: string }
  /**
   * Multi-armed value-position conditional. Canonical form for if-with-
   * returns, ternary chains, switch w/o fall-through, &&/|| when Bool-
   * typed. `arms` is non-empty; `otherwise` is required at the
   * expression level (else-branch is always present in a value-position
   * conditional — there's no value if guards all fail).
   */
  | {
      kind: "cond";
      arms: ReadonlyArray<readonly [IR1Expr, IR1Expr]>;
      otherwise: IR1Expr;
    }
  /**
   * Transitional delegation to a pre-built Layer 2 IRExpr. Used for
   * sub-expressions outside the current milestone's normalization
   * concern (e.g., M1 conditional guards/values whose internal
   * structure is M2/M3 territory).
   *
   * Lowering: emits the wrapped L2 IRExpr verbatim.
   *
   * **Lifetime**: shrinks at M3 (more sub-expressions reach L1
   * natively); deleted at M6 (legacy cleanup). Do not introduce new
   * uses outside the build pipeline's scoped sub-expression delegation.
   */
  | { kind: "from-l2"; expr: IRExpr };

// --------------------------------------------------------------------------
// Statement forms (effect-position)
// --------------------------------------------------------------------------

/**
 * Layer 1 statements. Ten forms cover the imperative TS shape needed
 * across the workstream. Active in M1+M2: `block`, `let`, `return`,
 * `cond` (expression-position via `IR1Expr`), `assign`, `while`. The
 * remaining forms are vocabulary-locked but their constructors throw
 * until the milestone that introduces them lands:
 *
 * - `foreach`, `for` — M3 (iteration normalization)
 * - `throw` — M3 (iteration body for guard-throw assertions)
 * - `expr-stmt` — M3 (effect-bearing expressions in statement position)
 * - statement-position `cond-stmt` — M3 (conditional with statement body,
 *   e.g., `if (g) obj.p = v`)
 */
export type IR1Stmt =
  /** Block of statements. Non-empty; single-statement blocks collapse. */
  | { kind: "block"; stmts: readonly [IR1Stmt, ...IR1Stmt[]] }
  /** Hygienic let-binding. Inlined at lowering (Pant has no `let`). */
  | { kind: "let"; name: string; value: IR1Expr }
  /**
   * Assignment. Canonical form for all increment/compound-assign/explicit
   * mutation forms. Target is an L1 expression (typically `var` or
   * `member`). M2 actively uses this for the μ-search counter step.
   */
  | { kind: "assign"; target: IR1Expr; value: IR1Expr }
  /**
   * Multi-armed statement-position conditional. Canonical form for
   * `if (g) {body}` and conditional mutation. Introduced in M3 (when
   * iteration + mutation land); constructor throws until then.
   */
  | {
      kind: "cond-stmt";
      arms: readonly [
        readonly [IR1Expr, IR1Stmt],
        ...ReadonlyArray<readonly [IR1Expr, IR1Stmt]>,
      ];
      otherwise: IR1Stmt | null;
    }
  /** Uniform iteration. Introduced in M3. */
  | {
      kind: "foreach";
      binder: string;
      source: IR1Expr;
      body: IR1Stmt;
    }
  /**
   * Generic counter-for; fallback when Foreach can't apply. Introduced
   * in M3.
   */
  | {
      kind: "for";
      init: IR1Stmt | null;
      cond: IR1Expr | null;
      step: IR1Stmt | null;
      body: IR1Stmt;
    }
  /**
   * Bounded while loop. M2 actively uses this to represent the
   * μ-search let/while/increment shape; general bounded-while lowering
   * (accumulator iteration, fixed-point search) lands in M3.
   */
  | { kind: "while"; cond: IR1Expr; body: IR1Stmt }
  /** Return statement. Optional expression for void-returning bodies. */
  | { kind: "return"; expr: IR1Expr | null }
  /**
   * Throw statement. Used as guard pattern (if-throw assertions).
   * Introduced in M3.
   */
  | { kind: "throw"; expr: IR1Expr }
  /**
   * Bare expression-as-statement (effect-bearing call, etc.). Introduced
   * in M3.
   */
  | { kind: "expr-stmt"; expr: IR1Expr };

// --------------------------------------------------------------------------
// Type guards
// --------------------------------------------------------------------------

export const isIR1FromL2 = (
  e: IR1Expr,
): e is Extract<IR1Expr, { kind: "from-l2" }> => e.kind === "from-l2";

// --------------------------------------------------------------------------
// Expression constructors
// --------------------------------------------------------------------------

export const ir1Var = (name: string, primed = false): IR1Expr => ({
  kind: "var",
  name,
  primed,
});

export const ir1LitNat = (value: number): IR1Expr => ({
  kind: "lit",
  value: { kind: "nat", value },
});

export const ir1LitBool = (value: boolean): IR1Expr => ({
  kind: "lit",
  value: { kind: "bool", value },
});

export const ir1LitString = (value: string): IR1Expr => ({
  kind: "lit",
  value: { kind: "string", value },
});

export const ir1Binop = (
  op: IR1Binop,
  lhs: IR1Expr,
  rhs: IR1Expr,
): IR1Expr => ({ kind: "binop", op, lhs, rhs });

export const ir1Unop = (op: IR1Unop, arg: IR1Expr): IR1Expr => ({
  kind: "unop",
  op,
  arg,
});

export const ir1App = (callee: IR1Expr, args: IR1Expr[]): IR1Expr => ({
  kind: "app",
  callee,
  args,
});

export const ir1Member = (receiver: IR1Expr, name: string): IR1Expr => ({
  kind: "member",
  receiver,
  name,
});

/**
 * Multi-armed value-position conditional. `arms` must be non-empty;
 * `otherwise` is required (value position has no "no-value" exit).
 *
 * Type-level invariants:
 * - Non-empty arms via tuple-rest pattern in the parameter type
 * - `otherwise` cannot be `null` (vs the statement-position cond)
 *
 * Single-arm with otherwise = `cond([(g, v)], otherwise)`. Multi-arm
 * `Cond([(g1, v1), (g2, v2)], default)` is the canonical form for
 * if-chains, ternary chains, switch (cases as arms), and Bool-typed
 * `&&`/`||`.
 */
export const ir1Cond = (
  arms: readonly [
    readonly [IR1Expr, IR1Expr],
    ...ReadonlyArray<readonly [IR1Expr, IR1Expr]>,
  ],
  otherwise: IR1Expr,
): IR1Expr => ({ kind: "cond", arms, otherwise });

/**
 * Transitional delegation to a pre-built Layer 2 IRExpr. Use only inside
 * `ir1-build.ts` for sub-expressions whose normalization is not the
 * current milestone's concern, and in `translatePureBody`'s arm-cond
 * assembly where `inlineConstBindings` produces pre-translated
 * OpaqueExprs. See module doc for lifetime — shrinks at M3, deleted at
 * M6.
 *
 * @deprecated transitional; do not introduce new call sites outside the
 * scoped sub-expression delegation in `ir1-build.ts` /
 * `translate-body.ts:translatePureBody`. Lifetime is bounded by the
 * workstream — see `workstreams/ts2pant-imperative-ir.md`.
 */
export const ir1FromL2 = (expr: IRExpr): IR1Expr => ({
  kind: "from-l2",
  expr,
});

// --------------------------------------------------------------------------
// Statement constructors
// --------------------------------------------------------------------------

/**
 * Block of statements. Non-empty; single-statement blocks collapse to
 * the inner statement at construction time so consumers don't need to
 * distinguish `Block([s])` from `s`.
 */
export const ir1Block = (stmts: readonly [IR1Stmt, ...IR1Stmt[]]): IR1Stmt => {
  if (stmts.length === 1) {
    return stmts[0];
  }
  return { kind: "block", stmts };
};

export const ir1Let = (name: string, value: IR1Expr): IR1Stmt => ({
  kind: "let",
  name,
  value,
});

export const ir1Return = (expr: IR1Expr | null): IR1Stmt => ({
  kind: "return",
  expr,
});

// --------------------------------------------------------------------------
// Statement constructors for forms beyond the M1 active set.
// All forms are now active as of M3 Patch 4; lowering for the iteration
// kinds (foreach / for / throw / while-outside-μ-search) lands in Patch 5.
// --------------------------------------------------------------------------

/**
 * Assignment / read-modify-write. Canonical form for all increment and
 * compound-assignment surface spellings. M2 activates this form for the
 * μ-search counter step; broader mutation patterns land in M3.
 *
 * `target` is typically `Var(name)` (a counter) or `Member(receiver,
 * name)` (a property write); `value` is the new value, which for
 * increment forms is `BinOp(<op>, target, <k>)`. The L1 → L2 lowering
 * for `Assign` is *not* generic — only `recognizeAndLowerMuSearch` in
 * `ir1-lower.ts` introspects the surrounding L1 shape and produces L2
 * output for it. A standalone `Assign` reaching `lowerL1Stmt` rejects.
 */
export const ir1Assign = (target: IR1Expr, value: IR1Expr): IR1Stmt => ({
  kind: "assign",
  target,
  value,
});

export const ir1CondStmt = (
  arms: readonly [
    readonly [IR1Expr, IR1Stmt],
    ...ReadonlyArray<readonly [IR1Expr, IR1Stmt]>,
  ],
  otherwise: IR1Stmt | null,
): IR1Stmt => ({ kind: "cond-stmt", arms, otherwise });

export const ir1Foreach = (
  binder: string,
  source: IR1Expr,
  body: IR1Stmt,
): IR1Stmt => ({ kind: "foreach", binder, source, body });

export const ir1For = (
  init: IR1Stmt | null,
  cond: IR1Expr | null,
  step: IR1Stmt | null,
  body: IR1Stmt,
): IR1Stmt => ({ kind: "for", init, cond, step, body });

/**
 * Bounded while loop. M2 activates this form to faithfully represent
 * the let/while/increment shape of a μ-search-shaped TS body —
 * `recognizeAndLowerMuSearch` in `ir1-lower.ts` introspects the
 * `Block([Let, While(_, Assign)])` shape and lowers to `Comb(min, Each)`.
 * General while-loop lowering (bounded fixed-point, accumulator
 * iteration) lands in M3; until then a standalone `While` reaching
 * `lowerL1Stmt` rejects.
 */
export const ir1While = (cond: IR1Expr, body: IR1Stmt): IR1Stmt => ({
  kind: "while",
  cond,
  body,
});

export const ir1Throw = (expr: IR1Expr): IR1Stmt => ({ kind: "throw", expr });

export const ir1ExprStmt = (expr: IR1Expr): IR1Stmt => ({
  kind: "expr-stmt",
  expr,
});
