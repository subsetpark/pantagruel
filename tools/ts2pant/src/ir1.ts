/**
 * Layer 1 Imperative Intermediate Representation for ts2pant.
 *
 * This is the IRSC-faithful imperative IR layer that sits between the
 * TypeScript AST and ts2pant's expression IR (`IRExpr` in `ir.ts`,
 * Layer 2). Layer 1 preserves TS's actual control-flow shape; normalization
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
 * Layer 1 is *not* an escape-hatch layer — the build pass either
 * produces L1 or rejects with `unsupported`. Forms can be added in
 * later milestones; the `is-nullish` primitive landed at M4 and the
 * `each` comprehension form landed alongside the functor-lift native
 * construction.
 *
 * **Migration status**: see `workstreams/ts2pant-imperative-ir.md` and
 * `tools/ts2pant/AGENTS.md` §"Imperative IR Workstream".
 */

import type { IRBinop, IRLiteral, IRUnop } from "./ir.js";

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
 * Layer 1 expressions. Fourteen forms preserve TS-shape canonicalization:
 *
 * - `var`, `lit` — literal references
 * - `binop`, `unop` — arithmetic/logical/comparison operators
 * - `app` — function or method application (callee is itself an IR1Expr)
 * - `member` — property access; canonicalized so `obj.f` and `obj["f"]`
 *   both build as `Member(obj, "f")`
 * - `cond` — multi-arm value-position conditional (M1 canonical form for
 *   if-with-returns, ternary chains, switch w/o fall-through, &&/||
 *   when Bool-typed)
 * - `is-nullish` — canonical Bool null/undefined test (M4 canonical form
 *   for `x == null`, `x === null`, `x === undefined`, the long
 *   `||`-form, and `typeof x === "undefined"`). Lowers to the
 *   cardinality-zero shape `#x = 0` under list-lift.
 * - `each` — list comprehension. Canonical form for the functor-lift
 *   recognizer's `each n in operand | projection` output. Mirrors L2
 *   `each` and lowers structurally.
 * - `comb-typed`, `forall`, `exists` — Pant-target binder forms.
 *   Mirror L2's typed comprehension / quantifier vocabulary and lower
 *   structurally.
 * - `map-read`, `set-read` — state-aware Map/Set reads (`.get`/`.has`
 *   on a Map, `.has` on a Set). Symmetric to the write-side
 *   `map-effect` / `set-effect` statement forms; mutating bodies resolve
 *   these reads through IR1 SSA so prior writes in the same path are
 *   observed. Pure / read-only callers lower these forms to the bare
 *   `App` / `Binop(in, ...)` shapes.
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
   * Canonical Bool test for null/undefined. Lowers mechanically to the
   * cardinality-zero shape `#x = 0`, which is the same shape `??` and
   * `?.` already use under the list-lift encoding (`T | null` → `[T]`).
   * `operand` is an arbitrary L1 expression — receiver chains
   * (`obj.prop`) and other non-`Var` shapes are accepted; the build pass
   * is responsible for verifying operand-identity in long-form
   * recognition (`x === null || x === undefined`).
   */
  | { kind: "is-nullish"; operand: IR1Expr }
  /**
   * List comprehension. Canonical L1 form for the functor-lift
   * recognizer's `each n in operand | projection`. Mirrors L2 `each`
   * one-to-one and lowers structurally in `lowerL1Expr`. M6 introduced
   * this form so the lift could build natively at L1 instead of
   * round-tripping through OpaqueExpr.
   */
  | {
      kind: "each";
      binder: string;
      src: IR1Expr;
      guards: IR1Expr[];
      proj: IR1Expr;
    }
  /**
   * Aggregate over a typed comprehension with no source. Mirrors L2
   * `comb-typed` one-to-one and lowers structurally in `lowerL1Expr`.
   */
  | {
      kind: "comb-typed";
      combiner: "min" | "max";
      binder: string;
      binderType: string;
      guards: IR1Expr[];
      proj: IR1Expr;
    }
  /**
   * Universal quantifier. Mirrors L2 `forall` one-to-one.
   */
  | {
      kind: "forall";
      binder: string;
      binderType: string;
      guard?: IR1Expr;
      body: IR1Expr;
    }
  /**
   * Existential quantifier. Mirrors L2 `exists` one-to-one.
   */
  | {
      kind: "exists";
      binder: string;
      binderType: string;
      guard?: IR1Expr;
      body: IR1Expr;
    }
  /**
   * State-aware Map read: `m.get(k)` (`op = "get"`) or `m.has(k)`
   * (`op = "has"`). Symmetric to the write-side `map-effect` form —
   * carries the Stage A / Stage B descriptor (`ruleName`,
   * `keyPredName`, `ownerType`, `keyType`) so the body lower path can
   * resolve it through IR1 SSA and observe prior writes inside the same
   * path. The pure / read-only lower path (`lowerL1Expr` in
   * `ir1-lower.ts`) emits the bare `App(rule, [receiver, key])` form.
   *
   * Built only when `ctx.state !== undefined` in
   * `tryBuildL1PureSubExpression`; pure callers keep emitting the bare
   * `App` form because there is no state to thread.
   */
  | {
      kind: "map-read";
      op: "get" | "has";
      ruleName: string;
      keyPredName: string;
      ownerType: string;
      keyType: string;
      receiver: IR1Expr;
      key: IR1Expr;
    }
  /**
   * State-aware Set read: `s.has(x)`. Symmetric to the write-side
   * `set-effect` form; mutating bodies resolve it through IR1 SSA so
   * prior `.add` / `.delete` / `.clear` in the same path are observed.
   * Pure lower emits the bare
   * `Binop(in, elem, App(rule, [receiver]))` form.
   */
  | {
      kind: "set-read";
      ruleName: string;
      ownerType: string;
      elemType: string;
      receiver: IR1Expr;
      elem: IR1Expr;
    };

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
/**
 * One Shape B accumulator-fold leaf. Carried inside `foreach.foldLeaves`.
 *
 * Models `a.p OP= f(x)` (or `if (g(x)) a.p OP= f(x)`) inside an
 * iteration body. The lower side emits one equation per leaf:
 *
 *     prop' target = prop target  OP  (combOP over each x in src[, g(x)] | rhs)
 *
 * `target` is the accumulator's receiver expression (e.g., `account`),
 * pre-translated against the outer state. `prop` is the qualified
 * rule name. `combiner` is the inner aggregate combiner (add/mul/and/
 * or). `outerOp` is the binop combining the accumulator's prior value
 * with the comprehension result. `rhs` is the per-iter contribution
 * (translated against the iter scope at build time). `guard`, if
 * present, folds into the comprehension's guard list.
 */
export interface IR1FoldLeaf {
  target: IR1Expr;
  prop: string;
  combiner: "add" | "mul" | "and" | "or";
  outerOp: IRBinop;
  rhs: IR1Expr;
  guard: IR1Expr | null;
}

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
  /**
   * Uniform iteration. Introduced in M3. Carries:
   * - `body`: Shape A statements (per-iter property writes targeting
   *   the iter binder), processed via a sub-state and emitted as
   *   quantified equations. `null` when the body is pure Shape B
   *   (accumulator-fold only) — there's no per-iteration mutation
   *   to emit. Typed as `IR1ForeachBody` (subset of `IR1Stmt`) so
   *   `return` / `throw` / nested loops cannot appear at the IR
   *   construction site — see `IR1ForeachBody` below.
   * - `foldLeaves`: Shape B accumulator-fold contributions (`a.p OP=
   *   f(x)`), emitted as single equations with a comb-aggregate RHS.
   *   Empty for pure-Shape-A bodies. Each leaf may carry an optional
   *   guard from `if (g(x)) a.p OP= f(x)`.
   */
  | {
      kind: "foreach";
      binder: string;
      source: IR1Expr;
      body: IR1ForeachBody | null;
      foldLeaves: IR1FoldLeaf[];
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
  /** Unlabelled loop break. Labeled forms reject in the TS build pass. */
  | { kind: "break" }
  /** Unlabelled loop continue. Labeled forms reject in the TS build pass. */
  | { kind: "continue" }
  /**
   * Throw statement. Used as guard pattern (if-throw assertions).
   * Introduced in M3.
   */
  | { kind: "throw"; expr: IR1Expr }
  /**
   * Bare expression-as-statement (effect-bearing call, etc.). Introduced
   * in M3.
   */
  | { kind: "expr-stmt"; expr: IR1Expr }
  /**
   * Map mutation effect: `m.set(k, v)` or `m.delete(k)`. Carries the
   * descriptor extracted by the build pass via `translateCallExpr`; the
   * mutating-body lower path turns it into IR1 SSA collection writes.
   *
   * Structural equivalent of `translate-body.ts:MapMutation` — kept
   * here so `ir1.ts` doesn't need to import from `translate-body.ts`
   * (one-way dependency: translate-body imports ir1 types). The
   * build/lower pair convert between the two representations.
   *
   * Discriminated on `op` so the payload invariant is encoded in the
   * type: `set` carries a value, `delete` is value-less.
   */
  | {
      kind: "map-effect";
      op: "set";
      ruleName: string;
      keyPredName: string;
      ownerType: string;
      keyType: string;
      objExpr: IR1Expr;
      keyExpr: IR1Expr;
      valueExpr: IR1Expr;
    }
  | {
      kind: "map-effect";
      op: "delete";
      ruleName: string;
      keyPredName: string;
      ownerType: string;
      keyType: string;
      objExpr: IR1Expr;
      keyExpr: IR1Expr;
      valueExpr: null;
    }
  /**
   * Set mutation effect: `s.add(e)`, `s.delete(e)`, `s.clear()`.
   * Same translation discipline as `map-effect` — `add` / `delete`
   * carry the affected element, `clear` is element-less.
   */
  | {
      kind: "set-effect";
      op: "add" | "delete";
      ruleName: string;
      ownerType: string;
      elemType: string;
      objExpr: IR1Expr;
      elemExpr: IR1Expr;
    }
  | {
      kind: "set-effect";
      op: "clear";
      ruleName: string;
      ownerType: string;
      elemType: string;
      objExpr: IR1Expr;
      elemExpr: null;
    };

/**
 * `cond-stmt` variant restricted to `IR1ForeachBody` arms — the inner
 * branches inherit the same Shape A subset, so a nested
 * `cond-stmt(g, return …)` is unrepresentable. Distinct from the
 * outer `cond-stmt` form (whose arms are general `IR1Stmt`) because
 * the foreach-body invariant has to recurse all the way down.
 */
export type IR1ForeachCondStmt = {
  kind: "cond-stmt";
  arms: readonly [
    readonly [IR1Expr, IR1ForeachBody],
    ...ReadonlyArray<readonly [IR1Expr, IR1ForeachBody]>,
  ];
  otherwise: IR1ForeachBody | null;
};

/**
 * Statement shapes admissible as the body of an `IR1Stmt.foreach` —
 * the M3 Shape A contract: per-iter property writes (`assign` against
 * `Member(iter, p)`), conditional writes (`cond-stmt`), and blocks
 * composing those. Map/Set effects do not belong in the foreach body
 * (the lower pass would reject them as out-of-scope); a `for (x of
 * xs) { tags.add(x) }`-style write to an outer collection is not a
 * Shape A iteration. Other forms (`return`, `throw`, nested `for` /
 * `while`, `let`, `expr-stmt`) are also excluded — they have no
 * meaning under per-iteration quantified emission.
 *
 * Block bodies and cond-stmt arms recurse via `IR1ForeachCondStmt` so
 * the no-escape-hatch invariant holds at any depth at the type level.
 * `ensureForeachBodyShape` in `ir1-build-body.ts` is a runtime check
 * that bridges the gap when narrowing from a general `IR1Stmt`
 * produced by `buildL1IfMutation` (whose return type is `IR1Stmt`).
 */
export type IR1ForeachBody =
  | Extract<IR1Stmt, { kind: "assign" }>
  | IR1ForeachCondStmt
  | {
      kind: "block";
      stmts: readonly [IR1ForeachBody, ...IR1ForeachBody[]];
    };

// --------------------------------------------------------------------------
// SSA contract forms
// --------------------------------------------------------------------------
//
// Milestone 1 exposes the IR1 SSA vocabulary without wiring production
// builders or lowerers to emit it yet. Versions are opaque identities that
// carry their location metadata; callers compare version objects by identity
// and inspect `location` when checking structural compatibility.

const IR1_SSA_VERSION: unique symbol = Symbol("IR1_SSA_VERSION");

export type IR1SsaRuleName = string;

export type IR1SsaLocation =
  | {
      kind: "property";
      ruleName: IR1SsaRuleName;
      receiver: IR1Expr;
      property: string;
    }
  | {
      kind: "map-value";
      ruleName: IR1SsaRuleName;
      keyPredName: string;
      ownerType: string;
      keyType: string;
      receiver: IR1Expr;
      key: IR1Expr;
    }
  | {
      kind: "map-membership";
      ruleName: IR1SsaRuleName;
      keyPredName: string;
      ownerType: string;
      keyType: string;
      receiver: IR1Expr;
      key: IR1Expr;
    }
  | {
      kind: "set-membership";
      ruleName: IR1SsaRuleName;
      ownerType: string;
      elemType: string;
      receiver: IR1Expr;
    }
  | {
      kind: "return-value";
      ruleName: IR1SsaRuleName;
    };

export interface IR1SsaVersion {
  readonly kind: "ssa-version";
  readonly id: symbol;
  readonly location: IR1SsaLocation;
  readonly origin:
    | "initial"
    | "write"
    | "join"
    | "loop-summary"
    | "loop-header";
  readonly [IR1_SSA_VERSION]: true;
}

export type IR1SsaMapValue = {
  kind: "map-value";
  op: "set";
  value: IR1Expr;
};

export type IR1SsaMapMembershipValue = {
  kind: "map-membership";
  op: "set" | "delete";
};

export type IR1SsaSetMembershipValue =
  | {
      kind: "set-membership";
      op: "add" | "delete";
      elem: IR1Expr;
    }
  | {
      kind: "set-membership";
      op: "clear";
      elem: null;
    };

export type IR1SsaPropertyValue = {
  kind: "property";
  value: IR1Expr;
};

export type IR1SsaValue =
  | IR1SsaPropertyValue
  | IR1SsaMapValue
  | IR1SsaMapMembershipValue
  | IR1SsaSetMembershipValue;

export interface IR1SsaRead {
  readonly kind: "ssa-read";
  readonly location: IR1SsaLocation;
  readonly version: IR1SsaVersion;
  readonly dominated: boolean;
}

export interface IR1SsaWrite {
  readonly kind: "ssa-write";
  readonly location: IR1SsaLocation;
  readonly version: IR1SsaVersion;
  readonly value: IR1SsaValue;
}

export interface IR1SsaJoin {
  readonly kind: "ssa-join";
  readonly location: IR1SsaLocation;
  readonly thenVersion: IR1SsaVersion;
  readonly elseVersion: IR1SsaVersion;
  readonly joinVersion: IR1SsaVersion;
}

export interface IR1SsaLoopHeaderJoin {
  readonly kind: "ssa-loop-header-join";
  readonly location: IR1SsaLocation;
  readonly preheaderVersion: IR1SsaVersion;
  loopBackVersion: IR1SsaVersion | null;
  readonly joinVersion: IR1SsaVersion;
  closed: boolean;
}

export type IR1SsaTerminationMetric =
  | {
      readonly kind: "ssa-termination-metric";
      readonly expr: IR1Expr;
      readonly lowerBound: IR1Expr | null;
    }
  | {
      readonly kind: "ssa-iterating-source-metric";
      readonly source: IR1Expr;
    };

export interface IR1SsaBreakHandle {
  readonly kind: "ssa-break-handle";
  readonly location: IR1SsaLocation;
  readonly version: IR1SsaVersion;
}

export interface IR1SsaContinueHandle {
  readonly kind: "ssa-continue-handle";
  readonly location: IR1SsaLocation;
  readonly version: IR1SsaVersion;
}

export interface IR1SsaReturnHandle {
  readonly kind: "ssa-return-handle";
  readonly location: IR1SsaLocation;
  readonly version: IR1SsaVersion;
}

export interface IR1SsaThrowHandle {
  readonly kind: "ssa-throw-handle";
  readonly location: IR1SsaLocation;
  readonly version: IR1SsaVersion;
  readonly guard: IR1Expr;
}

export interface IR1SsaLoopBody {
  readonly kind: "ssa-loop-body";
  readonly headerJoins: readonly IR1SsaLoopHeaderJoin[];
  readonly writes: readonly IR1SsaWrite[];
  readonly joins: readonly IR1SsaJoin[];
  readonly breakHandles: readonly IR1SsaBreakHandle[];
  readonly continueHandles: readonly IR1SsaContinueHandle[];
  readonly returnHandles: readonly IR1SsaReturnHandle[];
  readonly throwHandles: readonly IR1SsaThrowHandle[];
  readonly terminationMetric: IR1SsaTerminationMetric | null;
}

export interface IR1SsaProgram {
  readonly reads: IR1SsaRead[];
  readonly writes: IR1SsaWrite[];
  readonly joins: IR1SsaJoin[];
  readonly loopHeaderJoins: IR1SsaLoopHeaderJoin[];
  readonly loopBodies: IR1SsaLoopBody[];
  readonly declaredRules: IR1SsaRuleName[];
  readonly modifiedRules: IR1SsaRuleName[];
  readonly framedRules: IR1SsaRuleName[];
}

export const ir1SsaPropertyLocation = (
  ruleName: IR1SsaRuleName,
  receiver: IR1Expr,
  property: string,
): IR1SsaLocation => ({
  kind: "property",
  ruleName,
  receiver,
  property,
});

export const ir1SsaMapValueLocation = (
  ruleName: IR1SsaRuleName,
  keyPredName: string,
  ownerType: string,
  keyType: string,
  receiver: IR1Expr,
  key: IR1Expr,
): IR1SsaLocation => ({
  kind: "map-value",
  ruleName,
  keyPredName,
  ownerType,
  keyType,
  receiver,
  key,
});

export const ir1SsaMapMembershipLocation = (
  ruleName: IR1SsaRuleName,
  keyPredName: string,
  ownerType: string,
  keyType: string,
  receiver: IR1Expr,
  key: IR1Expr,
): IR1SsaLocation => ({
  kind: "map-membership",
  ruleName,
  keyPredName,
  ownerType,
  keyType,
  receiver,
  key,
});

export const ir1SsaSetMembershipLocation = (
  ruleName: IR1SsaRuleName,
  ownerType: string,
  elemType: string,
  receiver: IR1Expr,
): IR1SsaLocation => ({
  kind: "set-membership",
  ruleName,
  ownerType,
  elemType,
  receiver,
});

export const ir1SsaReturnValueLocation = (
  ruleName: IR1SsaRuleName,
): IR1SsaLocation => ({
  kind: "return-value",
  ruleName,
});

export const ir1SsaRuleOfLocation = (
  location: IR1SsaLocation,
): IR1SsaRuleName =>
  location.kind === "map-membership" ? location.keyPredName : location.ruleName;

const ir1SsaVersion = (
  location: IR1SsaLocation,
  origin: IR1SsaVersion["origin"],
): IR1SsaVersion => ({
  kind: "ssa-version",
  id: Symbol(`ir1-ssa:${origin}`),
  location,
  origin,
  [IR1_SSA_VERSION]: true,
});

export const ir1SsaInitialVersion = (location: IR1SsaLocation): IR1SsaVersion =>
  ir1SsaVersion(location, "initial");

export const ir1SsaWrite = (
  location: IR1SsaLocation,
  value: IR1SsaValue,
): IR1SsaWrite => {
  ir1SsaAssertWriteValueCompatible(location, value);
  return {
    kind: "ssa-write",
    location,
    version: ir1SsaVersion(location, "write"),
    value,
  };
};

export const ir1SsaRead = (
  location: IR1SsaLocation,
  version: IR1SsaVersion,
  dominated = true,
): IR1SsaRead => {
  ir1SsaAssertLocationCompatible(location, version.location);
  return { kind: "ssa-read", location, version, dominated };
};

export const ir1SsaJoin = (
  location: IR1SsaLocation,
  thenVersion: IR1SsaVersion,
  elseVersion: IR1SsaVersion,
): IR1SsaJoin | IR1SsaVersion => {
  ir1SsaAssertLocationCompatible(location, thenVersion.location);
  ir1SsaAssertLocationCompatible(location, elseVersion.location);
  if (thenVersion === elseVersion) {
    return thenVersion;
  }
  return {
    kind: "ssa-join",
    location,
    thenVersion,
    elseVersion,
    joinVersion: ir1SsaVersion(location, "join"),
  };
};

export const ir1SsaOpenLoopHeader = (
  location: IR1SsaLocation,
  preheaderVersion: IR1SsaVersion,
): IR1SsaLoopHeaderJoin => {
  ir1SsaAssertLocationCompatible(location, preheaderVersion.location);
  return {
    kind: "ssa-loop-header-join",
    location,
    preheaderVersion,
    loopBackVersion: null,
    joinVersion: ir1SsaVersion(location, "loop-header"),
    closed: false,
  };
};

export const ir1SsaCloseLoopHeader = (
  header: IR1SsaLoopHeaderJoin,
  loopBackVersion: IR1SsaVersion,
): void => {
  if (header.closed) {
    throw new Error("loop-header already closed");
  }
  ir1SsaAssertLocationCompatible(header.location, loopBackVersion.location);
  header.loopBackVersion = loopBackVersion;
  header.closed = true;
};

export const ir1SsaTerminationMetric = (
  expr: IR1Expr,
  lowerBound: IR1Expr | null = null,
): IR1SsaTerminationMetric => ({
  kind: "ssa-termination-metric",
  expr,
  lowerBound,
});

export const ir1SsaIteratingSourceMetric = (
  source: IR1Expr,
): IR1SsaTerminationMetric => ({
  kind: "ssa-iterating-source-metric",
  source,
});

export const ir1SsaBreakHandle = (
  location: IR1SsaLocation,
  version: IR1SsaVersion,
): IR1SsaBreakHandle => {
  ir1SsaAssertLocationCompatible(location, version.location);
  return { kind: "ssa-break-handle", location, version };
};

export const ir1SsaContinueHandle = (
  location: IR1SsaLocation,
  version: IR1SsaVersion,
): IR1SsaContinueHandle => {
  ir1SsaAssertLocationCompatible(location, version.location);
  return { kind: "ssa-continue-handle", location, version };
};

export const ir1SsaReturnHandle = (
  location: IR1SsaLocation,
  version: IR1SsaVersion,
): IR1SsaReturnHandle => {
  ir1SsaAssertLocationCompatible(location, version.location);
  return { kind: "ssa-return-handle", location, version };
};

export const ir1SsaThrowHandle = (
  location: IR1SsaLocation,
  version: IR1SsaVersion,
  guard: IR1Expr,
): IR1SsaThrowHandle => {
  ir1SsaAssertLocationCompatible(location, version.location);
  return { kind: "ssa-throw-handle", location, version, guard };
};

export const ir1SsaLoopBody = (input: {
  readonly headerJoins?: readonly IR1SsaLoopHeaderJoin[];
  readonly writes?: readonly IR1SsaWrite[];
  readonly joins?: readonly IR1SsaJoin[];
  readonly breakHandles?: readonly IR1SsaBreakHandle[];
  readonly continueHandles?: readonly IR1SsaContinueHandle[];
  readonly returnHandles?: readonly IR1SsaReturnHandle[];
  readonly throwHandles?: readonly IR1SsaThrowHandle[];
  readonly terminationMetric?: IR1SsaTerminationMetric | null;
}): IR1SsaLoopBody => {
  const headerJoins = input.headerJoins ?? [];
  const writes = input.writes ?? [];
  const joins = input.joins ?? [];
  const breakHandles = input.breakHandles ?? [];
  const continueHandles = input.continueHandles ?? [];
  const returnHandles = input.returnHandles ?? [];
  const throwHandles = input.throwHandles ?? [];

  for (const header of headerJoins) {
    if (!header.closed) {
      throw new Error("loop body cannot wrap an open loop-header join");
    }
  }
  for (const handle of breakHandles) {
    ir1SsaAssertLocationCompatible(handle.location, handle.version.location);
  }
  for (const handle of continueHandles) {
    ir1SsaAssertLocationCompatible(handle.location, handle.version.location);
  }
  for (const handle of returnHandles) {
    ir1SsaAssertLocationCompatible(handle.location, handle.version.location);
  }
  for (const handle of throwHandles) {
    ir1SsaAssertLocationCompatible(handle.location, handle.version.location);
  }

  return Object.freeze({
    kind: "ssa-loop-body",
    headerJoins,
    writes,
    joins,
    breakHandles,
    continueHandles,
    returnHandles,
    throwHandles,
    terminationMetric: input.terminationMetric ?? null,
  });
};

export const ir1SsaPropertyValue = (value: IR1Expr): IR1SsaPropertyValue => ({
  kind: "property",
  value,
});

export const ir1SsaMapSetValue = (value: IR1Expr): IR1SsaMapValue => ({
  kind: "map-value",
  op: "set",
  value,
});

export const ir1SsaMapMembershipValue = (
  op: "set" | "delete",
): IR1SsaMapMembershipValue => ({
  kind: "map-membership",
  op,
});

export const ir1SsaSetMembershipValue = (
  op: "add" | "delete",
  elem: IR1Expr,
): IR1SsaSetMembershipValue => ({
  kind: "set-membership",
  op,
  elem,
});

export const ir1SsaSetClearValue = (): IR1SsaSetMembershipValue => ({
  kind: "set-membership",
  op: "clear",
  elem: null,
});

const ir1SsaAssertLocationCompatible = (
  expected: IR1SsaLocation,
  actual: IR1SsaLocation,
): void => {
  if (!ir1SsaLocationEquals(expected, actual)) {
    throw new Error("IR1 SSA version location mismatch");
  }
};

const ir1SsaLocationEquals = (
  a: IR1SsaLocation,
  b: IR1SsaLocation,
): boolean => {
  switch (a.kind) {
    case "property": {
      if (b.kind !== "property") {
        return false;
      }
      return (
        a.ruleName === b.ruleName &&
        a.property === b.property &&
        ir1SsaExprEquals(a.receiver, b.receiver)
      );
    }
    case "map-value": {
      if (b.kind !== "map-value") {
        return false;
      }
      return (
        a.ruleName === b.ruleName &&
        a.keyPredName === b.keyPredName &&
        a.ownerType === b.ownerType &&
        a.keyType === b.keyType &&
        ir1SsaExprEquals(a.receiver, b.receiver) &&
        ir1SsaExprEquals(a.key, b.key)
      );
    }
    case "map-membership": {
      if (b.kind !== "map-membership") {
        return false;
      }
      return (
        a.ruleName === b.ruleName &&
        a.keyPredName === b.keyPredName &&
        a.ownerType === b.ownerType &&
        a.keyType === b.keyType &&
        ir1SsaExprEquals(a.receiver, b.receiver) &&
        ir1SsaExprEquals(a.key, b.key)
      );
    }
    case "set-membership": {
      if (b.kind !== "set-membership") {
        return false;
      }
      return (
        a.ruleName === b.ruleName &&
        a.ownerType === b.ownerType &&
        a.elemType === b.elemType &&
        ir1SsaExprEquals(a.receiver, b.receiver)
      );
    }
    case "return-value": {
      if (b.kind !== "return-value") {
        return false;
      }
      return a.ruleName === b.ruleName;
    }
    default: {
      const _exhaustive: never = a;
      return _exhaustive;
    }
  }
};

export const ir1SsaExprEquals = (a: IR1Expr, b: IR1Expr): boolean => {
  switch (a.kind) {
    case "var": {
      if (b.kind !== "var") {
        return false;
      }
      return a.name === b.name && (a.primed ?? false) === (b.primed ?? false);
    }
    case "lit": {
      if (b.kind !== "lit") {
        return false;
      }
      return a.value.kind === b.value.kind && a.value.value === b.value.value;
    }
    case "binop": {
      if (b.kind !== "binop") {
        return false;
      }
      return (
        a.op === b.op &&
        ir1SsaExprEquals(a.lhs, b.lhs) &&
        ir1SsaExprEquals(a.rhs, b.rhs)
      );
    }
    case "unop": {
      if (b.kind !== "unop") {
        return false;
      }
      return a.op === b.op && ir1SsaExprEquals(a.arg, b.arg);
    }
    case "app": {
      if (b.kind !== "app") {
        return false;
      }
      return (
        ir1SsaExprEquals(a.callee, b.callee) &&
        ir1SsaArrayEquals(a.args, b.args, ir1SsaExprEquals)
      );
    }
    case "member": {
      if (b.kind !== "member") {
        return false;
      }
      return a.name === b.name && ir1SsaExprEquals(a.receiver, b.receiver);
    }
    case "cond": {
      if (b.kind !== "cond") {
        return false;
      }
      return (
        ir1SsaArrayEquals(a.arms, b.arms, ir1SsaCondArmEquals) &&
        ir1SsaExprEquals(a.otherwise, b.otherwise)
      );
    }
    case "is-nullish": {
      if (b.kind !== "is-nullish") {
        return false;
      }
      return ir1SsaExprEquals(a.operand, b.operand);
    }
    case "each": {
      if (b.kind !== "each") {
        return false;
      }
      return (
        a.binder === b.binder &&
        ir1SsaExprEquals(a.src, b.src) &&
        ir1SsaArrayEquals(a.guards, b.guards, ir1SsaExprEquals) &&
        ir1SsaExprEquals(a.proj, b.proj)
      );
    }
    case "comb-typed": {
      if (b.kind !== "comb-typed") {
        return false;
      }
      return (
        a.combiner === b.combiner &&
        a.binder === b.binder &&
        a.binderType === b.binderType &&
        ir1SsaArrayEquals(a.guards, b.guards, ir1SsaExprEquals) &&
        ir1SsaExprEquals(a.proj, b.proj)
      );
    }
    case "forall": {
      if (b.kind !== "forall") {
        return false;
      }
      return (
        a.binder === b.binder &&
        a.binderType === b.binderType &&
        optionalIr1SsaExprEquals(a.guard, b.guard) &&
        ir1SsaExprEquals(a.body, b.body)
      );
    }
    case "exists": {
      if (b.kind !== "exists") {
        return false;
      }
      return (
        a.binder === b.binder &&
        a.binderType === b.binderType &&
        optionalIr1SsaExprEquals(a.guard, b.guard) &&
        ir1SsaExprEquals(a.body, b.body)
      );
    }
    case "map-read": {
      if (b.kind !== "map-read") {
        return false;
      }
      return (
        a.op === b.op &&
        a.ruleName === b.ruleName &&
        a.keyPredName === b.keyPredName &&
        a.ownerType === b.ownerType &&
        a.keyType === b.keyType &&
        ir1SsaExprEquals(a.receiver, b.receiver) &&
        ir1SsaExprEquals(a.key, b.key)
      );
    }
    case "set-read": {
      if (b.kind !== "set-read") {
        return false;
      }
      return (
        a.ruleName === b.ruleName &&
        a.ownerType === b.ownerType &&
        a.elemType === b.elemType &&
        ir1SsaExprEquals(a.receiver, b.receiver) &&
        ir1SsaExprEquals(a.elem, b.elem)
      );
    }
    default: {
      const _exhaustive: never = a;
      return _exhaustive;
    }
  }
};

const ir1SsaCondArmEquals = (
  a: readonly [IR1Expr, IR1Expr],
  b: readonly [IR1Expr, IR1Expr],
): boolean => ir1SsaExprEquals(a[0], b[0]) && ir1SsaExprEquals(a[1], b[1]);

const optionalIr1SsaExprEquals = (
  a: IR1Expr | undefined,
  b: IR1Expr | undefined,
): boolean =>
  a === undefined || b === undefined
    ? a === undefined && b === undefined
    : ir1SsaExprEquals(a, b);

const ir1SsaArrayEquals = <T>(
  a: ReadonlyArray<T>,
  b: ReadonlyArray<T>,
  eq: (a: T, b: T) => boolean,
): boolean => {
  if (a.length !== b.length) {
    return false;
  }
  for (let i = 0; i < a.length; i += 1) {
    if (!eq(a[i] as T, b[i] as T)) {
      return false;
    }
  }
  return true;
};

const ir1SsaAssertWriteValueCompatible = (
  location: IR1SsaLocation,
  value: IR1SsaValue,
): void => {
  if (location.kind !== value.kind) {
    throw new Error(
      `IR1 SSA write value/location kind mismatch: location=${location.kind}, value=${value.kind}`,
    );
  }
};

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
 * Canonical L1 nullish test. The M4 build pass routes all nullish
 * surface forms (`x == null`, `x === null`, `x === undefined`, the
 * long `||`-form, and `typeof x === "undefined"`) through this
 * constructor; negated forms wrap as `unop("not", isNullish(x))`.
 * Lowers mechanically to `#x = 0` (cardinality-zero under list-lift),
 * the same shape `??` and `?.` already use. Operand may be any L1
 * expression — receiver chains and other non-`Var` shapes are accepted.
 */
export const ir1IsNullish = (operand: IR1Expr): IR1Expr => ({
  kind: "is-nullish",
  operand,
});

/**
 * L1 list comprehension. Used by the functor-lift recognizer to build
 * `each n in operand | projection` natively in L1. Mirrors L2 `each`
 * one-to-one; `lowerL1Expr` recurses on `src`/`guards`/`proj` and
 * constructs the L2 `each` form.
 */
export const ir1Each = (
  binder: string,
  src: IR1Expr,
  guards: IR1Expr[],
  proj: IR1Expr,
): IR1Expr => ({
  kind: "each",
  binder,
  src,
  guards,
  proj,
});

export const ir1CombTyped = (
  combiner: "min" | "max",
  binder: string,
  binderType: string,
  guards: IR1Expr[],
  proj: IR1Expr,
): IR1Expr => ({
  kind: "comb-typed",
  combiner,
  binder,
  binderType,
  guards,
  proj,
});

export const ir1Forall = (
  binder: string,
  binderType: string,
  body: IR1Expr,
  guard?: IR1Expr,
): IR1Expr =>
  guard !== undefined
    ? { kind: "forall", binder, binderType, guard, body }
    : { kind: "forall", binder, binderType, body };

export const ir1Exists = (
  binder: string,
  binderType: string,
  body: IR1Expr,
  guard?: IR1Expr,
): IR1Expr =>
  guard !== undefined
    ? { kind: "exists", binder, binderType, guard, body }
    : { kind: "exists", binder, binderType, body };

/**
 * State-aware Map read. Mutating-body lowering resolves this form
 * through IR1 SSA; the pure / read-only lower path (`ir1-lower.ts`) emits the bare
 * `App(callee, [receiver, key])` form (`callee = ruleName` for `get`,
 * `keyPredName` for `has`).
 */
export const ir1MapRead = (
  op: "get" | "has",
  ruleName: string,
  keyPredName: string,
  ownerType: string,
  keyType: string,
  receiver: IR1Expr,
  key: IR1Expr,
): IR1Expr => ({
  kind: "map-read",
  op,
  ruleName,
  keyPredName,
  ownerType,
  keyType,
  receiver,
  key,
});

/**
 * State-aware Set read. Mutating-body lowering resolves this form
 * through IR1 SSA; the pure / read-only lower path emits the bare
 * `Binop(in, elem, App(ruleName, [receiver]))` form.
 */
export const ir1SetRead = (
  ruleName: string,
  ownerType: string,
  elemType: string,
  receiver: IR1Expr,
  elem: IR1Expr,
): IR1Expr => ({
  kind: "set-read",
  ruleName,
  ownerType,
  elemType,
  receiver,
  elem,
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

export const ir1Break = (): IR1Stmt => ({ kind: "break" });

export const ir1Continue = (): IR1Stmt => ({ kind: "continue" });

// --------------------------------------------------------------------------
// Statement constructors for forms beyond M1's active set.
//
// All constructors are now active; the consumer side (lowering or build
// pipeline) is responsible for handling each form or rejecting with a
// specific reason. Adding a constructor that builds a vocabulary-locked
// IR1Stmt without a corresponding lowering arm is allowed during
// staged work — the lowering throws on first use, surfacing the gap
// rather than letting it silently miscompile.
// --------------------------------------------------------------------------

/**
 * Assignment / read-modify-write. Canonical form for all increment and
 * compound-assignment surface spellings.
 *
 * `target` is typically `Var(name)` (a counter), or `Member(receiver,
 * name)` (a property write). `value` is the new value — for increment
 * forms `BinOp(<op>, target, <k>)`.
 *
 * Two consumers:
 * - μ-search L1 construction: `buildL1MuSearchCombTyped` validates the
 *   canonical `Assign(c, c+1)` shape before building L1 `comb-typed`.
 * - Mutating-body lowering: `ir1-lower-body.ts` handles property writes
 *   (`Assign(Member(...), v)`) through IR1 SSA. Var-target assigns
 *   outside a μ-search shape reject.
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
  body: IR1ForeachBody | null,
  foldLeaves: IR1FoldLeaf[] = [],
): IR1Stmt => ({ kind: "foreach", binder, source, body, foldLeaves });

export const ir1For = (
  init: IR1Stmt | null,
  cond: IR1Expr | null,
  step: IR1Stmt | null,
  body: IR1Stmt,
): IR1Stmt => ({ kind: "for", init, cond, step, body });

/**
 * Bounded while loop. Used to faithfully represent loop-shaped TS bodies
 * for the mutating-body pipeline and statement-level tests. Pure μ-search
 * prelude recognition builds L1 `comb-typed` directly upstream; general
 * bounded-fixed-point / accumulator-iteration lowering is not implemented.
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

/**
 * Map mutation constructors. Split per `op` so the type system enforces
 * the payload invariant (`set` always carries a value; `delete` never
 * does). Build/lower call sites must dispatch on `op` before
 * constructing — see `buildL1EffectCall` in `ir1-build-body.ts`.
 */
export const ir1MapSet = (
  ruleName: string,
  keyPredName: string,
  ownerType: string,
  keyType: string,
  objExpr: IR1Expr,
  keyExpr: IR1Expr,
  valueExpr: IR1Expr,
): IR1Stmt => ({
  kind: "map-effect",
  op: "set",
  ruleName,
  keyPredName,
  ownerType,
  keyType,
  objExpr,
  keyExpr,
  valueExpr,
});

export const ir1MapDelete = (
  ruleName: string,
  keyPredName: string,
  ownerType: string,
  keyType: string,
  objExpr: IR1Expr,
  keyExpr: IR1Expr,
): IR1Stmt => ({
  kind: "map-effect",
  op: "delete",
  ruleName,
  keyPredName,
  ownerType,
  keyType,
  objExpr,
  keyExpr,
  valueExpr: null,
});

/**
 * Set mutation constructors. Split per `op` for the same reason as
 * `ir1MapSet` / `ir1MapDelete`: `add` / `delete` always carry an
 * element; `clear` never does.
 */
export const ir1SetAddOrDelete = (
  op: "add" | "delete",
  ruleName: string,
  ownerType: string,
  elemType: string,
  objExpr: IR1Expr,
  elemExpr: IR1Expr,
): IR1Stmt => ({
  kind: "set-effect",
  op,
  ruleName,
  ownerType,
  elemType,
  objExpr,
  elemExpr,
});

export const ir1SetClear = (
  ruleName: string,
  ownerType: string,
  elemType: string,
  objExpr: IR1Expr,
): IR1Stmt => ({
  kind: "set-effect",
  op: "clear",
  ruleName,
  ownerType,
  elemType,
  objExpr,
  elemExpr: null,
});
