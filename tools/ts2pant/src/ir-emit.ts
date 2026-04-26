/**
 * IR → OpaqueExpr lowering.
 *
 * Walks `IRExpr` and produces `OpaqueExpr` via `pant-ast.ts` constructors.
 * The IR is read-only here; build is in `ir-build.ts`.
 *
 * Stage 1 scope: handles `Var`, `Lit`, `App` (all heads), `Cond`, `Each`,
 * `Comb`, `Forall`, `Exists`, and the migration-only `IRWrap` escape
 * hatch. `Let` substitutes inline at lowering time (Pant has no `let`).
 *
 * Statement-layer lowering (Write, LetIf, Seq, Assert) is wired up in
 * Stage 9.
 *
 * See CLAUDE.md §IR for the form-by-form lowering rationale.
 */

import {
  type IRAssertExit,
  type IRBinop,
  type IRCombiner,
  type IREquation,
  type IRExpr,
  type IRFoldCombiner,
  type IRHead,
  type IRStmt,
  type IRUnop,
  type IRWriteTarget,
  irAppName,
  irAppPrimed,
  irBinop,
  irCond,
  irLitBool,
  irVar,
  irWrap,
  isFoldComb,
} from "./ir.js";
import type {
  OpaqueBinop,
  OpaqueCombiner,
  OpaqueExpr,
  OpaqueGuard,
  OpaqueParam,
  OpaqueUnop,
} from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";

// --------------------------------------------------------------------------
// Operator dispatch
// --------------------------------------------------------------------------

/** `IRBinop` → `OpaqueBinop`. One switch case per Pant op. */
function lowerBinop(op: IRBinop): OpaqueBinop {
  const ast = getAst();
  switch (op) {
    case "and":
      return ast.opAnd();
    case "or":
      return ast.opOr();
    case "impl":
      return ast.opImpl();
    case "iff":
      return ast.opIff();
    case "eq":
      return ast.opEq();
    case "neq":
      return ast.opNeq();
    case "lt":
      return ast.opLt();
    case "gt":
      return ast.opGt();
    case "le":
      return ast.opLe();
    case "ge":
      return ast.opGe();
    case "in":
      return ast.opIn();
    case "subset":
      return ast.opSubset();
    case "add":
      return ast.opAdd();
    case "sub":
      return ast.opSub();
    case "mul":
      return ast.opMul();
    case "div":
      return ast.opDiv();
    default: {
      const _exhaustive: never = op;
      void _exhaustive;
      throw new Error("unreachable: IRBinop");
    }
  }
}

function lowerUnop(op: IRUnop): OpaqueUnop {
  const ast = getAst();
  switch (op) {
    case "not":
      return ast.opNot();
    case "neg":
      return ast.opNeg();
    case "card":
      return ast.opCard();
    default: {
      const _exhaustive: never = op;
      void _exhaustive;
      throw new Error("unreachable: IRUnop");
    }
  }
}

function lowerCombiner(c: IRCombiner): OpaqueCombiner {
  const ast = getAst();
  switch (c) {
    case "add":
      return ast.combAdd();
    case "mul":
      return ast.combMul();
    case "and":
      return ast.combAnd();
    case "or":
      return ast.combOr();
    case "min":
      return ast.combMin();
    case "max":
      return ast.combMax();
    default: {
      const _exhaustive: never = c;
      void _exhaustive;
      throw new Error("unreachable: IRCombiner");
    }
  }
}

// --------------------------------------------------------------------------
// IRExpr → OpaqueExpr
// --------------------------------------------------------------------------

/**
 * Lower an `IRExpr` to an `OpaqueExpr`.
 *
 * `Let` is inlined via capture-avoiding substitution at lowering time —
 * the let-elimination is a separate pass in `ir-subst.ts` that callers
 * may run before `lowerExpr`, but `lowerExpr` itself also handles `Let`
 * by substituting the value into the body (see `ir-subst.ts` for the
 * shared substitution discipline).
 */
export function lowerExpr(e: IRExpr): OpaqueExpr {
  const ast = getAst();
  switch (e.kind) {
    case "var":
      return e.primed ? ast.primed(e.name) : ast.var(e.name);

    case "lit": {
      const v = e.value;
      switch (v.kind) {
        case "nat":
          return ast.litNat(v.value);
        case "bool":
          return ast.litBool(v.value);
        case "string":
          return ast.litString(v.value);
        default: {
          const _exhaustive: never = v;
          void _exhaustive;
          throw new Error("unreachable: IRLiteral");
        }
      }
    }

    case "app":
      return lowerApp(e.head, e.args.map(lowerExpr));

    case "cond":
      return ast.cond(e.arms.map(([g, v]) => [lowerExpr(g), lowerExpr(v)]));

    case "let": {
      // Pant has no let — inline the value into the body via
      // `substituteBinder` (Pant's capture-avoiding substitution at the
      // OpaqueExpr layer). Right-fold semantics drop out of recursive
      // lowering: `Let(a, ea, Let(b, eb, body))` lowers the inner Let
      // first (substituting b), then the outer (substituting a) — so
      // an `eb` that references `a` still gets resolved correctly. We
      // intentionally call `substituteBinder` here rather than `substIR`
      // because the body may contain `IRWrap(...)` (legacy fallback
      // output) that ir-subst can't traverse, and `substituteBinder`
      // operates uniformly on the lowered OpaqueExpr.
      return ast.substituteBinder(
        lowerExpr(e.body),
        e.name,
        lowerExpr(e.value),
      );
    }

    case "each": {
      const guards = [
        ast.gIn(e.binder, lowerExpr(e.src)),
        ...e.guards.map((g) => ast.gExpr(lowerExpr(g))),
      ];
      return ast.each([], guards, lowerExpr(e.proj));
    }

    case "comb": {
      const each = e.each;
      const guards = [
        ast.gIn(each.binder, lowerExpr(each.src)),
        ...each.guards.map((g) => ast.gExpr(lowerExpr(g))),
      ];
      const combExpr = ast.eachComb(
        [],
        guards,
        lowerCombiner(e.combiner),
        lowerExpr(each.proj),
      );
      // min/max have no `init` by construction (forbidden at the IR
      // type level). Foldable combiners get identity-elision and a
      // binop-fold for non-identity `init`.
      if (
        !isFoldComb(e) ||
        e.init === undefined ||
        isIdentityFor(e.combiner, e.init)
      ) {
        return combExpr;
      }
      return ast.binop(
        combinerToBinop(e.combiner),
        lowerExpr(e.init),
        combExpr,
      );
    }

    case "comb-typed": {
      // Source-less typed comprehension — `min over each j: T, g | proj`.
      // Distinct from `comb` (which iterates a `src`); produces the
      // typed-param form via `eachComb([param(b, type)], ...)`.
      return ast.eachComb(
        [ast.param(e.binder, ast.tName(e.binderType))],
        e.guards.map((g) => ast.gExpr(lowerExpr(g))),
        lowerCombiner(e.combiner),
        lowerExpr(e.proj),
      );
    }

    case "forall": {
      const param: OpaqueParam = ast.param(e.binder, ast.tName(e.binderType));
      const guards = e.guard ? [ast.gExpr(lowerExpr(e.guard))] : [];
      return ast.forall([param], guards, lowerExpr(e.body));
    }

    case "exists": {
      const param: OpaqueParam = ast.param(e.binder, ast.tName(e.binderType));
      const guards = e.guard ? [ast.gExpr(lowerExpr(e.guard))] : [];
      return ast.exists([param], guards, lowerExpr(e.body));
    }

    case "ir-wrap":
      return e.expr;
    default: {
      const _exhaustive: never = e;
      void _exhaustive;
      throw new Error("unreachable: IRExpr");
    }
  }
}

function lowerApp(head: IRHead, args: OpaqueExpr[]): OpaqueExpr {
  const ast = getAst();
  switch (head.kind) {
    case "name": {
      const fn = head.primed ? ast.primed(head.name) : ast.var(head.name);
      return ast.app(fn, args);
    }
    case "binop": {
      if (args.length !== 2) {
        throw new Error(
          `IRBinop "${head.op}" requires 2 args, got ${args.length}`,
        );
      }
      return ast.binop(lowerBinop(head.op), args[0]!, args[1]!);
    }
    case "unop": {
      if (args.length !== 1) {
        throw new Error(
          `IRUnop "${head.op}" requires 1 arg, got ${args.length}`,
        );
      }
      return ast.unop(lowerUnop(head.op), args[0]!);
    }
    case "expr": {
      // Arbitrary expression head — list-indexing `(x 1)`, etc.
      // Mirrors Pant's `ast.app(fn: OpaqueExpr, args)`.
      return ast.app(lowerExpr(head.expr), args);
    }
    default: {
      const _exhaustive: never = head;
      void _exhaustive;
      throw new Error("unreachable: IRHead");
    }
  }
}

// --------------------------------------------------------------------------
// Combiner identity helpers
// --------------------------------------------------------------------------

function combinerToBinop(c: IRFoldCombiner): OpaqueBinop {
  const ast = getAst();
  switch (c) {
    case "add":
      return ast.opAdd();
    case "mul":
      return ast.opMul();
    case "and":
      return ast.opAnd();
    case "or":
      return ast.opOr();
    default: {
      const _exhaustive: never = c;
      void _exhaustive;
      throw new Error("unreachable: IRFoldCombiner");
    }
  }
}

function isIdentityFor(c: IRFoldCombiner, init: IRExpr): boolean {
  if (init.kind !== "lit") {
    return false;
  }
  const v = init.value;
  switch (c) {
    case "add":
      return v.kind === "nat" && v.value === 0;
    case "mul":
      return v.kind === "nat" && v.value === 1;
    case "and":
      return v.kind === "bool" && v.value === true;
    case "or":
      return v.kind === "bool" && v.value === false;
    default: {
      const _exhaustive: never = c;
      void _exhaustive;
      return false;
    }
  }
}

// --------------------------------------------------------------------------
// IREquation / IRAssertExit lowering
// --------------------------------------------------------------------------

/**
 * Lower an `IREquation` to a `PropResult`-shaped object suitable for
 * pushing into the propositions array. Caller wraps in `kind: "equation"`.
 */
export function lowerEquation(eq: IREquation): {
  quantifiers: OpaqueParam[];
  guards: OpaqueGuard[];
  lhs: OpaqueExpr;
  rhs: OpaqueExpr;
} {
  const ast = getAst();
  return {
    quantifiers: eq.quantifiers.map((q) =>
      ast.param(q.name, ast.tName(q.type)),
    ),
    guards: (eq.guards ?? []).map((g) => ast.gExpr(lowerExpr(g))),
    lhs: lowerExpr(eq.lhs),
    rhs: lowerExpr(eq.rhs),
  };
}

export function lowerAssert(a: IRAssertExit): {
  quantifiers: OpaqueParam[];
  guards: OpaqueGuard[];
  body: OpaqueExpr;
} {
  const ast = getAst();
  return {
    quantifiers: a.quantifiers.map((q) => ast.param(q.name, ast.tName(q.type))),
    guards: (a.guards ?? []).map((g) => ast.gExpr(lowerExpr(g))),
    body: lowerExpr(a.body),
  };
}

// --------------------------------------------------------------------------
// IRStmt → IREquation[] / IRAssertExit[] (M3 Patch 1)
// --------------------------------------------------------------------------
//
// `emitStmt` walks a sequence of L2 IRStmts and accumulates their effects
// into a structured result: equations (one per modified rule, with
// per-rule overrides coalesced into a single override expression),
// assertions (Set membership writes), and the deterministic insertion-
// ordered set of modified rule names used by callers to drive frame-
// condition synthesis.
//
// Coalescing discipline mirrors the legacy `state.writes` Map in
// `translate-body.ts`: multiple `.set` calls on the same Map rule
// produce a single equation with multiple key-tuple overrides;
// repeated property-field writes overwrite (last-write-wins);
// `.clear()` on a Set drops the pre-state membership fallthrough.
//
// **Patch 1 scope**: `seq` (recurse + concat into a single
// accumulator) and `write` (all three target kinds). `let-if` and
// `quantified-stmt` arms throw NOT_IMPL — Patches 2 and 3 land them.

/**
 * Caller-supplied allocator for fresh quantifier binders. Used for the
 * `m1`/`k1`/`y` binders introduced by Map and Set write equations. The
 * production caller wires this to the document-wide `NameRegistry` (via
 * `cellRegisterName`) so binders don't collide with the function's
 * params or other emitted binders. Standalone tests can pass any
 * collision-safe allocator.
 */
export interface EmitStmtCtx {
  allocBinder: (hint: string) => string;
}

export interface EmitStmtResult {
  equations: IREquation[];
  assertions: IRAssertExit[];
  /**
   * Names of primed rules emitted by this statement sequence, in
   * insertion order. Threaded into `state.modifiedProps` by the
   * caller so `generateFrameConditions` produces frames for
   * unmodified rules only.
   */
  modifiedProps: Set<string>;
}

interface PropertyWriteAcc {
  kind: "property";
  ruleName: string;
  objExpr: IRExpr;
  value: IRExpr;
}

interface MapOverrideAcc {
  keyExpr: IRExpr;
  value: IRExpr;
}

interface MapWriteAcc {
  kind: "map";
  ruleName: string;
  keyPredName: string;
  ownerType: string;
  keyType: string;
  objExpr: IRExpr;
  valueOverrides: MapOverrideAcc[];
  membershipOverrides: MapOverrideAcc[];
}

interface SetOverrideAcc {
  elemExpr: IRExpr;
  value: IRExpr;
}

interface SetWriteAcc {
  kind: "set";
  ruleName: string;
  ownerType: string;
  elemType: string;
  objExpr: IRExpr;
  memberOverrides: SetOverrideAcc[];
  cleared: boolean;
}

type WriteAcc = PropertyWriteAcc | MapWriteAcc | SetWriteAcc;

interface EmitStmtState {
  // Insertion-ordered Map keyed by write-key string. Iteration order
  // is the order in which writes were first encountered, which feeds
  // deterministic emission and frame-ordering.
  writes: Map<string, WriteAcc>;
  // Assertions accumulated in textual order (Set membership writes).
  assertions: IRAssertExit[];
}

function makeEmitState(): EmitStmtState {
  return {
    writes: new Map(),
    assertions: [],
  };
}

/**
 * Compute a stable string key for a write target. Different writes to
 * the same rule + canonical receiver coalesce; writes to different
 * receivers stay separate.
 *
 * Patch 1 uses a structural string of the IRExpr; semantic
 * canonicalization (e.g., resolving `applyConst` substitutions) is the
 * caller's responsibility before emitting (mirrors how legacy
 * `installMapWrite` runs `applyConst` on the receiver before keying).
 */
function writeKey(target: IRWriteTarget): string {
  const objKey = irExprKey(target.objExpr);
  switch (target.kind) {
    case "property-field":
      return `prop:${target.ruleName}:${objKey}`;
    case "map-entry":
      return `map:${target.ruleName}:${objKey}`;
    case "set-member":
      return `set:${target.ruleName}:${objKey}`;
    default: {
      const _exhaustive: never = target;
      void _exhaustive;
      throw new Error("unreachable: IRWriteTarget");
    }
  }
}

/**
 * Structural string key for an IRExpr. Used only for write-coalescing
 * — the strings are not stable across IR refactors, but they are
 * stable within a single translation pass, which is all we need.
 * `ir-wrap` falls back to the OpaqueExpr identity (best-effort:
 * `OpaqueExpr` is opaque from TS, so two structurally-equal wraps
 * may produce different keys; in practice, callers pass the same
 * canonicalized OpaqueExpr instance for matching receivers).
 */
function irExprKey(e: IRExpr): string {
  switch (e.kind) {
    case "var":
      return `v:${e.primed ? "'" : ""}${e.name}`;
    case "lit":
      return litKey(e.value);
    case "app":
      return `a:${headKey(e.head)}(${e.args.map(irExprKey).join(",")})`;
    case "cond":
      return `c:${e.arms.map(([g, v]) => `${irExprKey(g)}=>${irExprKey(v)}`).join("|")}`;
    case "let":
      return `l:${e.name}=${irExprKey(e.value)};${irExprKey(e.body)}`;
    case "each":
      return `ea:${e.binder}@${irExprKey(e.src)}|${e.guards.map(irExprKey).join(",")}|${irExprKey(e.proj)}`;
    case "comb":
      return `cb:${e.combiner}|${irExprKey(e.each)}`;
    case "comb-typed":
      return `ct:${e.combiner}|${e.binder}:${e.binderType}|${e.guards.map(irExprKey).join(",")}|${irExprKey(e.proj)}`;
    case "forall":
      return `fa:${e.binder}:${e.binderType}|${e.guard ? irExprKey(e.guard) : ""}|${irExprKey(e.body)}`;
    case "exists":
      return `ex:${e.binder}:${e.binderType}|${e.guard ? irExprKey(e.guard) : ""}|${irExprKey(e.body)}`;
    case "ir-wrap":
      // OpaqueExpr is opaque from TS — best-effort identity.
      return `w:${String(e.expr)}`;
    default: {
      const _exhaustive: never = e;
      void _exhaustive;
      throw new Error("unreachable: IRExpr");
    }
  }
}

function litKey(v: Extract<IRExpr, { kind: "lit" }>["value"]): string {
  switch (v.kind) {
    case "nat":
      return `n:${v.value}`;
    case "bool":
      return `b:${v.value}`;
    case "string":
      return `s:${v.value}`;
    default: {
      const _exhaustive: never = v;
      void _exhaustive;
      throw new Error("unreachable: IRLiteral");
    }
  }
}

function headKey(head: IRHead): string {
  switch (head.kind) {
    case "name":
      return `${head.primed ? "'" : ""}${head.name}`;
    case "binop":
      return `bo:${head.op}`;
    case "unop":
      return `uo:${head.op}`;
    case "expr":
      return `e:${irExprKey(head.expr)}`;
    default: {
      const _exhaustive: never = head;
      void _exhaustive;
      throw new Error("unreachable: IRHead");
    }
  }
}

function processStmt(s: IRStmt, st: EmitStmtState): void {
  switch (s.kind) {
    case "seq":
      for (const child of s.stmts) {
        processStmt(child, st);
      }
      return;

    case "write":
      processWrite(s.target, s.value, st);
      return;

    case "let-if":
      throw new Error(
        "emitStmt: `let-if` lowering is M3 Patch 2 (φ-merged IREquation[]); " +
          "see plan at /Users/zax/.claude/plans/plan-it-snug-tulip.md",
      );

    case "assert":
      st.assertions.push({
        quantifiers: s.quantifiers,
        body: s.body,
      });
      return;

    default: {
      const _exhaustive: never = s;
      void _exhaustive;
      throw new Error("unreachable: IRStmt");
    }
  }
}

function processWrite(
  target: IRWriteTarget,
  value: IRExpr | null,
  st: EmitStmtState,
): void {
  const key = writeKey(target);
  switch (target.kind) {
    case "property-field": {
      if (value === null) {
        throw new Error(
          "emitStmt: property-field write requires a non-null value",
        );
      }
      // Last-write-wins: legacy `putWrite` overwrites prior entries
      // for the same write-key. Sequential writes to the same property
      // produce a single equation with the latest value.
      st.writes.set(key, {
        kind: "property",
        ruleName: target.ruleName,
        objExpr: target.objExpr,
        value,
      });
      return;
    }

    case "map-entry": {
      const prior = st.writes.get(key);
      const existing: MapWriteAcc =
        prior !== undefined && prior.kind === "map"
          ? prior
          : {
              kind: "map",
              ruleName: target.ruleName,
              keyPredName: target.keyPredName,
              ownerType: target.ownerType,
              keyType: target.keyType,
              objExpr: target.objExpr,
              valueOverrides: [],
              membershipOverrides: [],
            };
      const op = target.op;
      const keyExpr = target.keyExpr;
      if (op === "set") {
        if (value === null) {
          throw new Error("emitStmt: map-entry set requires a non-null value");
        }
        existing.valueOverrides.push({ keyExpr, value });
        existing.membershipOverrides.push({ keyExpr, value: irLitBool(true) });
      } else {
        // delete: drop any previous value override at the same key
        // and append a `false` membership override. Pantagruel's
        // declaration-guard discipline makes the value-rule body
        // vacuous under false membership, so we don't restate the
        // value for delete (matches legacy `installMapWrite`).
        existing.membershipOverrides.push({ keyExpr, value: irLitBool(false) });
      }
      st.writes.set(key, existing);
      return;
    }

    case "set-member": {
      const prior = st.writes.get(key);
      const existing: SetWriteAcc =
        prior !== undefined && prior.kind === "set"
          ? prior
          : {
              kind: "set",
              ruleName: target.ruleName,
              ownerType: target.ownerType,
              elemType: target.elemType,
              objExpr: target.objExpr,
              memberOverrides: [],
              cleared: false,
            };
      switch (target.op) {
        case "add":
          if (target.elemExpr === null) {
            throw new Error("emitStmt: set-member add requires elemExpr");
          }
          existing.memberOverrides.push({
            elemExpr: target.elemExpr,
            value: irLitBool(true),
          });
          break;
        case "delete":
          if (target.elemExpr === null) {
            throw new Error("emitStmt: set-member delete requires elemExpr");
          }
          existing.memberOverrides.push({
            elemExpr: target.elemExpr,
            value: irLitBool(false),
          });
          break;
        case "clear":
          existing.cleared = true;
          existing.memberOverrides = [];
          break;
        default:
          throw new Error("unreachable: set-member op");
      }
      st.writes.set(key, existing);
      return;
    }
    default: {
      const _exhaustive: never = target;
      void _exhaustive;
      throw new Error("unreachable: IRWriteTarget");
    }
  }
}

/**
 * Walk an IRStmt (or array of IRStmts) and produce the L2 emission
 * result: per-rule equations (with overrides coalesced), assertions,
 * and the insertion-order set of modified rule names.
 */
export function emitStmt(
  stmts: IRStmt | IRStmt[],
  ctx: EmitStmtCtx,
): EmitStmtResult {
  const st = makeEmitState();
  const items = Array.isArray(stmts) ? stmts : [stmts];
  for (const s of items) {
    processStmt(s, st);
  }
  return finalizeEmit(st, ctx);
}

function finalizeEmit(st: EmitStmtState, ctx: EmitStmtCtx): EmitStmtResult {
  const equations: IREquation[] = [];
  const modifiedProps = new Set<string>();

  for (const acc of st.writes.values()) {
    switch (acc.kind) {
      case "property":
        equations.push(emitPropertyEquation(acc));
        modifiedProps.add(acc.ruleName);
        break;
      case "map": {
        const eqs = emitMapEquations(acc, ctx);
        equations.push(...eqs);
        if (acc.valueOverrides.length > 0) {
          modifiedProps.add(acc.ruleName);
        }
        if (acc.membershipOverrides.length > 0) {
          modifiedProps.add(acc.keyPredName);
        }
        break;
      }
      case "set": {
        const a = emitSetMembershipAssertion(acc, ctx);
        if (a !== null) {
          st.assertions.push(a);
          modifiedProps.add(acc.ruleName);
        }
        break;
      }
      default: {
        const _exhaustive: never = acc;
        void _exhaustive;
        throw new Error("unreachable: WriteAcc");
      }
    }
  }

  return {
    equations,
    assertions: st.assertions,
    modifiedProps,
  };
}

function emitPropertyEquation(acc: PropertyWriteAcc): IREquation {
  return {
    quantifiers: [],
    lhs: irAppPrimed(acc.ruleName, [acc.objExpr]),
    rhs: acc.value,
  };
}

function emitMapEquations(acc: MapWriteAcc, ctx: EmitStmtCtx): IREquation[] {
  const ast = getAst();
  const eqs: IREquation[] = [];
  if (acc.valueOverrides.length > 0) {
    const m1 = ctx.allocBinder("m");
    const k1 = ctx.allocBinder("k");
    const overrides = acc.valueOverrides.map(
      (o) =>
        [
          ast.tuple([lowerExpr(acc.objExpr), lowerExpr(o.keyExpr)]),
          lowerExpr(o.value),
        ] as [OpaqueExpr, OpaqueExpr],
    );
    const rhs = irWrap(
      ast.app(ast.override(acc.ruleName, overrides), [
        ast.var(m1),
        ast.var(k1),
      ]),
    );
    eqs.push({
      quantifiers: [
        { name: m1, type: acc.ownerType },
        { name: k1, type: acc.keyType },
      ],
      lhs: irAppPrimed(acc.ruleName, [irVar(m1), irVar(k1)]),
      rhs,
    });
  }
  if (acc.membershipOverrides.length > 0) {
    const m1 = ctx.allocBinder("m");
    const k1 = ctx.allocBinder("k");
    const overrides = acc.membershipOverrides.map(
      (o) =>
        [
          ast.tuple([lowerExpr(acc.objExpr), lowerExpr(o.keyExpr)]),
          lowerExpr(o.value),
        ] as [OpaqueExpr, OpaqueExpr],
    );
    const rhs = irWrap(
      ast.app(ast.override(acc.keyPredName, overrides), [
        ast.var(m1),
        ast.var(k1),
      ]),
    );
    eqs.push({
      quantifiers: [
        { name: m1, type: acc.ownerType },
        { name: k1, type: acc.keyType },
      ],
      lhs: irAppPrimed(acc.keyPredName, [irVar(m1), irVar(k1)]),
      rhs,
    });
  }
  return eqs;
}

function emitSetMembershipAssertion(
  acc: SetWriteAcc,
  ctx: EmitStmtCtx,
): IRAssertExit | null {
  if (acc.memberOverrides.length === 0 && !acc.cleared) {
    return null;
  }
  const y = ctx.allocBinder("y");
  const yVar = irVar(y);
  const memberIn = (rule: IRExpr) => irBinop("in", yVar, rule);
  const primedApp = irAppPrimed(acc.ruleName, [acc.objExpr]);
  const preApp = irAppName(acc.ruleName, [acc.objExpr]);
  const fallback: [IRExpr, IRExpr] = acc.cleared
    ? [irLitBool(true), irLitBool(false)]
    : [irLitBool(true), memberIn(preApp)];
  const condArms: Array<[IRExpr, IRExpr]> = acc.memberOverrides.map((o) => [
    irBinop("eq", yVar, o.elemExpr),
    o.value,
  ]);
  const rhs =
    condArms.length === 0 ? fallback[1] : irCond([...condArms, fallback]);
  return {
    quantifiers: [{ name: y, type: acc.elemType }],
    body: irBinop("iff", memberIn(primedApp), rhs),
  };
}
