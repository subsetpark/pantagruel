/**
 * IRExpr → OpaqueExpr lowering.
 *
 * Walks `IRExpr` and produces `OpaqueExpr` via `pant-ast.ts` constructors.
 * The IR is read-only here; build is in `ir-build.ts` (and L1 → L2 in
 * `ir1-lower.ts`).
 *
 * Handles `Var`, `Lit`, `App` (all heads), `Cond`, `Each`, `Comb`,
 * `Forall`, `Exists`, and the migration-only `IRWrap` escape hatch.
 * `Let` substitutes inline at lowering time (Pant has no `let`).
 *
 * Mutating-body lowering does NOT route through here — it goes through
 * `ir1-lower-body.ts` directly into `PropResult[]`. See
 * `workstreams/ts2pant-imperative-ir.md` § "Architectural Lessons".
 */

import {
  type IRBinop,
  type IRCombiner,
  type IRExpr,
  type IRFoldCombiner,
  type IRHead,
  type IRUnop,
  isFoldComb,
} from "./ir.js";
import type {
  OpaqueBinop,
  OpaqueCombiner,
  OpaqueExpr,
  OpaqueParam,
  OpaqueUnop,
} from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";

// --------------------------------------------------------------------------
// Operator dispatch
// --------------------------------------------------------------------------

/** `IRBinop` → `OpaqueBinop`. One switch case per Pant op. */
export function lowerBinop(op: IRBinop): OpaqueBinop {
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
 * `Let` is inlined via capture-avoiding substitution at lowering time
 * (`ast.substituteBinder` — Pant's capture-avoiding substitution at
 * the OpaqueExpr layer). Right-fold semantics drop out of recursive
 * lowering: `Let(a, ea, Let(b, eb, body))` lowers the inner Let first
 * (substituting b), then the outer (substituting a).
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

    case "cond": {
      const arms: Array<[OpaqueExpr, OpaqueExpr]> = e.arms.map(([g, v]) => [
        lowerExpr(g),
        lowerExpr(v),
      ]);
      if (e.otherwise !== undefined) {
        arms.push([ast.litBool(true), lowerExpr(e.otherwise)]);
      }
      return ast.cond(arms);
    }

    case "let": {
      // Pant has no let — inline the value into the body via
      // `substituteBinder`. The body may contain `IRWrap(...)` (legacy
      // fallback output) which is opaque at the IR layer, so the
      // substitution operates on the lowered OpaqueExpr where Pant's
      // capture-avoiding machinery handles all forms uniformly.
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
