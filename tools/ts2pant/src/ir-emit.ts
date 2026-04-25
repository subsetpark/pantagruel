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

import type {
  IRAssertExit,
  IRBinop,
  IRCombiner,
  IREquation,
  IRExpr,
  IRHead,
  IRStmt,
  IRUnop,
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
      const param = ast.param(
        e.binder,
        ast.tName(e.binderType ?? unknownBinderType()),
      );
      const guards = [
        ast.gIn(e.binder, lowerExpr(e.src)),
        ...e.guards.map((g) => ast.gExpr(lowerExpr(g))),
      ];
      // For comprehensions over a list expression, the binder is bound by
      // a `gIn` guard, not by a `param` of its own — so `params` is empty
      // and `guards` carries the binder. This matches the existing usage:
      // `ast.each([], [ast.gIn(binder, src), ...], proj)`.
      void param;
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
      // Identity-elision: `Comb(add, 0, ...)` collapses to the bare
      // comprehension; non-identity `init` folds in via the
      // corresponding Pant binop.
      if (e.init === undefined || isIdentityFor(e.combiner, e.init)) {
        return combExpr;
      }
      return ast.binop(
        combinerToBinop(e.combiner),
        lowerExpr(e.init),
        combExpr,
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

function combinerToBinop(c: IRCombiner): OpaqueBinop {
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
    case "min":
    case "max":
      // No binop equivalent — caller must not request init-folding for
      // min/max. Identity-elision (`isIdentityFor`) returns false for
      // these, but `e.init !== undefined` reaching here is a bug.
      throw new Error(
        `Comb combiner "${c}" has no binop fold; init must be undefined`,
      );
    default: {
      const _exhaustive: never = c;
      void _exhaustive;
      throw new Error("unreachable: IRCombiner");
    }
  }
}

function isIdentityFor(c: IRCombiner, init: IRExpr): boolean {
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
    case "min":
    case "max":
      return false;
    default: {
      const _exhaustive: never = c;
      void _exhaustive;
      return false;
    }
  }
}

// --------------------------------------------------------------------------
// IRStmt and IRBody — Stage 1 stubs (will expand in Stage 9+)
// --------------------------------------------------------------------------

/**
 * Lower an `IRStmt` list. Stage 1 throws — statement-layer emission is
 * Stage 9+ work. The signature is reserved here so call sites can be
 * stubbed without circular dependencies.
 */
export function lowerStmts(_stmts: IRStmt[]): never {
  throw new Error(
    "IRStmt lowering not implemented yet — Stage 9 introduces this",
  );
}

/**
 * Lower an `IREquation` to a `PropResult`-shaped object suitable for
 * pushing into the propositions array. Caller wraps in `kind: "equation"`.
 */
export function lowerEquation(eq: IREquation): {
  quantifiers: OpaqueParam[];
  guards: ReturnType<typeof getAst>["gExpr"] extends (e: OpaqueExpr) => infer G
    ? G[]
    : never;
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
  guards: ReturnType<typeof getAst>["gExpr"] extends (e: OpaqueExpr) => infer G
    ? G[]
    : never;
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
// Diagnostics
// --------------------------------------------------------------------------

function unknownBinderType(): string {
  // An `Each` form should always carry a binderType once Stage 4 (μ-search,
  // which canonically introduces a typed `j: Nat` binder) lands. Until
  // then, leaving binderType undefined falls back to a placeholder so
  // emission proceeds — but `pant --check` will reject the result.
  return "Unknown";
}
