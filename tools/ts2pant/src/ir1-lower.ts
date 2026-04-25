/**
 * Layer 1 → Layer 2 lowering.
 *
 * Walks `IR1Expr` and produces L2 `IRExpr` (today's `ir.ts`). The downstream
 * lowering Layer 2 → OpaqueExpr lives in `ir-emit.ts` and is unchanged.
 *
 * **M1 scope**: `var`, `lit`, `binop`, `unop`, `app`, `member`, `cond`, and
 * the transitional `from-l2` form. Statement lowering (`lowerL1Stmt`) is
 * declared but throws — it lands in M3 alongside iteration + mutation
 * normalization.
 *
 * **Cond lowering.** L1 `cond({arms, otherwise})` produces L2
 * `irCond([...arms, [litBool(true), otherwise]])`. The trailing
 * `(true, otherwise)` arm is the canonical L2 cond shape; L1's
 * separate `otherwise` field exists so consumers can't construct a
 * value-position cond without a default.
 *
 * **Member lowering — already-qualified name.** L1 `member(receiver,
 * name)` lowers to L2 `App(name=name, [receiver])`. The build pass is
 * responsible for qualifying `name` to a rule (today's
 * `qualifyFieldAccess` machinery). The lowering is mechanical: it does
 * not consult a TypeChecker.
 */

import {
  type IRExpr,
  type IRStmt,
  irAppExpr,
  irAppName,
  irBinop,
  irCond,
  irLitBool,
  irUnop,
  irVar,
} from "./ir.js";
import type { IR1Expr, IR1Stmt } from "./ir1.js";

/**
 * Lower a Layer 1 expression to Layer 2 `IRExpr`.
 */
export function lowerL1Expr(e: IR1Expr): IRExpr {
  switch (e.kind) {
    case "var":
      return irVar(e.name, e.primed ?? false);

    case "lit":
      // L1 `lit` and L2 `lit` share the same shape (`IR1Literal = IRLiteral`),
      // so the variant is structurally a valid `IRExpr`. Pass through.
      return e;

    case "binop":
      return irBinop(e.op, lowerL1Expr(e.lhs), lowerL1Expr(e.rhs));

    case "unop":
      return irUnop(e.op, lowerL1Expr(e.arg));

    case "app": {
      // Specialize: a `var`-headed application lowers to L2 `App(name=...)`,
      // which produces the cleanest OpaqueExpr (a named function call).
      // Everything else lowers to L2 `App(expr=..., ...)`. Both are valid
      // IR; the specialization is purely cosmetic — for snapshot
      // byte-equality with the legacy path, which always emitted named
      // applications when the callee was an identifier.
      const args = e.args.map(lowerL1Expr);
      if (e.callee.kind === "var" && !e.callee.primed) {
        return irAppName(e.callee.name, args);
      }
      return irAppExpr(lowerL1Expr(e.callee), args);
    }

    case "member": {
      // Member is already-qualified at build time (responsibility of the
      // build pass; M1 doesn't construct `member` so this case is
      // exercised only by unit tests with hand-built trees, which pass
      // explicit qualified names). M5 (property-access normalization)
      // is where the build-time qualification logic lands.
      return irAppName(e.name, [lowerL1Expr(e.receiver)]);
    }

    case "cond": {
      const armPairs: Array<[IRExpr, IRExpr]> = e.arms.map(([g, v]) => [
        lowerL1Expr(g),
        lowerL1Expr(v),
      ]);
      armPairs.push([irLitBool(true), lowerL1Expr(e.otherwise)]);
      return irCond(armPairs);
    }

    case "from-l2":
      return e.expr;

    default: {
      const _exhaustive: never = e;
      void _exhaustive;
      throw new Error("unreachable: IR1Expr");
    }
  }
}

/**
 * Lower a Layer 1 statement to a list of L2 `IRStmt`. **Not implemented
 * in M1** — statement lowering lands in M3 alongside iteration + mutation
 * normalization, where L2's `Write` / `LetIf` / `Seq` forms become the
 * targets. Until then, calling this function on any L1 statement is a
 * bug; M1 only constructs and lowers L1 *expressions*.
 *
 * The signature returns the eventual M3 type (`IRStmt[]`) so adding the
 * real implementation in M3 is not a public type break — the body throws
 * unconditionally until then.
 */
export function lowerL1Stmt(s: IR1Stmt): IRStmt[] {
  void s;
  throw new Error(
    "lowerL1Stmt: Layer 1 statement lowering is M3 (iteration + mutation); " +
      "see workstreams/ts2pant-imperative-ir.md",
  );
}

// ---------------------------------------------------------------------------
// μ-search L1 recognizer (M2)
//
// Pattern-match the canonical L1 form
//   `Block([Let(c, init), While(p, Assign(Var(c), BinOp(add, Var(c), Lit(1))))])`
// against an L1 statement built via `buildL1LetWhile`. Returns
// `{ ok: true, init, predicate, counterName }` with the extracted
// pieces, or `{ ok: false, unsupported }` with a specific reason.
//
// **Single source of μ-search semantics.** Today the TS-AST level also
// validates step-is-`+1` and predicate-references-counter; M2 patch 3
// strips those checks at TS-AST and relies on this recognizer. This
// function is the canonical home for the canonical-shape check.
// ---------------------------------------------------------------------------

/**
 * Result of recognizing the canonical μ-search L1 form.
 */
export type MuSearchRecognition =
  | {
      ok: true;
      counterName: string;
      init: IR1Expr;
      predicate: IR1Expr;
    }
  | { ok: false; unsupported: string };

/**
 * Recognize the canonical μ-search L1 shape on the named counter.
 */
export function isCanonicalMuSearchForm(
  form: IR1Stmt,
  counterName: string,
): MuSearchRecognition {
  if (form.kind !== "block") {
    return { ok: false, unsupported: "μ-search prelude must be a Block" };
  }
  if (form.stmts.length !== 2) {
    return {
      ok: false,
      unsupported: "μ-search prelude must contain exactly Let + While",
    };
  }
  const letStmt = form.stmts[0];
  const whileStmt = form.stmts[1]!;
  if (letStmt.kind !== "let") {
    return {
      ok: false,
      unsupported: "first prelude statement must be a Let binding",
    };
  }
  if (letStmt.name !== counterName) {
    return {
      ok: false,
      unsupported: "Let binding name does not match counter",
    };
  }
  if (whileStmt.kind !== "while") {
    return {
      ok: false,
      unsupported: "second prelude statement must be a While loop",
    };
  }
  const body = whileStmt.body;
  if (body.kind !== "assign") {
    return {
      ok: false,
      unsupported: "while body must be a single Assign step",
    };
  }
  if (body.target.kind !== "var" || body.target.name !== counterName) {
    return {
      ok: false,
      unsupported: "Assign target is not the counter",
    };
  }
  const value = body.value;
  if (
    value.kind !== "binop" ||
    value.op !== "add" ||
    value.lhs.kind !== "var" ||
    value.lhs.name !== counterName ||
    value.rhs.kind !== "lit" ||
    value.rhs.value.kind !== "nat" ||
    value.rhs.value.value !== 1
  ) {
    return {
      ok: false,
      unsupported: "step is not a canonical `+1` increment",
    };
  }
  return {
    ok: true,
    counterName,
    init: letStmt.value,
    predicate: whileStmt.cond,
  };
}
