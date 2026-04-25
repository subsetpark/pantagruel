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
