/**
 * Layer 1 → Layer 2 expression lowering.
 *
 * Walks `IR1Expr` and produces L2 `IRExpr` (`ir.ts`). The downstream
 * lowering Layer 2 → OpaqueExpr lives in `ir-emit.ts`.
 *
 * Layer 1 *statement* lowering does not route through here — mutating
 * bodies go through `ir1-lower-body.ts`, which lowers IR1 SSA into
 * `PropResult[]`. Recognition happens upstream at TS-AST → L1; this file
 * is a mechanical expression mapper.
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
  irAppExpr,
  irAppName,
  irBinop,
  irCombTyped,
  irCond,
  irEach,
  irExists,
  irForall,
  irLitNat,
  irUnop,
  irVar,
} from "./ir.js";
import type { IR1Expr } from "./ir1.js";

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
      return irCond(armPairs, lowerL1Expr(e.otherwise));
    }

    case "is-nullish":
      // Mechanical lowering under list-lift: `T | null` → `[T]`, so
      // "is nullish" is "list is empty", i.e. `#operand = 0`.
      return irBinop("eq", irUnop("card", lowerL1Expr(e.operand)), irLitNat(0));

    case "each": {
      // Structural mirror to L2 `each`. Used by the functor-lift
      // recognizer; mechanical lowering recurses on src/guards/proj.
      return irEach(
        e.binder,
        lowerL1Expr(e.src),
        e.guards.map(lowerL1Expr),
        lowerL1Expr(e.proj),
      );
    }

    case "comb-typed":
      return irCombTyped(
        e.combiner,
        e.binder,
        e.binderType,
        e.guards.map(lowerL1Expr),
        lowerL1Expr(e.proj),
      );

    case "forall":
      return irForall(
        e.binder,
        e.binderType,
        lowerL1Expr(e.body),
        e.guard === undefined ? undefined : lowerL1Expr(e.guard),
      );

    case "exists":
      return irExists(
        e.binder,
        e.binderType,
        lowerL1Expr(e.body),
        e.guard === undefined ? undefined : lowerL1Expr(e.guard),
      );

    case "map-read": {
      // Pure / read-only path: emit the bare `App(callee, [receiver,
      // key])` form. Mutating bodies resolve Map reads through IR1 SSA
      // before this pure lowering path is used.
      const callee = e.op === "has" ? e.keyPredName : e.ruleName;
      return irAppName(callee, [lowerL1Expr(e.receiver), lowerL1Expr(e.key)]);
    }

    case "set-read": {
      // Pure / read-only path: emit the bare `Binop(in, elem,
      // App(rule, [receiver]))` form. Mutating bodies resolve Set reads
      // through IR1 SSA before this pure lowering path is used.
      return irBinop(
        "in",
        lowerL1Expr(e.elem),
        irAppName(e.ruleName, [lowerL1Expr(e.receiver)]),
      );
    }

    default: {
      const _exhaustive: never = e;
      void _exhaustive;
      throw new Error("unreachable: IR1Expr");
    }
  }
}
