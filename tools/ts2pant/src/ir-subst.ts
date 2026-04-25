/**
 * IR-level capture-avoiding substitution.
 *
 * Mirrors the Barendregt-convention discipline used at the OpaqueExpr
 * layer (`pant-ast.ts:substituteBinder`), but operates on `IRExpr` nodes
 * so we don't have to lower → substitute → re-lift twice through the
 * substitution machinery. See `ir.ts` §"Hybrid SSA scope" for the
 * rationale.
 *
 * Stage 1 scope: handles all current `IRExpr` forms. Stage 6 (const-binding
 * migration) will exercise this against `Let` heavily; Stage 9 will add
 * IRStmt substitution for write-keyed `LetIf` φ-vars.
 *
 * **Hygiene assumption**: IR binders (`Let.name`, `Each.binder`,
 * `Forall.binder`, `Exists.binder`) come from the document-wide
 * `UniqueSupply` / `cellRegisterName` and are guaranteed not to collide
 * with parameter names or accessor rules. We therefore implement
 * substitution as straight name-based rewrite without α-renaming. If
 * binder allocation ever loses this property, this file needs an
 * α-renaming pass added.
 */

import {
  type IRExpr,
  type IRExprEach,
  irApp,
  irComb,
  irCond,
  irEach,
  irExists,
  irForall,
  irLet,
} from "./ir.js";

/**
 * Substitute every free occurrence of `name` in `body` with `value`.
 * Capture is avoided by skipping subterms that re-bind `name` (Let,
 * Each, Forall, Exists, and Each-as-comprehension binders).
 */
export function substIR(body: IRExpr, name: string, value: IRExpr): IRExpr {
  switch (body.kind) {
    case "var":
      return body.name === name && !body.primed ? value : body;

    case "lit":
      return body;

    case "app":
      return irApp(
        body.head.kind === "expr"
          ? { kind: "expr", expr: substIR(body.head.expr, name, value) }
          : body.head,
        body.args.map((a) => substIR(a, name, value)),
      );

    case "cond":
      return irCond(
        body.arms.map(([g, v]): [IRExpr, IRExpr] => [
          substIR(g, name, value),
          substIR(v, name, value),
        ]),
      );

    case "let":
      // Let always evaluates `value` in the outer scope (substitute it),
      // and only substitutes in the body if the let's binder doesn't
      // shadow `name`.
      return irLet(
        body.name,
        substIR(body.value, name, value),
        body.name === name ? body.body : substIR(body.body, name, value),
      );

    case "each": {
      const src = substIR(body.src, name, value);
      const shadowed = body.binder === name;
      const guards = shadowed
        ? body.guards
        : body.guards.map((g) => substIR(g, name, value));
      const proj = shadowed ? body.proj : substIR(body.proj, name, value);
      return irEach(body.binder, src, guards, proj, body.binderType);
    }

    case "comb": {
      const each = substIR(body.each, name, value) as IRExprEach;
      const init =
        body.init !== undefined ? substIR(body.init, name, value) : undefined;
      return irComb(body.combiner, each, init);
    }

    case "forall": {
      const shadowed = body.binder === name;
      const newGuard = shadowed
        ? body.guard
        : body.guard !== undefined
          ? substIR(body.guard, name, value)
          : undefined;
      const newBody = shadowed ? body.body : substIR(body.body, name, value);
      return irForall(body.binder, body.binderType, newBody, newGuard);
    }

    case "exists": {
      const shadowed = body.binder === name;
      const newGuard = shadowed
        ? body.guard
        : body.guard !== undefined
          ? substIR(body.guard, name, value)
          : undefined;
      const newBody = shadowed ? body.body : substIR(body.body, name, value);
      return irExists(body.binder, body.binderType, newBody, newGuard);
    }

    case "ir-wrap":
      // OpaqueExpr is opaque from TypeScript — substitution into an
      // already-lowered expression goes through `pant-ast.ts`'s
      // `substituteBinder`. But that requires lowering `value` first,
      // which means we need a callback. For Stage 1 we don't expect any
      // path that constructs a `Let` whose body contains an `IRWrap`
      // referring to the bound name (ir-build wraps wholesale OpaqueExpr
      // for unimplemented forms; the bound name `name` couldn't appear
      // inside a wrapped form by construction). Once Stage 6 lands
      // const-binding inlining, this case needs a strategy.
      return body;
    default: {
      const _exhaustive: never = body;
      void _exhaustive;
      throw new Error("unreachable: IRExpr in substIR");
    }
  }
}
