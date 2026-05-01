/**
 * Layer 1 → Layer 2 expression lowering.
 *
 * Walks `IR1Expr` and produces L2 `IRExpr` (`ir.ts`). The downstream
 * lowering Layer 2 → OpaqueExpr lives in `ir-emit.ts`.
 *
 * Layer 1 *statement* lowering does not route through here — mutating
 * bodies go through `ir1-lower-body.ts` which threads `SymbolicState`
 * directly into `PropResult[]`. The single μ-search recognizer below
 * is the one statement-shaped form that produces an L2 `IRExpr`
 * (`comb-typed`); everything else is expression-position.
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
  irCond,
  irEach,
  irLitNat,
  irUnop,
  irVar,
} from "./ir.js";
import { lowerExpr } from "./ir-emit.js";
import type { IR1Expr, IR1Stmt } from "./ir1.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import type { NumericStrategy } from "./translate-types.js";

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

    case "map-read": {
      // Pure / read-only path: emit the bare `App(callee, [receiver,
      // key])` form — byte-identical to the legacy fast-path output
      // when no symbolic state is in play. The body lower path
      // (`ir1-lower-body.ts`) intercepts `map-read` before reaching
      // here and dispatches to `readMapThroughWrites` so prior staged
      // writes are observed.
      const callee = e.op === "has" ? e.keyPredName : e.ruleName;
      return irAppName(callee, [lowerL1Expr(e.receiver), lowerL1Expr(e.key)]);
    }

    case "set-read": {
      // Pure / read-only path: emit the bare `Binop(in, elem,
      // App(rule, [receiver]))` form. The body lower path dispatches
      // this form to `readSetThroughWrites` before reaching here.
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
 * Validate that an L1 form matches the canonical μ-search shape on the
 * named counter. Returns `null` on success, an `{ unsupported }`
 * descriptor with a specific reason on failure. The L1 form's
 * sub-expressions (init, predicate) are not extracted — the caller
 * already has them via the source `MuSearch` and re-translates them
 * under binder substitution for the comprehension.
 */
export function isCanonicalMuSearchForm(
  form: IR1Stmt,
  counterName: string,
): { unsupported: string } | null {
  if (form.kind !== "block") {
    return { unsupported: "μ-search prelude must be a Block" };
  }
  if (form.stmts.length !== 2) {
    return {
      unsupported: "μ-search prelude must contain exactly Let + While",
    };
  }
  const letStmt = form.stmts[0];
  const whileStmt = form.stmts[1]!;
  if (letStmt.kind !== "let") {
    return { unsupported: "first prelude statement must be a Let binding" };
  }
  if (letStmt.name !== counterName) {
    return { unsupported: "Let binding name does not match counter" };
  }
  if (whileStmt.kind !== "while") {
    return { unsupported: "second prelude statement must be a While loop" };
  }
  const body = whileStmt.body;
  if (body.kind !== "assign") {
    return { unsupported: "while body must be a single Assign step" };
  }
  if (body.target.kind !== "var" || body.target.name !== counterName) {
    return { unsupported: "Assign target is not the counter" };
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
    return { unsupported: "step is not a canonical `+1` increment" };
  }
  return null;
}

// ---------------------------------------------------------------------------
// μ-search L1 → OpaqueExpr lowering (M2 cleanup)
//
// `lowerL1MuSearch` is the single source of μ-search semantics. It
// pattern-matches the canonical L1 form, validates the predicate
// references the counter and the strategy is discrete, and produces
// an `OpaqueExpr` with shape `min over each j: T, j >= INIT, ¬P(j) | j`
// — byte-equal to the pre-M2 legacy `translateMuSearchInit` emission.
//
// The substitution counter→binder happens on the lowered OpaqueExpr
// via `ast.substituteBinder` (Pant's capture-avoiding substitution
// at the wasm layer). Doing it post-emit (rather than via an L2
// rewriter) preserves the Barendregt-convention guarantee even if
// the predicate happens to bind the same name as the fresh
// μ-search binder, which a non-capture-avoiding L2 walker would
// silently mishandle.
// ---------------------------------------------------------------------------

/**
 * Context for μ-search lowering. The binder allocator is a callback
 * because the production path uses the synthCell-aware
 * `cellRegisterName` (collision-safe against the document-wide
 * NameRegistry) and standalone test paths fall back to a numeric
 * supply with collision checks against `scopedParams`. Threading the
 * synthCell + supply directly would couple `ir1-lower.ts` to
 * `translate-body.ts` internals; the callback inverts the dependency.
 */
export interface MuSearchLowerCtx {
  strategy: NumericStrategy;
  /**
   * Pant name for the counter in the current translation scope.
   * Typically `scopedParams.get(counterName) ?? counterName`. Used
   * as the variable name to substitute on the lowered predicate.
   */
  counterPantName: string;
  /** Allocate a fresh binder name (e.g., `j`, `j1`, …). */
  allocateBinder: (hint: string) => string;
}

/**
 * Lower an L1 μ-search-shaped form to an `OpaqueExpr`. Validates
 * canonical shape, predicate-references-counter, and strategy.
 *
 * Caller responsibility: build the L1 form via `buildL1LetWhile`
 * (which translates init and predicate sub-expressions as native L1
 * via `buildSubExpr`).
 *
 * Returns the lowered `OpaqueExpr` on success or `{ unsupported }`
 * with a specific reason. Returning `OpaqueExpr` (rather than an
 * L2 `IRExpr`) lets the substitution counter→binder use Pant's
 * capture-avoiding `ast.substituteBinder` at the wasm layer; an L2
 * walker would only be shadow-aware, not capture-avoiding, and
 * could silently mis-substitute when the predicate binds the same
 * name as the fresh μ-search binder.
 */
export function lowerL1MuSearch(
  form: IR1Stmt,
  counterName: string,
  ctx: MuSearchLowerCtx,
): OpaqueExpr | { unsupported: string } {
  const rejection = isCanonicalMuSearchForm(form, counterName);
  if (rejection !== null) {
    return rejection;
  }
  // Strategy must be discrete. μ-search enumerates `INIT, INIT+1, …`;
  // a Real binder would range over a dense domain and diverge from
  // that semantics.
  const counterType = ctx.strategy.mapNumber();
  if (counterType === "Real") {
    return {
      unsupported:
        "μ-search is only supported for discrete numeric strategies (got Real)",
    };
  }
  // After `isCanonicalMuSearchForm` has validated, we know the form
  // shape: Block([Let(c, init), While(p, Assign(c, c + 1))]).
  // Re-destructure to extract the init and predicate sub-expressions.
  if (form.kind !== "block" || form.stmts.length !== 2) {
    // Unreachable — the canonical-shape check above already verified
    // this. Defense in depth.
    return { unsupported: "μ-search prelude shape changed unexpectedly" };
  }
  const letStmt = form.stmts[0];
  const whileStmt = form.stmts[1]!;
  if (letStmt.kind !== "let" || whileStmt.kind !== "while") {
    return { unsupported: "μ-search prelude shape changed unexpectedly" };
  }
  const initL2 = lowerL1Expr(letStmt.value);
  const predicateL2 = lowerL1Expr(whileStmt.cond);
  // Predicate-references-counter is a precondition checked at L1
  // build time (see `buildL1LetWhile` in `ir1-build.ts`). Skipping
  // here — the lowering trusts that the L1 form represents a
  // well-formed let+while pair.
  //
  // Substitute counter → binder on the *predicate alone* via Pant's
  // wasm-level `ast.substituteBinder`. Substituting on the predicate
  // before it is wrapped in the outer comb-typed sidesteps Pant's
  // refuse-on-capture behavior — were we to substitute on the
  // already-wrapped form, `substituteBinder` would see the comb-
  // typed's `binder` parameter, observe that `binder` ∈ free-vars of
  // the replacement (`Var(binder)` itself), and decline to substitute.
  // Substituting on the bare predicate keeps the substitution
  // capture-avoiding for any *inner* binders within the predicate
  // (the documented hygienic-binder invariant guarantees the fresh
  // `binder` never collides with predicate-internal binders, but
  // this layer enforces it independently).
  const binder = ctx.allocateBinder("j");
  const ast = getAst();
  const predicateOpaque = ast.substituteBinder(
    lowerExpr(predicateL2),
    ctx.counterPantName,
    ast.var(binder),
  );
  const initOpaque = lowerExpr(initL2);
  // Emit `min over each binder: T, binder >= INIT, ~P | binder`
  // directly at the OpaqueExpr layer.
  return ast.eachComb(
    [ast.param(binder, ast.tName(counterType))],
    [
      ast.gExpr(ast.binop(ast.opGe(), ast.var(binder), initOpaque)),
      ast.gExpr(ast.unop(ast.opNot(), predicateOpaque)),
    ],
    ast.combMin(),
    ast.var(binder),
  );
}
