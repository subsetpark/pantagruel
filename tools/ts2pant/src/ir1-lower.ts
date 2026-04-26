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
  irCombTyped,
  irCond,
  irLitBool,
  irStmtSeq,
  irStmtWrite,
  irUnop,
  irVar,
  irWrap,
} from "./ir.js";
import { lowerExpr } from "./ir-emit.js";
import type { IR1Expr, IR1Stmt } from "./ir1.js";
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
 * Lower a Layer 1 statement to a list of L2 `IRStmt`.
 *
 * **M3 Patch 4 scope**: handles `block` (→ `seq`) and `cond-stmt` (→
 * `let-if`) and `assign` with a `member` target (→ `write{property-field}`).
 * Iteration kinds (`foreach`, `for`, `while`, `throw`) and the more
 * complex non-iteration kinds (`let` substitution, `return` packaging,
 * `expr-stmt` Map/Set effect detection) throw `not-implemented` pointing
 * at Patch 5, where the L1 build side gets wired up alongside the
 * branched-mutation cutover.
 *
 * Callers feed the resulting `IRStmt[]` to `emitStmt` (Patch 1) which
 * produces `{ equations, assertions, modifiedProps }` for installation
 * into the propositions array.
 */
export function lowerL1Stmt(s: IR1Stmt): IRStmt[] {
  switch (s.kind) {
    case "block": {
      const lowered: IRStmt[] = [];
      for (const child of s.stmts) {
        lowered.push(...lowerL1Stmt(child));
      }
      // Single-element `seq` collapses to its child at L2; >1 wraps in
      // `seq` so the emit pass sees a uniform sequence.
      if (lowered.length === 1) {
        return lowered;
      }
      return [irStmtSeq(lowered)];
    }

    case "assign": {
      // M3 Patch 4 supports member-target assigns (property writes).
      // Var-target assigns are reserved for μ-search counter steps and
      // are handled by `lowerL1MuSearch`, not by general statement
      // lowering.
      if (s.target.kind !== "member") {
        throw new Error(
          `lowerL1Stmt: assign with target kind '${s.target.kind}' is not ` +
            "supported by general statement lowering (var-target is " +
            "μ-search-only via lowerL1MuSearch)",
        );
      }
      return [
        irStmtWrite(
          {
            kind: "property-field",
            ruleName: s.target.name,
            objExpr: lowerL1Expr(s.target.receiver),
          },
          lowerL1Expr(s.value),
        ),
      ];
    }

    case "cond-stmt": {
      // Multi-armed cond-stmt folds right-to-left into nested `let-if`.
      // Each arm `(g, body)` becomes a `let-if(cond=g, then=lower(body),
      // else=<rest>)`. The final `otherwise` (or empty stmts) is the
      // innermost else-branch.
      const otherwiseStmts: IRStmt[] =
        s.otherwise === null ? [] : lowerL1Stmt(s.otherwise);
      let acc: IRStmt[] = otherwiseStmts;
      for (let i = s.arms.length - 1; i >= 0; i--) {
        const [guard, body] = s.arms[i]!;
        const thenStmts = lowerL1Stmt(body);
        // phiVars is informational at this lowering layer — emitStmt's
        // let-if processor computes touched keys from the actual
        // sub-state writes. Use a placeholder `["__phi__"]` to satisfy
        // the IRStmt type's non-empty invariant.
        acc = [
          {
            kind: "let-if",
            phiVars: ["__phi__"],
            cond: lowerL1Expr(guard),
            // biome-ignore lint/suspicious/noThenProperty: IRStmt ADT shape uses `then`/`else` for branching mutation merge.
            then: thenStmts,
            else: acc,
            continuation: [],
          },
        ];
      }
      return acc;
    }

    case "foreach":
      return lowerL1Foreach(s);

    case "let":
    case "return":
    case "expr-stmt":
    case "while":
    case "for":
    case "throw":
      throw new Error(
        `lowerL1Stmt: lowering for L1 form '${s.kind}' is deferred ` +
          "(L1 build + iteration cutover); see plan",
      );

    default: {
      const _exhaustive: never = s;
      void _exhaustive;
      throw new Error("unreachable: IR1Stmt");
    }
  }
}

// ---------------------------------------------------------------------------
// L1 foreach lowering (M3 Patch 5)
//
// `foreach(x: T, src, body)` lowers to one of:
//   - Shape A: body is `Assign(Member(x, p), e)` (or block of such writes)
//     → `quantified-stmt([{x, T}], [in(x, src)], lower(body))`
//   - Shape A with guard: body is `CondStmt([(g, body')], None)`
//     → `quantified-stmt([{x, T}], [in(x, src), g], lower(body'))`
//
// Shape B (accumulator fold) and Shape C (.reduce as expression) are
// recognized at the L1 build site (`ir1-build.ts`) and lowered there to
// L2 IRExpr forms (`comb`, `comb-typed`); they do not flow through this
// statement-level lowering. Mixed Shape A + Shape B bodies: each Shape
// B accumulator becomes its own L2 fold expression at build time and
// is wired into the surrounding equation; the residual Shape A writes
// pass through this lowering.
//
// **Implementation discipline (M3 Patch 5)**: this lowering recognizes
// the canonical Shape A (and guarded Shape A) shape only. Other body
// shapes (multiple statements, mutation through receivers other than
// the iterator, nested foreach) are rejected with a specific error.
// The build pass is responsible for either canonicalizing into one of
// these shapes at L1 construction time or rejecting up-front.
// ---------------------------------------------------------------------------

function lowerL1Foreach(s: Extract<IR1Stmt, { kind: "foreach" }>): IRStmt[] {
  const inGuard = irBinop("in", irVar(s.binder), lowerL1Expr(s.source));
  // Strip a single-armed guard cond-stmt at the top of the body
  // (Shape A with guard). Multi-armed cond inside a foreach is
  // structurally suspicious — reject for now (workstream open
  // question).
  let body = s.body;
  const extraGuards: IRExpr[] = [inGuard];
  if (body.kind === "cond-stmt") {
    if (body.arms.length !== 1 || body.otherwise !== null) {
      throw new Error(
        "lowerL1Foreach: only single-armed cond-stmt (no else) is " +
          "supported as a foreach body guard (Shape B with guard); " +
          "got multi-armed or with-else cond-stmt",
      );
    }
    const [g, innerBody] = body.arms[0]!;
    extraGuards.push(lowerL1Expr(g));
    body = innerBody;
  }
  // Body must be either a single member-target assign or a block of
  // such assigns.
  validateShapeABody(body);
  const loweredBody = lowerL1Stmt(body);
  // Wrap the (possibly multi-statement) body in a `seq` if needed,
  // then wrap that in `quantified-stmt`.
  const innerStmt: IRStmt =
    loweredBody.length === 1
      ? loweredBody[0]!
      : { kind: "seq", stmts: loweredBody };
  return [
    {
      kind: "quantified-stmt",
      quantifiers: [{ name: s.binder, type: s.binderType }],
      guards: extraGuards,
      body: innerStmt,
    },
  ];
}

function validateShapeABody(body: IR1Stmt): void {
  if (body.kind === "block") {
    for (const child of body.stmts) {
      validateShapeABody(child);
    }
    return;
  }
  if (body.kind !== "assign") {
    throw new Error(
      `lowerL1Foreach: foreach body must be Assign(Member, e) (Shape A); ` +
        `got '${body.kind}' — Shape B (accumulator fold) and other ` +
        "iteration shapes are recognized at the L1 build pass and lowered " +
        "to L2 IRExpr (comb), not via this statement-level path",
    );
  }
  if (body.target.kind !== "member") {
    throw new Error(
      `lowerL1Foreach: Shape A assign must target a Member; got ` +
        `'${body.target.kind}'`,
    );
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
// μ-search L1 → L2 lowering (M2 cleanup)
//
// `lowerL1MuSearch` is the single source of μ-search semantics. It
// pattern-matches the canonical L1 form, validates the predicate
// references the counter and the strategy is discrete, and produces
// an L2 `comb-typed` expression. The L2 → OpaqueExpr step in
// `ir-emit.ts` produces the same `min over each j: T, j >= INIT,
// ¬P(j) | j` shape today's legacy `translateMuSearchInit` emits —
// byte-equality is the cutover gate.
//
// The substitution counter→binder happens on the lowered OpaqueExpr
// via `ast.substituteBinder` (Pant's capture-avoiding substitution
// at the wasm layer) rather than re-translating the predicate under
// a new scope. One translation per sub-expression — no double work.
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
  allocateBinder(hint: string): string;
}

/**
 * Extract the L2 IRExpr from an L1 `from-l2` wrap, or return null if
 * the L1 expression is not a `from-l2`.
 */
function extractFromL2(e: IR1Expr): IRExpr | null {
  return e.kind === "from-l2" ? e.expr : null;
}

/**
 * Lower an L1 μ-search-shaped form to an L2 `comb-typed`. Validates
 * canonical shape, predicate-references-counter, and strategy.
 *
 * Caller responsibility: build the L1 form via `buildL1LetWhile`
 * (which translates init and predicate sub-expressions through the
 * legacy `translateBodyExpr` pipeline and wraps via `from-l2`).
 *
 * Returns `IRExpr` (an L2 `comb-typed`) on success, or
 * `{ unsupported }` with a specific reason.
 */
export function lowerL1MuSearch(
  form: IR1Stmt,
  counterName: string,
  ctx: MuSearchLowerCtx,
): IRExpr | { unsupported: string } {
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
  // The init and predicate are wrapped in `from-l2` by
  // `buildL1LetWhile` because their internal structure is not L1's
  // concern (M1 / M2 architectural commitment). Unwrap them here.
  const initL2 = extractFromL2(letStmt.value);
  if (initL2 === null) {
    return {
      unsupported: "μ-search init is not a from-l2 wrap (build pipeline error)",
    };
  }
  const predicateL2 = extractFromL2(whileStmt.cond);
  if (predicateL2 === null) {
    return {
      unsupported:
        "μ-search predicate is not a from-l2 wrap (build pipeline error)",
    };
  }
  // Predicate-references-counter is a precondition checked at L1
  // build time (see `buildL1LetWhile` in `ir1-build.ts`). Skipping
  // here — the lowering trusts that the L1 form represents a
  // well-formed let+while pair.
  const ast = getAst();
  const predicateOpaque = lowerExpr(predicateL2);
  // Allocate the comprehension binder and substitute counter → binder
  // on the lowered predicate. `substituteBinder` is Pant's
  // capture-avoiding substitution at the wasm layer; it traverses
  // OpaqueExpr correctly (the L2 IR cannot — `irWrap` holds an
  // opaque expression that `substIR` refuses to enter).
  const binder = ctx.allocateBinder("j");
  const substitutedPredicate = ast.substituteBinder(
    predicateOpaque,
    ctx.counterPantName,
    ast.var(binder),
  );
  // Build the L2 comb-typed:
  //   min over each j: T, j >= INIT, ¬P[c := j] | j
  return irCombTyped(
    "min",
    binder,
    counterType,
    [
      irBinop("ge", irVar(binder), initL2),
      irUnop("not", irWrap(substitutedPredicate)),
    ],
    irVar(binder),
  );
}
