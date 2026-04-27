/**
 * TS AST → Layer 1 builder.
 *
 * **M1 scope** (workstream M1: imperative-ir-conditionals): conditional
 * **value** forms — ternary chains, if-with-returns, switch without
 * fall-through, `&&`/`||` when both operands are statically Bool-typed.
 * All collapse to one canonical L1 `cond([(g, v)], otherwise)` form.
 *
 * **M2 scope** (workstream M2: imperative-ir-assign-mu-search): increment
 * surface forms (`i++`, `++i`, `i += k`, `i -= k`, `i = i ⊕ k`,
 * `i = k ⊕ i`) build to canonical L1 `Assign(target, BinOp(<op>, target, <k>))`.
 * The five `+1` spellings produce identical L1 output. The TS-AST level
 * does NOT carry μ-search semantics — it only provides structural
 * acceptance for the let+while pair; semantic recognition (predicate
 * references counter, step is `+1`, discrete numeric strategy) lives
 * entirely in `ir1-lower.ts`'s `recognizeAndLowerMuSearch`.
 *
 * Sub-expressions inside conditionals (the guard, the value, the switch
 * discriminant) are translated through the legacy `translateBodyExpr`
 * pipeline and wrapped via the L1 `from-l2` adapter — see `ir1.ts`
 * for the lifetime contract on that adapter.
 *
 * **Conservative-refusal policy 3(b)**: when the build pass cannot prove
 * an equivalence (switch with possible fall-through, `==`/`===`-mixing
 * default-not-last, non-Bool short-circuit, non-literal switch case,
 * object-literal arms in a value-position cond), the function rejects
 * with an `unsupported` reason. Lifting any of these is a follow-up
 * within the workstream — see CLAUDE.md §"Imperative IR".
 *
 * **Hard rule per equivalence class** (workstream decision 4): every
 * conditional-value-shaped TS construct goes through this builder. There
 * is no fall-through to a legacy handler — L1 rejection is terminal.
 */

import ts from "typescript";
import { irWrap } from "./ir.js";
import { lowerExpr } from "./ir-emit.js";
import {
  type IR1Binop,
  type IR1Expr,
  type IR1Stmt,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1Cond,
  ir1FromL2,
  ir1Let,
  ir1LitBool,
  ir1LitNat,
  ir1LitString,
  ir1Member,
  ir1Unop,
  ir1Var,
  ir1While,
} from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import {
  type NullishTranslate,
  recognizeAnyLeaf,
  recognizeNullishForm,
  unwrapTransparentExpression,
} from "./nullish-recognizer.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import { isStaticallyBoolTyped } from "./purity.js";
import type {
  MuSearch,
  SymbolicState,
  UniqueSupply,
} from "./translate-body.js";
import {
  bodyExpr,
  expressionHasSideEffects,
  expressionReferencesNames,
  extractReturnFromBranch,
  isBodyUnsupported,
  isNullableTsType,
  qualifyFieldAccess,
  symbolicKey,
  translateBodyExpr,
} from "./translate-body.js";
import {
  cellRegisterName,
  isMapType,
  isSetType,
  type NumericStrategy,
} from "./translate-types.js";

/**
 * Context threaded through L1 build. Mirrors the parameter set of
 * `translateBodyExpr` so sub-expression delegation stays trivial.
 */
export interface L1BuildContext {
  checker: ts.TypeChecker;
  strategy: NumericStrategy;
  paramNames: ReadonlyMap<string, string>;
  state: SymbolicState | undefined;
  supply: UniqueSupply;
}

export type L1BuildResult = IR1Expr | { unsupported: string };

export const isL1Unsupported = (
  r: L1BuildResult,
): r is { unsupported: string } =>
  typeof r === "object" && r !== null && "unsupported" in r;

/**
 * L1-layering primitive: strip operationally-equivalent syntactic
 * wrappers at the L1 build boundary so downstream recognizers see
 * canonical shapes without re-implementing the strip themselves.
 *
 * Only `ParenthesizedExpression` is normalized away — type-erasure
 * wrappers (`as T`, `!`, `<T>x`, `satisfies`) change the TS-checker
 * type of the inner node and are load-bearing for type-dependent
 * dispatch (`qualifyFieldAccess`, nullability gates, etc.). They stay
 * on the standard build path that consults the type-checker.
 */
export function unwrapParens(n: ts.Node): ts.Node {
  while (ts.isParenthesizedExpression(n)) {
    n = n.expression;
  }
  return n;
}

// ---------------------------------------------------------------------------
// Cardinality dispatch (`.length` / `.size` → `Unop(card, x)`)
// ---------------------------------------------------------------------------
//
// Pant's primitive for list cardinality is `#x` (`Unop(card, x)`), not a
// `length` / `size` rule application. Routing `arr.length` through Member
// would lower to `length arr` — an EUF uninterpreted function distinct
// from `#arr`. Specs reasoning about list sizes would silently fail. So
// the build pass attempts cardinality recognition before Member dispatch
// for property access on the six list-shaped TS types: `Array`,
// `ReadonlyArray`, `Set`, `ReadonlySet`, `Map`, `ReadonlyMap`. This is
// a deliberate non-Member dispatch driven by the target language; see
// `tools/ts2pant/CLAUDE.md` § "Divergence from IRSC".

/**
 * True when `t` is a TS type whose cardinality is exposed via `length`
 * (arrays) or `size` (sets, maps). Centralizes the predicate so the
 * cardinality dispatch and the Member fallback agree on coverage.
 */
function isLengthCardinalityType(t: ts.Type, checker: ts.TypeChecker): boolean {
  return checker.isArrayType(t);
}

function isSizeCardinalityType(t: ts.Type): boolean {
  return isSetType(t) || isMapType(t);
}

/**
 * Try to build an L1 `Unop(card, receiver)` for `node` if it is a
 * non-optional `PropertyAccessExpression` whose receiver is a list-
 * shaped TS type and whose property is the cardinality slot for that
 * type. Returns null when the node is not a cardinality form — caller
 * falls through to `buildL1MemberAccess`.
 */
export function tryBuildL1Cardinality(
  node: ts.Expression,
  ctx: L1BuildContext,
): IR1Expr | null {
  const stripped = unwrapParens(node);
  if (!ts.isPropertyAccessExpression(stripped)) {
    return null;
  }
  // Optional-chain `.length` / `.size` (i.e., `xs?.length`) preserves
  // `?.` semantics — when `xs` is null the chain short-circuits to
  // `undefined`, which `#xs` would not encode. Defer to the optional-
  // chain handler in legacy `translateBodyExpr` rather than collapsing
  // to a cardinality.
  if ((stripped.flags & ts.NodeFlags.OptionalChain) !== 0) {
    return null;
  }
  const propName = stripped.name.text;
  if (propName !== "length" && propName !== "size") {
    return null;
  }
  const receiverNode = unwrapParens(stripped.expression);
  const receiverType = ctx.checker.getTypeAtLocation(receiverNode);
  const matches =
    (propName === "length" &&
      isLengthCardinalityType(receiverType, ctx.checker)) ||
    (propName === "size" && isSizeCardinalityType(receiverType));
  if (!matches) {
    return null;
  }
  // Receiver translates through the legacy pipeline so it picks up
  // const-binding inlining, symbolic-state lookup, and any other
  // body-level normalization. Wrap as `from-l2` for embedding under
  // L1 `Unop`.
  const r = translateBodyExpr(
    receiverNode as ts.Expression,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
  );
  if (isBodyUnsupported(r)) {
    return null;
  }
  if ("effect" in r) {
    return null;
  }
  return ir1Unop("card", ir1FromL2(irWrap(bodyExpr(r))));
}

// ---------------------------------------------------------------------------
// Property access → L1 Member
// ---------------------------------------------------------------------------
//
// Build the canonical L1 `Member(receiver, qualified-name)` form for a TS
// `PropertyAccessExpression` (and, in M5 Patch 3, string-literal
// `ElementAccessExpression`). The qualified name is resolved at build
// time via `qualifyFieldAccess` — IRSC discipline preserves the
// pre-qualification: L1 `Member` is a *normalization* form whose lowering
// at `ir1-lower.ts` is mechanical (`Member(r, n) → App(n, [lower r])`),
// matching the legacy `App(qualified-rule, [receiver])` byte-for-byte.

/**
 * Build an L1 `Member(receiverL1, qualifiedName)` for a TS property
 * access. Returns an `{ unsupported }` descriptor on ambiguous owner,
 * unsupported access shape, or a sub-expression that itself rejects.
 *
 * Receiver translation:
 *   - State-free contexts (pure path, unit tests) recurse on
 *     `PropertyAccessExpression` receivers, producing nested L1
 *     `Member` trees. The recursion bottoms out at a non-property
 *     receiver translated through the legacy pipeline and wrapped as
 *     `from-l2`.
 *   - State-bearing contexts (mutating-body path) translate the
 *     receiver via the legacy pipeline so symbolic-state lookup at
 *     each nested level fires identically to the pre-M5 path. This
 *     preserves byte-equality with the legacy snapshots while still
 *     routing the outer access through L1 `Member`.
 *
 * Patch 1 handles `PropertyAccessExpression` only; the
 * `ElementAccessExpression` arm (string-literal element access lands
 * on the same canonical Member; computed access rejects) is M5
 * Patch 3.
 */
export function buildL1MemberAccess(
  node: ts.PropertyAccessExpression | ts.ElementAccessExpression,
  ctx: L1BuildContext,
): L1BuildResult {
  const stripped = unwrapParens(node);
  if (!ts.isPropertyAccessExpression(stripped)) {
    // Patch 1 scope: ElementAccess lands in Patch 3 (string-literal key
    // canonicalizes to the same Member; computed key rejects).
    return {
      unsupported:
        "element access not yet routed through L1 Member (M5 Patch 3)",
    };
  }
  // Optional-chain `.f` (`x?.f` and the chain tail) routes through the
  // optional-chain functor-lift recognizer, not Member. Caller is
  // expected to filter optional chains before invoking this helper.
  if ((stripped.flags & ts.NodeFlags.OptionalChain) !== 0) {
    return {
      unsupported:
        "optional-chain property access uses functor-lift, not Member",
    };
  }
  const receiverNode = unwrapParens(stripped.expression);
  const fieldName = stripped.name.text;
  const receiverType = ctx.checker.getTypeAtLocation(receiverNode);
  const qualified = qualifyFieldAccess(
    receiverType,
    fieldName,
    ctx.checker,
    ctx.strategy,
    ctx.supply.synthCell,
  );
  if (qualified === null) {
    // Ambiguous owner — union/intersection members declare this field
    // on multiple distinct types.
    return {
      unsupported:
        `property access .${fieldName}: ambiguous owner — ` +
        `union/intersection members declare this field on multiple distinct ` +
        `types, so no single qualified accessor rule applies`,
    };
  }

  let receiverL1: IR1Expr;
  if (
    ctx.state === undefined &&
    ts.isPropertyAccessExpression(receiverNode) &&
    (receiverNode.flags & ts.NodeFlags.OptionalChain) === 0
  ) {
    // Pure path: prefer canonical nested Member trees. Cardinality
    // dispatch fires first so a chain like `arr.length.foo` wouldn't
    // silently route `arr.length` through Member.
    const card = tryBuildL1Cardinality(receiverNode, ctx);
    if (card !== null) {
      receiverL1 = card;
    } else {
      const inner = buildL1MemberAccess(receiverNode, ctx);
      if (isL1Unsupported(inner)) {
        return inner;
      }
      receiverL1 = inner;
    }
  } else {
    const r = translateBodyExpr(
      receiverNode as ts.Expression,
      ctx.checker,
      ctx.strategy,
      ctx.paramNames,
      ctx.state,
      ctx.supply,
    );
    if (isBodyUnsupported(r)) {
      return { unsupported: r.unsupported };
    }
    if ("effect" in r) {
      return {
        unsupported: "collection mutation as property-access receiver",
      };
    }
    receiverL1 = ir1FromL2(irWrap(bodyExpr(r)));
  }

  // Symbolic-state lookup at the outer level (mirrors
  // `translate-body.ts:2861`). When a prior write to this exact
  // property exists at this canonicalized receiver, return the prior
  // value rather than re-emitting the rule application.
  if (ctx.state !== undefined) {
    const objOpaque = lowerExpr(lowerL1Expr(receiverL1));
    const key = symbolicKey(qualified, ctx.state.canonicalize(objOpaque));
    const entry = ctx.state.writes.get(key);
    if (entry !== undefined && entry.kind === "property") {
      return ir1FromL2(irWrap(entry.value));
    }
  }

  return ir1Member(receiverL1, qualified);
}

// ---------------------------------------------------------------------------
// Recognizer entry — is this TS node a conditional shape M1 handles?
// ---------------------------------------------------------------------------

/**
 * True when `node` is a conditional **value** form M1 normalizes:
 * ternary, if-with-returns, switch w/o fall-through, or a Bool-typed
 * `&&`/`||`. The Bool-type check requires the checker, so the function
 * takes one as a parameter rather than being purely syntactic.
 *
 * `&&`/`||` with a non-Bool operand is *not* a conditional form for M1
 * — it stays on the legacy translateOperator path (or, post-cutover,
 * rejects as UNSUPPORTED at the call site).
 */
export function isL1ConditionalForm(
  node: ts.Node,
  checker: ts.TypeChecker,
): boolean {
  if (
    ts.isConditionalExpression(node) ||
    ts.isIfStatement(node) ||
    ts.isSwitchStatement(node)
  ) {
    return true;
  }
  if (
    ts.isBinaryExpression(node) &&
    (node.operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken ||
      node.operatorToken.kind === ts.SyntaxKind.BarBarToken)
  ) {
    return (
      isStaticallyBoolTyped(node.left, checker) &&
      isStaticallyBoolTyped(node.right, checker)
    );
  }
  return false;
}

// ---------------------------------------------------------------------------
// Sub-expression delegation: translate via legacy, wrap into L1 via from-l2
// ---------------------------------------------------------------------------

/**
 * Translate a non-conditional sub-expression via the legacy pipeline and
 * wrap as L1. Used for guards, values, and switch discriminants inside
 * an L1 conditional during M1 — those positions receive arbitrary TS
 * expressions whose normalization isn't M1's concern.
 *
 * Rejects collection-mutation sub-expressions (the legacy result type
 * `BodyResult` admits an "effect" variant that's only valid in
 * statement position; an L1 expression cannot host one).
 */
function buildSubExpr(expr: ts.Expression, ctx: L1BuildContext): L1BuildResult {
  expr = unwrapParens(expr) as ts.Expression;
  // Object literals don't lower to a Pantagruel value — Pant has no
  // record-constructor expression syntax. Reject *before* translating
  // so the error message is precise (legacy translateBodyExpr would
  // produce garbage instead).
  if (ts.isObjectLiteralExpression(expr)) {
    return { unsupported: "object literal in conditional arm" };
  }
  const result = translateBodyExpr(
    expr,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
  );
  if (isBodyUnsupported(result)) {
    return { unsupported: result.unsupported };
  }
  if ("effect" in result) {
    return {
      unsupported: "collection mutation cannot appear in a conditional arm",
    };
  }
  // Materialize any deferred .filter()/.map() chain into a flat each(...)
  // before wrapping — using result.expr directly would drop the traversal
  // and lower the bare projection body instead.
  return ir1FromL2(irWrap(bodyExpr(result)));
}

// ---------------------------------------------------------------------------
// Functor-lift recognizer (M4 Patch 5)
// ---------------------------------------------------------------------------
//
// `if (x == null) return []; else return [f(x)]` — and the equivalent
// ternary forms — lower as `each n in x | f n`, the canonical
// list-lifted comprehension under the `T | null → [T]` encoding. Pant
// has no list literal, so the alternative cardinality-dispatch lowering
// (`cond #x = 0 => [], true => [f((x 1))]`) has no expressible Pant
// target. This recognizer is the difference between translatable and
// untranslatable surface for idiomatic null-guarded list-returning TS.
//
// Eligibility is conservative: all four checks must hold (nullish
// guard, empty-equivalent empty side, single-element-producing present
// side referencing the operand, list-lifted result type). Any failure
// returns null and the caller falls through to standard L1 Cond
// construction — which then rejects at the no-list-literal wall, the
// pre-M4 behavior preserved.

/**
 * Candidate input for the functor-lift recognizer: the three TS
 * expressions that make up a value-position conditional, plus the
 * AST node from which the conditional's static result type can be
 * inferred (the ConditionalExpression itself, or the IfStatement
 * containing the branches).
 */
export interface FunctorLiftCandidate {
  guard: ts.Expression;
  thenExpr: ts.Expression;
  elseExpr: ts.Expression;
  /**
   * Node at which to compute the result type:
   *   - For a ternary, the `ConditionalExpression` itself
   *     (`getTypeAtLocation` returns the union of branch types).
   *   - For an `if (x == null) return []; return [f(x)]` shape, the
   *     enclosing function declaration so the return type can drive
   *     the list-lifted check.
   */
  contextNode: ts.Node;
}

const ARRAY_MULTI_PRODUCING_METHODS = new Set([
  "concat",
  "flat",
  "flatMap",
  "filter",
  "map",
  "slice",
  "splice",
]);

/**
 * True when `expr` is an empty-equivalent literal — `[]`, `null`, or
 * the global `undefined`. The `undefined` check is checker-aware
 * because `undefined` is not a reserved word in JS: a parameter,
 * local, or import named `undefined` is a legal binding that shadows
 * the global. The resolved type's flags must include `Undefined`; a
 * shadowed `undefined: number` resolves to `number` (no Undefined
 * flag) and is rejected — falling through to the standard Cond
 * build rather than silently rewriting semantics. Mirrors the
 * shadowing gate in nullish-recognizer.ts.
 */
function isEmptyEquivalent(
  expr: ts.Expression,
  checker: ts.TypeChecker,
): boolean {
  const u = unwrapTransparentExpression(expr);
  if (ts.isArrayLiteralExpression(u) && u.elements.length === 0) {
    return true;
  }
  if (u.kind === ts.SyntaxKind.NullKeyword) {
    return true;
  }
  if (ts.isIdentifier(u) && u.text === "undefined") {
    const type = checker.getTypeAtLocation(u);
    return (type.flags & ts.TypeFlags.Undefined) !== 0;
  }
  return false;
}

/**
 * If `expr` is a single-element array literal (`[e]`), return `e`. The
 * lift treats `[u.name]` and `u.name` symmetrically — both project to
 * the same `each n in u | name n` shape.
 *
 * Spread elements (`[...xs]`) and multi-element literals fall through
 * unchanged; they fail the present-side check below.
 */
function unwrapSingletonArray(expr: ts.Expression): ts.Expression {
  const u = unwrapTransparentExpression(expr);
  if (ts.isArrayLiteralExpression(u) && u.elements.length === 1) {
    const el = u.elements[0]!;
    if (!ts.isSpreadElement(el)) {
      return el as ts.Expression;
    }
  }
  return u;
}

/**
 * True when `expr` is a single-element-producing expression of
 * `operandName`. After stripping a single-element array wrapper, the
 * expression must reference the operand and not be a multi-element
 * construction (multi-element array literal, spread, or known
 * multi-producing array method like `.concat` / `.flat`).
 */
function isSingleElementProducingOf(
  expr: ts.Expression,
  operandName: string,
): boolean {
  const u = unwrapTransparentExpression(expr);
  if (ts.isArrayLiteralExpression(u)) {
    return false;
  }
  if (ts.isSpreadElement(u)) {
    return false;
  }
  if (ts.isCallExpression(u) && ts.isPropertyAccessExpression(u.expression)) {
    if (ARRAY_MULTI_PRODUCING_METHODS.has(u.expression.name.text)) {
      return false;
    }
  }
  return expressionReferencesNames(u, new Set([operandName]));
}

/**
 * True when `node`'s static type (or the enclosing function's return
 * type, for an IfStatement) is list-lifted: `T[]`, `T | null`,
 * `T | undefined`, or `T | null | undefined`.
 */
function isListLiftedTsType(t: ts.Type, checker: ts.TypeChecker): boolean {
  if (checker.isArrayType(t)) {
    return true;
  }
  return isNullableTsType(t);
}

function isListLiftedAtNode(node: ts.Node, checker: ts.TypeChecker): boolean {
  if (ts.isConditionalExpression(node)) {
    return isListLiftedTsType(checker.getTypeAtLocation(node), checker);
  }
  // For an if-statement (or return-statement, or the enclosing function
  // node itself for the if-conversion entry point at the top of a
  // pure body), the result type is the enclosing function's return
  // type. Walk up to the function-like declaration if `node` isn't
  // already one.
  let fnLike: ts.SignatureDeclaration | null = null;
  if (
    ts.isFunctionDeclaration(node) ||
    ts.isFunctionExpression(node) ||
    ts.isArrowFunction(node) ||
    ts.isMethodDeclaration(node) ||
    ts.isGetAccessorDeclaration(node) ||
    ts.isConstructorDeclaration(node)
  ) {
    fnLike = node;
  } else if (ts.isIfStatement(node) || ts.isReturnStatement(node)) {
    fnLike =
      ts.findAncestor(
        node,
        (a): a is ts.SignatureDeclaration =>
          ts.isFunctionDeclaration(a) ||
          ts.isFunctionExpression(a) ||
          ts.isArrowFunction(a) ||
          ts.isMethodDeclaration(a) ||
          ts.isGetAccessorDeclaration(a) ||
          ts.isConstructorDeclaration(a),
      ) ?? null;
  }
  if (!fnLike) {
    return false;
  }
  const sig = checker.getSignatureFromDeclaration(fnLike);
  if (!sig) {
    return false;
  }
  return isListLiftedTsType(checker.getReturnTypeOfSignature(sig), checker);
}

/**
 * Allocate a fresh comprehension binder name. Mirrors the μ-search
 * binder allocator: prefer `cellRegisterName` against the document-wide
 * NameRegistry when a synthCell is plumbed through; fall back to a
 * supply-based name with a collision check against the active scoped
 * params for standalone test paths.
 */
function allocateLiftBinder(ctx: L1BuildContext, hint: string): string {
  if (ctx.supply.synthCell) {
    return cellRegisterName(ctx.supply.synthCell, hint);
  }
  const used = new Set(ctx.paramNames.values());
  let name: string;
  do {
    const n = ctx.supply.n;
    ctx.supply.n = n + 1;
    name = `${hint}${n}`;
  } while (used.has(name));
  return name;
}

/**
 * Functor-lift recognizer. Returns an L1 `from-l2(each n in x | proj
 * n)` when the four eligibility checks pass; returns `null` to fall
 * through to the standard L1 Cond build.
 *
 * Eligibility:
 *   (a) Guard is a leaf nullish form (`x == null`, `x === null`,
 *       `x === undefined`, `typeof x === 'undefined'`, or their
 *       negations).
 *   (b) The "empty side" (the branch corresponding to `IsNullish(x) =
 *       true`) is empty-equivalent: `[]`, `null`, or `undefined`.
 *   (c) The "present side" (the `not IsNullish(x)` branch), after
 *       stripping a single-element array wrapper, is a
 *       single-element-producing expression that references the
 *       operand. Multi-element constructions reject.
 *   (d) The conditional's static result type is list-lifted (`T[]`,
 *       `T | null`, `T | undefined`, …).
 *
 * Operand restriction: the leaf operand must be a simple identifier.
 * Substitution maps the operand's Pant name to the binder via
 * `ast.substituteBinder`, which only rewrites variable references.
 * Property-access operands (`obj.prop == null`) would require
 * generalizing substitution to arbitrary expressions and are out of
 * scope for Patch 5; they fall through to the standard Cond build.
 */
export function tryRecognizeFunctorLift(
  cand: FunctorLiftCandidate,
  ctx: L1BuildContext,
): IR1Expr | null {
  // (a) Guard is a leaf nullish form.
  const guard = unwrapTransparentExpression(cand.guard);
  if (!ts.isBinaryExpression(guard)) {
    return null;
  }
  const leaf = recognizeAnyLeaf(guard, ctx.checker);
  if (leaf === null) {
    return null;
  }
  const operandNode = unwrapTransparentExpression(leaf.operand);
  if (!ts.isIdentifier(operandNode)) {
    return null;
  }
  const operandName = operandNode.text;

  // Polarity: a positive nullish guard (`x === null`) means the
  // then-branch is the empty side; a negated guard (`x !== null`)
  // means the then-branch is the present side.
  const emptyExpr = leaf.negated ? cand.elseExpr : cand.thenExpr;
  const presentExpr = leaf.negated ? cand.thenExpr : cand.elseExpr;

  // (b) Empty side is empty-equivalent.
  if (!isEmptyEquivalent(emptyExpr, ctx.checker)) {
    return null;
  }

  // (c) Present side, after stripping a singleton array wrapper, is a
  //     single-element-producing expression of `operandName`.
  const projection = unwrapSingletonArray(presentExpr);
  if (!isSingleElementProducingOf(projection, operandName)) {
    return null;
  }

  // (d) Result type is list-lifted.
  if (!isListLiftedAtNode(cand.contextNode, ctx.checker)) {
    return null;
  }

  // Build the lifted L1 expression.
  //
  // The operand and projection both translate through the legacy
  // pipeline so embedded sub-expressions stay normalized exactly as
  // they would on the standard Cond path. The substitution then
  // rewrites references to the operand's Pant name with the fresh
  // binder; `ast.substituteBinder` is Pant's capture-avoiding
  // substitution, so a fresh hygienic binder is sufficient.
  //
  // Snapshot the hygienic supply counter so that a `buildSubExpr`
  // failure (which can advance `supply.n` via deferred-comprehension
  // allocations before returning unsupported) doesn't perturb the
  // fallback Cond path's binder sequencing. The synthCell registration
  // via `cellRegisterName` runs only after the last failure point, so
  // it needs no rollback. Counter increments past the last failure
  // point are the lift's real allocations and stay committed.
  const supplyCounterSnapshot = ctx.supply.n;
  const operandSub = buildSubExpr(operandNode, ctx);
  if (isL1Unsupported(operandSub)) {
    ctx.supply.n = supplyCounterSnapshot;
    return null;
  }
  const projectionSub = buildSubExpr(projection, ctx);
  if (isL1Unsupported(projectionSub)) {
    ctx.supply.n = supplyCounterSnapshot;
    return null;
  }

  const operandOpaque = lowerExpr(lowerL1Expr(operandSub));
  const projectionOpaque = lowerExpr(lowerL1Expr(projectionSub));

  // Mirror `translateExpr`'s identifier handling
  // (`translate-signature.ts:1122` — `paramNames.get(expr.text) ?? expr.text`).
  // Only *parameters* go through name sanitization on translation;
  // bare identifiers (locals, captures from outer scope) are emitted
  // as their raw TS text. Sanitizing here would produce
  // `toPantTermName("maybeUser") = "maybe-user"`, which the lowered
  // projection — which still references `maybeUser` — would never
  // match, leaving the binder unsubstituted and the lift broken.
  const operandPantName = ctx.paramNames.get(operandName) ?? operandName;
  const binderName = allocateLiftBinder(ctx, "n");
  const ast = getAst();
  const substitutedProjection = ast.substituteBinder(
    projectionOpaque,
    operandPantName,
    ast.var(binderName),
  );

  const opaqueEach = ast.each(
    [],
    [ast.gIn(binderName, operandOpaque)],
    substitutedProjection,
  );
  return ir1FromL2(irWrap(opaqueEach));
}

// ---------------------------------------------------------------------------
// Top-level dispatch
// ---------------------------------------------------------------------------

/**
 * Build a Layer 1 value-position conditional from a TS conditional shape.
 * Dispatches on node kind; rejects with `unsupported` when the form
 * cannot be canonicalized (per conservative-refusal policy 3(b)).
 */
export function buildL1Conditional(
  expr: ts.Expression | ts.IfStatement | ts.SwitchStatement,
  ctx: L1BuildContext,
): L1BuildResult {
  expr = unwrapParens(expr) as
    | ts.Expression
    | ts.IfStatement
    | ts.SwitchStatement;
  if (ts.isConditionalExpression(expr)) {
    return buildFromTernary(expr, ctx);
  }
  if (ts.isIfStatement(expr)) {
    return buildFromIfStatement(expr, ctx);
  }
  if (ts.isSwitchStatement(expr)) {
    return buildFromSwitchStatement(expr, ctx);
  }
  if (
    ts.isBinaryExpression(expr) &&
    (expr.operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken ||
      expr.operatorToken.kind === ts.SyntaxKind.BarBarToken)
  ) {
    return buildFromShortCircuit(expr, ctx);
  }
  return { unsupported: "not a recognized conditional form" };
}

/**
 * Compose an L1 cond from pre-built L1 arm pairs and a TS terminal.
 *
 * Caller responsibility: pre-built arms are typically wrapped via
 * `ir1FromL2(irWrap(opaqueExpr))` from `inlineConstBindings`'s
 * already-translated `arms: [OpaqueExpr, OpaqueExpr][]` list. Terminal
 * is translated by this function — either as an L1 conditional shape
 * (if it matches one) or as a plain expression delegated through the
 * `from-l2` adapter.
 *
 * The flattening rule: if the terminal is itself a conditional that
 * lowers to an L1 cond, its arms get appended to the prelude arms and
 * its otherwise becomes the combined cond's otherwise. This produces
 * one L1 cond node that the lowerer flattens to one L2 cond — matching
 * today's legacy `cond([...arms, [true, terminal]])` materialization.
 */
export function buildL1ConditionalFromArms(
  preludeArms: ReadonlyArray<readonly [IR1Expr, IR1Expr]>,
  terminal: ts.Expression | ts.IfStatement | ts.SwitchStatement,
  ctx: L1BuildContext,
): L1BuildResult {
  terminal = unwrapParens(terminal) as
    | ts.Expression
    | ts.IfStatement
    | ts.SwitchStatement;
  const builtArms: Array<readonly [IR1Expr, IR1Expr]> = [...preludeArms];

  const terminalIsConditional =
    ts.isIfStatement(terminal) ||
    ts.isSwitchStatement(terminal) ||
    (ts.isExpression(terminal) && isL1ConditionalForm(terminal, ctx.checker));

  if (!terminalIsConditional) {
    if (builtArms.length === 0) {
      return { unsupported: "no conditional shape to build" };
    }
    if (!ts.isExpression(terminal)) {
      return { unsupported: "unsupported terminal form" };
    }
    const otherwise = buildSubExpr(terminal, ctx);
    if (isL1Unsupported(otherwise)) {
      return otherwise;
    }
    return ir1Cond(
      builtArms as [readonly [IR1Expr, IR1Expr], ...typeof builtArms],
      otherwise,
    );
  }

  // Terminal IS a conditional: build it as an L1 cond, then merge.
  const terminalL1 = buildL1Conditional(terminal, ctx);
  if (isL1Unsupported(terminalL1)) {
    return terminalL1;
  }
  if (terminalL1.kind === "cond") {
    for (const [g, v] of terminalL1.arms) {
      builtArms.push([g, v] as const);
    }
    if (builtArms.length === 0) {
      return { unsupported: "no conditional shape to build" };
    }
    return ir1Cond(
      builtArms as [readonly [IR1Expr, IR1Expr], ...typeof builtArms],
      terminalL1.otherwise,
    );
  }
  // Terminal lowered to a non-cond L1 (e.g., switch with only default
  // collapsed to its default value, or short-circuit returning a binop).
  // Treat the L1 result as the otherwise.
  if (builtArms.length === 0) {
    return terminalL1;
  }
  return ir1Cond(
    builtArms as [readonly [IR1Expr, IR1Expr], ...typeof builtArms],
    terminalL1,
  );
}

/**
 * Lower an L1 build result to an `OpaqueExpr` ready to drop into the
 * legacy pipeline. Combines `lowerL1Expr` and `lowerExpr` for callers
 * that want a one-liner.
 */
export function lowerL1ToOpaque(l1: IR1Expr): OpaqueExpr {
  return lowerExpr(lowerL1Expr(l1));
}

// ---------------------------------------------------------------------------
// Ternary chain flattening (right-associative)
// ---------------------------------------------------------------------------

function buildFromTernary(
  expr: ts.ConditionalExpression,
  ctx: L1BuildContext,
): L1BuildResult {
  // M4 Patch 5: functor-lift on a null-guarded list-lifted ternary —
  // `(x == null) ? [] : [f(x)]` collapses to `each n in x | f n`.
  // Only the outermost ternary is examined (no chain unwinding), since
  // the lift requires the full (guard, then, else) shape on one node.
  const lifted = tryRecognizeFunctorLift(
    {
      guard: expr.condition,
      thenExpr: expr.whenTrue,
      elseExpr: expr.whenFalse,
      contextNode: expr,
    },
    ctx,
  );
  if (lifted !== null) {
    return lifted;
  }

  const arms: Array<readonly [IR1Expr, IR1Expr]> = [];
  let current: ts.Expression = expr;
  // Right-leaning chain: a ? x : (b ? y : (c ? z : w)) → flatten.
  while (ts.isConditionalExpression(current)) {
    const guard = buildSubExpr(current.condition, ctx);
    if (isL1Unsupported(guard)) {
      return guard;
    }
    const value = buildSubExpr(current.whenTrue, ctx);
    if (isL1Unsupported(value)) {
      return value;
    }
    arms.push([guard, value] as const);
    current = current.whenFalse;
  }
  // Terminal else (non-ternary).
  const otherwise = buildSubExpr(current, ctx);
  if (isL1Unsupported(otherwise)) {
    return otherwise;
  }
  if (arms.length === 0) {
    // Cannot happen — we entered the loop because expr is a ternary.
    return { unsupported: "ternary with no arms (unreachable)" };
  }
  return ir1Cond(
    arms as [readonly [IR1Expr, IR1Expr], ...typeof arms],
    otherwise,
  );
}

// ---------------------------------------------------------------------------
// If-statement → cond (if-with-returns required for value position)
// ---------------------------------------------------------------------------

function buildFromIfStatement(
  stmt: ts.IfStatement,
  ctx: L1BuildContext,
): L1BuildResult {
  // M4 Patch 5: functor-lift on the simple two-branch shape
  // `if (x == null) return []; else return [f(x)]`. Only attempts the
  // lift when there is no else-if chain — multi-arm if-with-returns
  // can't be uniformly lifted to one comprehension.
  if (stmt.elseStatement && !ts.isIfStatement(stmt.elseStatement)) {
    const thenExpr = extractReturnFromBranch(stmt.thenStatement, ctx.checker);
    const elseExpr = extractReturnFromBranch(stmt.elseStatement, ctx.checker);
    if (thenExpr !== null && elseExpr !== null) {
      const lifted = tryRecognizeFunctorLift(
        {
          guard: stmt.expression,
          thenExpr,
          elseExpr,
          contextNode: stmt,
        },
        ctx,
      );
      if (lifted !== null) {
        return lifted;
      }
    }
  }

  const arms: Array<readonly [IR1Expr, IR1Expr]> = [];
  let current: ts.IfStatement = stmt;
  while (true) {
    if (!current.elseStatement) {
      return {
        unsupported:
          "if-without-else cannot lower to value-position cond (need both branches)",
      };
    }
    const guard = buildSubExpr(current.expression, ctx);
    if (isL1Unsupported(guard)) {
      return guard;
    }
    const thenExpr = extractReturnFromBranch(
      current.thenStatement,
      ctx.checker,
    );
    if (!thenExpr) {
      return {
        unsupported: "if-then branch must contain a single return-with-value",
      };
    }
    const thenValue = buildSubExpr(thenExpr, ctx);
    if (isL1Unsupported(thenValue)) {
      return thenValue;
    }
    arms.push([guard, thenValue] as const);

    // Else: another IfStatement (chain) or a terminal block-with-return.
    if (ts.isIfStatement(current.elseStatement)) {
      current = current.elseStatement;
      continue;
    }
    const elseExpr = extractReturnFromBranch(
      current.elseStatement,
      ctx.checker,
    );
    if (!elseExpr) {
      return {
        unsupported: "if-else branch must contain a single return-with-value",
      };
    }
    const elseValue = buildSubExpr(elseExpr, ctx);
    if (isL1Unsupported(elseValue)) {
      return elseValue;
    }
    if (arms.length === 0) {
      return {
        unsupported: "if-statement walk produced no arms (unreachable)",
      };
    }
    return ir1Cond(
      arms as [readonly [IR1Expr, IR1Expr], ...typeof arms],
      elseValue,
    );
  }
}

// ---------------------------------------------------------------------------
// Switch → cond (cases as arms, default as otherwise)
// ---------------------------------------------------------------------------

function buildFromSwitchStatement(
  stmt: ts.SwitchStatement,
  ctx: L1BuildContext,
): L1BuildResult {
  // Validate clause structure: every case ends with `return`/`break`/
  // `throw`; default is last (or the only clause); reject fall-through.
  const clauses = stmt.caseBlock.clauses;
  if (clauses.length === 0) {
    return { unsupported: "empty switch" };
  }

  let defaultIdx: number = -1;
  for (let i = 0; i < clauses.length; i++) {
    if (ts.isDefaultClause(clauses[i]!)) {
      if (i !== clauses.length - 1) {
        return { unsupported: "switch default must be the last clause" };
      }
      defaultIdx = i;
    }
  }
  if (defaultIdx === -1) {
    return {
      unsupported:
        "switch without default; literal-union exhaustiveness deferred (workstream open question)",
    };
  }

  // Discriminant must be side-effect-free — we structurally share it
  // across arms, so re-evaluation would diverge from TS's once-evaluated
  // semantics if it has effects.
  if (expressionHasSideEffects(stmt.expression, ctx.checker)) {
    return { unsupported: "switch discriminant has side effects" };
  }
  const disc = buildSubExpr(stmt.expression, ctx);
  if (isL1Unsupported(disc)) {
    return disc;
  }

  // Each non-default case: literal label, body ends in `return EXPR`.
  // M1 only supports return-cases; break/throw cases are rejected
  // (no coherent value-position lowering).
  const arms: Array<readonly [IR1Expr, IR1Expr]> = [];
  for (let i = 0; i < defaultIdx; i++) {
    const clause = clauses[i] as ts.CaseClause;
    const labelL1 = buildCaseLabel(clause.expression, ctx);
    if (isL1Unsupported(labelL1)) {
      return labelL1;
    }
    const ret = extractCaseReturn(clause);
    if (!ret) {
      return {
        unsupported:
          "switch case must end with `return EXPR` (no fall-through, no break/throw cases in M1)",
      };
    }
    const value = buildSubExpr(ret, ctx);
    if (isL1Unsupported(value)) {
      return value;
    }
    arms.push([ir1Binop("eq", disc, labelL1), value] as const);
  }

  // Default clause.
  const defaultClause = clauses[defaultIdx] as ts.DefaultClause;
  const defaultRet = extractCaseReturn(defaultClause);
  if (!defaultRet) {
    return {
      unsupported: "switch default must end with `return EXPR`",
    };
  }
  const otherwise = buildSubExpr(defaultRet, ctx);
  if (isL1Unsupported(otherwise)) {
    return otherwise;
  }

  if (arms.length === 0) {
    // Switch with only a default: collapse to just the default value.
    return otherwise;
  }
  return ir1Cond(
    arms as [readonly [IR1Expr, IR1Expr], ...typeof arms],
    otherwise,
  );
}

/**
 * Switch case labels must be literal — number, string, or boolean.
 * Non-literal labels (computed expressions) reject. Literal nat are
 * built directly to avoid re-translating through the legacy pipeline,
 * which is mechanical for literals but adds a `from-l2` wrap that
 * doesn't simplify under structural equality.
 */
function buildCaseLabel(
  label: ts.Expression,
  ctx: L1BuildContext,
): L1BuildResult {
  // Numeric literals: handles both plain `case 1:` and `case -1:`
  // (TS parses the latter as PrefixUnary(MinusToken, NumericLiteral)).
  const intLit = tryBuildL1IntegerLiteral(label);
  if (intLit !== null) {
    return intLit;
  }
  // Reject non-integer numerics with a precise reason; non-numeric
  // labels (string, bool, identifier) fall through to their own
  // branches below.
  if (
    ts.isNumericLiteral(label) ||
    (ts.isPrefixUnaryExpression(label) &&
      label.operator === ts.SyntaxKind.MinusToken &&
      ts.isNumericLiteral(label.operand))
  ) {
    return { unsupported: "switch case label: non-integer numeric literal" };
  }
  if (ts.isStringLiteral(label)) {
    return ir1LitString(label.text);
  }
  if (label.kind === ts.SyntaxKind.TrueKeyword) {
    return ir1LitBool(true);
  }
  if (label.kind === ts.SyntaxKind.FalseKeyword) {
    return ir1LitBool(false);
  }
  // Could be a const-bound identifier, a property access (enum), etc.
  // Defer those to a follow-up — M1 keeps switch labels strictly literal
  // to avoid mis-treating non-literal labels as fall-through-eligible
  // when their evaluation order matters.
  void ctx;
  return {
    unsupported: "switch case label must be a literal (number/string/bool)",
  };
}

/**
 * Extract the `return EXPR` from a case clause. Accepts the canonical
 * shape: a single `return EXPR;` statement (possibly preceded by a
 * single `break;` that's structurally unreachable — but we reject
 * `break` since it implies fall-through-by-omission elsewhere). Reject
 * any case body containing more than one statement, or with a
 * non-return last statement.
 */
function extractCaseReturn(
  clause: ts.CaseClause | ts.DefaultClause,
): ts.Expression | null {
  // Filter to the structural body. Each case body is a list of
  // statements at the top level (TS doesn't wrap them in a Block by
  // default). M1 requires exactly one statement: `return EXPR;`.
  const stmts = clause.statements;
  if (stmts.length === 0) {
    // Empty case body = fall-through. Reject.
    return null;
  }
  // Walk past a trailing `break;` if present (no-op in a case body that
  // ends with return — but if the *only* stmt is break, that's a
  // statement-position effect M1 doesn't support).
  let lastIdx = stmts.length - 1;
  if (lastIdx > 0 && stmts[lastIdx]!.kind === ts.SyntaxKind.BreakStatement) {
    lastIdx--;
  }
  const last: ts.Statement = stmts[lastIdx]!;
  // Accept `{ return EXPR; }` block.
  if (
    ts.isBlock(last) &&
    last.statements.length === 1 &&
    ts.isReturnStatement(last.statements[0]!) &&
    last.statements[0]!.expression
  ) {
    return last.statements[0]!.expression!;
  }
  // Only one effective statement allowed (after trailing-break trim).
  if (lastIdx !== 0) {
    return null;
  }
  if (ts.isReturnStatement(last) && last.expression) {
    return last.expression;
  }
  return null;
}

// ---------------------------------------------------------------------------
// && / || → cond (Bool-typed only)
// ---------------------------------------------------------------------------

function buildFromShortCircuit(
  expr: ts.BinaryExpression,
  ctx: L1BuildContext,
): L1BuildResult {
  // M4: try the long-form nullish recognizer first. The recognizer
  // collapses `x === null || x === undefined` (and the negated `&&`
  // variant) into a single `IsNullish(x)` rather than two equality
  // tests joined by `or`. Operand-mismatch falls through and the
  // chain is handled by the regular Bool-typed short-circuit below.
  const nullishTranslate: NullishTranslate = (sub) => {
    const result = buildSubExpr(sub, ctx);
    if (isL1Unsupported(result)) {
      return { unsupported: result.unsupported };
    }
    return result;
  };
  const recognized = recognizeNullishForm(expr, ctx.checker, nullishTranslate);
  if (recognized !== null) {
    if ("unsupported" in recognized) {
      return { unsupported: recognized.unsupported };
    }
    return recognized;
  }

  if (
    !isStaticallyBoolTyped(expr.left, ctx.checker) ||
    !isStaticallyBoolTyped(expr.right, ctx.checker)
  ) {
    return {
      unsupported:
        "&&/|| requires both operands to be statically Bool-typed (workstream conservative-refusal 3(b))",
    };
  }
  const left = buildSubExpr(expr.left, ctx);
  if (isL1Unsupported(left)) {
    return left;
  }
  const right = buildSubExpr(expr.right, ctx);
  if (isL1Unsupported(right)) {
    return right;
  }
  if (expr.operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken) {
    return ir1Binop("and", left, right);
  }
  return ir1Binop("or", left, right);
}

// ---------------------------------------------------------------------------
// Increment-step normalization (M2)
//
// Collapse all surface spellings of an increment on a counter into one
// canonical L1 `Assign(Var(c), BinOp(<op>, Var(c), <k>))`. The five `+1`
// spellings (`i++`, `++i`, `i += 1`, `i = i + 1`, `i = 1 + i`) all
// produce identical L1 output. Compound assignments (`i += k`) and
// commutative explicit forms (`i = k * i`) build to the same Assign
// shape with the appropriate op and operand.
//
// The TS-AST level here is purely structural — it does NOT validate
// that the step is `+1`, that the step matches a μ-search shape, or
// anything semantic about the surrounding context. The L1 recognizer in
// `ir1-lower.ts:recognizeAndLowerMuSearch` is responsible for those
// checks.
// ---------------------------------------------------------------------------

export type L1StmtBuildResult = IR1Stmt | { unsupported: string };

export const isL1StmtUnsupported = (
  r: L1StmtBuildResult,
): r is { unsupported: string } =>
  typeof r === "object" && r !== null && "unsupported" in r;

/**
 * Build an L1 increment-step `Assign` from a TS expression-statement
 * body. Accepts six surface shapes on the named counter:
 *
 *  - `c++` / `++c` → `Assign(Var(c), BinOp(add, Var(c), Lit(1)))`
 *  - `c--` / `--c` → `Assign(Var(c), BinOp(sub, Var(c), Lit(1)))`
 *  - `c += k`, `c -= k`, `c *= k`, `c /= k`
 *      → `Assign(Var(c), BinOp(<op>, Var(c), <k>))`
 *  - `c = c ⊕ k` / `c = k ⊕ c` (commutative `⊕`)
 *      → `Assign(Var(c), BinOp(<op>, Var(c), <k>))`
 *
 * The non-`+1` cases produce a valid L1 form but the μ-search recognizer
 * at L1 lowering rejects them. `i = k - i` (non-commutative on right)
 * rejects here because canonical form requires the counter on the
 * left side of the binop.
 *
 * Anything else — assignment to a different variable, non-binop RHS,
 * non-literal `k` (not rejected here; the L1 recognizer handles it) —
 * either rejects or builds a non-canonical Assign that downstream
 * recognition won't match.
 */
export function buildL1IncrementStep(
  expr: ts.Expression,
  counterName: string,
  ctx: L1BuildContext,
): L1StmtBuildResult {
  expr = unwrapParens(expr) as ts.Expression;
  // `c++` / `++c` / `c--` / `--c`
  if (ts.isPostfixUnaryExpression(expr) || ts.isPrefixUnaryExpression(expr)) {
    if (!ts.isIdentifier(expr.operand) || expr.operand.text !== counterName) {
      return { unsupported: "increment operand is not the counter" };
    }
    if (expr.operator === ts.SyntaxKind.PlusPlusToken) {
      return ir1Assign(
        ir1Var(counterName),
        ir1Binop("add", ir1Var(counterName), ir1LitNat(1)),
      );
    }
    if (expr.operator === ts.SyntaxKind.MinusMinusToken) {
      return ir1Assign(
        ir1Var(counterName),
        ir1Binop("sub", ir1Var(counterName), ir1LitNat(1)),
      );
    }
    return { unsupported: "unsupported unary increment operator" };
  }

  if (!ts.isBinaryExpression(expr)) {
    return { unsupported: "step is not an assignment or increment" };
  }

  const opKind = expr.operatorToken.kind;

  // Compound assignments: `c += k`, `c -= k`, `c *= k`, `c /= k`
  const compoundOp = compoundOpToBinop(opKind);
  if (compoundOp !== null) {
    if (!ts.isIdentifier(expr.left) || expr.left.text !== counterName) {
      return {
        unsupported: "compound-assignment target is not the counter",
      };
    }
    // Prefer native L1 for numeric-literal RHS so the μ-search recognizer
    // can pattern-match `BinOp(add, Var(c), Lit(1))` byte-equally across
    // all five `+1` spellings. Non-literal RHS falls back to the legacy
    // sub-expression pipeline via `buildSubExpr`.
    const litRhs = tryBuildL1IntegerLiteral(expr.right);
    const rhs = litRhs ?? buildSubExpr(expr.right, ctx);
    if (isL1Unsupported(rhs)) {
      return rhs;
    }
    return ir1Assign(
      ir1Var(counterName),
      ir1Binop(compoundOp, ir1Var(counterName), rhs),
    );
  }

  // Plain assignment: `c = c ⊕ k` or `c = k ⊕ c` (commutative ⊕ only).
  if (opKind === ts.SyntaxKind.EqualsToken) {
    if (!ts.isIdentifier(expr.left) || expr.left.text !== counterName) {
      return { unsupported: "assignment target is not the counter" };
    }
    if (!ts.isBinaryExpression(expr.right)) {
      return {
        unsupported: "assignment RHS is not a binary expression",
      };
    }
    const rhsOpKind = expr.right.operatorToken.kind;
    const explicitOp = explicitOpToBinop(rhsOpKind);
    if (explicitOp === null) {
      return {
        unsupported: "assignment RHS uses an unsupported binary operator",
      };
    }
    const a = expr.right.left;
    const b = expr.right.right;
    const aIsCounter = ts.isIdentifier(a) && a.text === counterName;
    const bIsCounter = ts.isIdentifier(b) && b.text === counterName;
    if (aIsCounter && !bIsCounter) {
      // `c = c ⊕ k`
      const litK = tryBuildL1IntegerLiteral(b);
      const k = litK ?? buildSubExpr(b, ctx);
      if (isL1Unsupported(k)) {
        return k;
      }
      return ir1Assign(
        ir1Var(counterName),
        ir1Binop(explicitOp, ir1Var(counterName), k),
      );
    }
    if (bIsCounter && !aIsCounter) {
      // `c = k ⊕ c` — only commutative ops.
      if (!isCommutativeBinop(explicitOp)) {
        return {
          unsupported:
            "non-commutative operator with counter on the right of RHS",
        };
      }
      const litK = tryBuildL1IntegerLiteral(a);
      const k = litK ?? buildSubExpr(a, ctx);
      if (isL1Unsupported(k)) {
        return k;
      }
      return ir1Assign(
        ir1Var(counterName),
        ir1Binop(explicitOp, ir1Var(counterName), k),
      );
    }
    return {
      unsupported: "assignment RHS does not reference counter exactly once",
    };
  }

  return { unsupported: "step shape is not a recognized increment form" };
}

/**
 * Build a native L1 integer-literal expression from a TS source.
 * Accepts a plain non-negative `NumericLiteral` and `-N` written as
 * `PrefixUnary(MinusToken, NumericLiteral)` (TS's representation of
 * negative integer literals). Returns null for anything else — strings,
 * booleans, identifier references, non-integer numerics — which the
 * caller handles via its own fallback.
 *
 * Critical for canonical-form matching: when the increment step is
 * `i += 1` or `i = i + 1`, the `1` must lower to a native L1
 * `LitNat(1)` so `isCanonicalMuSearchForm` can pattern-match
 * `BinOp(add, Var(c), Lit(1))`. A `from-l2`-wrapped literal would
 * defeat the canonical-shape check.
 */
function tryBuildL1IntegerLiteral(expr: ts.Expression): IR1Expr | null {
  if (
    ts.isPrefixUnaryExpression(expr) &&
    expr.operator === ts.SyntaxKind.MinusToken &&
    ts.isNumericLiteral(expr.operand)
  ) {
    const n = Number(expr.operand.text);
    if (Number.isFinite(n) && Number.isInteger(n)) {
      return ir1Unop("neg", ir1LitNat(n));
    }
    return null;
  }
  if (ts.isNumericLiteral(expr)) {
    const n = Number(expr.text);
    if (Number.isFinite(n) && Number.isInteger(n) && n >= 0) {
      return ir1LitNat(n);
    }
  }
  return null;
}

function compoundOpToBinop(kind: ts.SyntaxKind): IR1Binop | null {
  switch (kind) {
    case ts.SyntaxKind.PlusEqualsToken:
      return "add";
    case ts.SyntaxKind.MinusEqualsToken:
      return "sub";
    case ts.SyntaxKind.AsteriskEqualsToken:
      return "mul";
    case ts.SyntaxKind.SlashEqualsToken:
      return "div";
    default:
      return null;
  }
}

function explicitOpToBinop(kind: ts.SyntaxKind): IR1Binop | null {
  switch (kind) {
    case ts.SyntaxKind.PlusToken:
      return "add";
    case ts.SyntaxKind.MinusToken:
      return "sub";
    case ts.SyntaxKind.AsteriskToken:
      return "mul";
    case ts.SyntaxKind.SlashToken:
      return "div";
    default:
      return null;
  }
}

function isCommutativeBinop(op: IR1Binop): boolean {
  return op === "add" || op === "mul";
}

// ---------------------------------------------------------------------------
// μ-search L1 form construction (M2)
//
// Build the canonical L1 representation of a let+while+increment μ-search
// prelude pair: `Block([Let(c, init), While(p, <step-Assign>)])`. The step
// is built via `buildL1IncrementStep`. Sub-expressions (init, predicate)
// flow through the legacy `translateBodyExpr` pipeline and wrap as
// `from-l2` — same scoped delegation pattern M1 conditionals use.
//
// The TS-AST recognizer (`recognizeMuSearch` in `translate-body.ts`)
// has already validated structural acceptance (let-decl + while-stmt +
// expression-statement body) before this builder runs. Semantic
// recognition (canonical step shape, predicate references counter,
// strategy is discrete) lives in `ir1-lower.ts:isCanonicalMuSearchForm`.
// ---------------------------------------------------------------------------

/**
 * Build the L1 representation of a μ-search-shaped TS prelude:
 *
 *   `Block([Let(counter, init), While(predicate, step)])`
 *
 * `step` is the canonical L1 Assign produced by `buildL1IncrementStep`
 * — for the five `+1` spellings, it's
 * `Assign(Var(counter), BinOp(add, Var(counter), Lit(1)))`.
 */
export function buildL1LetWhile(
  mu: MuSearch,
  ctx: L1BuildContext,
): L1StmtBuildResult {
  // Structural sanity: a let+while pair where the while predicate
  // doesn't reference the let-bound counter is either a no-op or
  // a divergent loop. Reject at build time on the TS expression
  // (free-var walk is straightforward there); post-translation the
  // predicate is wrapped in `from-l2(irWrap(OpaqueExpr))` and the
  // equivalent check would need a wasm helper to introspect OpaqueExpr.
  if (
    !expressionReferencesNames(mu.predicateTsExpr, new Set([mu.counterName]))
  ) {
    return {
      unsupported: "while predicate does not reference the let-bound counter",
    };
  }
  const init = buildSubExpr(mu.initTsExpr, ctx);
  if (isL1Unsupported(init)) {
    return init;
  }
  const predicate = buildSubExpr(mu.predicateTsExpr, ctx);
  if (isL1Unsupported(predicate)) {
    return predicate;
  }
  const step = buildL1IncrementStep(mu.stepExpr, mu.counterName, ctx);
  if (isL1StmtUnsupported(step)) {
    return step;
  }
  return ir1Block([ir1Let(mu.counterName, init), ir1While(predicate, step)]);
}
