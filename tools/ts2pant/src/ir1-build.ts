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
import { type IRExpr, irWrap } from "./ir.js";
import { lowerExpr } from "./ir-emit.js";
import {
  type IR1Binop,
  type IR1Expr,
  type IR1Stmt,
  ir1App,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1Cond,
  ir1FromL2,
  ir1IsNullish,
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
  getOperandDeclaredType,
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
  toPantTermName,
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

function binaryOperatorToL1(kind: ts.SyntaxKind): IR1Binop | null {
  switch (kind) {
    case ts.SyntaxKind.EqualsEqualsEqualsToken:
      return "eq";
    case ts.SyntaxKind.ExclamationEqualsEqualsToken:
      return "neq";
    case ts.SyntaxKind.LessThanToken:
      return "lt";
    case ts.SyntaxKind.GreaterThanToken:
      return "gt";
    case ts.SyntaxKind.LessThanEqualsToken:
      return "le";
    case ts.SyntaxKind.GreaterThanEqualsToken:
      return "ge";
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

const MUTATING_ARRAY_METHODS = new Set([
  "copyWithin",
  "fill",
  "pop",
  "push",
  "reverse",
  "shift",
  "sort",
  "splice",
  "unshift",
]);

const ARRAY_CHAIN_METHODS = new Set(["filter", "map", "reduce", "reduceRight"]);

function callMemberName(
  callee: ts.Expression,
): { receiver: ts.Expression; methodName: string } | null {
  if (ts.isPropertyAccessExpression(callee)) {
    return {
      receiver: callee.expression,
      methodName: callee.name.text,
    };
  }
  if (ts.isElementAccessExpression(callee)) {
    const methodName = elementAccessLiteralKey(callee);
    if (methodName !== null) {
      return { receiver: callee.expression, methodName };
    }
  }
  return null;
}

function isKnownEffectfulNativeCall(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
): boolean {
  const member = callMemberName(expr.expression);
  if (member === null) {
    return false;
  }
  const receiverType = checker.getTypeAtLocation(member.receiver);
  if (
    (member.methodName === "add" ||
      member.methodName === "delete" ||
      member.methodName === "clear") &&
    isSetType(receiverType)
  ) {
    return true;
  }
  if (
    (member.methodName === "set" ||
      member.methodName === "delete" ||
      member.methodName === "clear") &&
    isMapType(receiverType)
  ) {
    return true;
  }
  return (
    isArrayOrTupleType(receiverType, checker) &&
    MUTATING_ARRAY_METHODS.has(member.methodName)
  );
}

function isArrayOrTupleType(t: ts.Type, checker: ts.TypeChecker): boolean {
  return checker.isArrayType(t) || checker.isTupleType(t);
}

function isArrayOrTupleUnionType(t: ts.Type, checker: ts.TypeChecker): boolean {
  if (isArrayOrTupleType(t, checker)) {
    return true;
  }
  return (
    t.isUnion() &&
    t.types.every((member) => isArrayOrTupleType(member, checker))
  );
}

function isArrayChainCall(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
): boolean {
  const member = callMemberName(expr.expression);
  if (member === null || !ARRAY_CHAIN_METHODS.has(member.methodName)) {
    return false;
  }
  const receiverType = checker.getTypeAtLocation(member.receiver);
  return isArrayOrTupleUnionType(receiverType, checker);
}

/**
 * Native L1 construction for ordinary pure sub-expressions. Returns
 * `null` when the expression is outside this cleanup patch's native
 * coverage and should continue to the temporary legacy safety net.
 * Explicitly unsupported boundaries still return `{ unsupported }`.
 */
export function tryBuildL1PureSubExpression(
  expr: ts.Expression,
  ctx: L1BuildContext,
): L1BuildResult | null {
  expr = unwrapParens(expr) as ts.Expression;

  if (expr.kind === ts.SyntaxKind.TrueKeyword) {
    return ir1LitBool(true);
  }
  if (expr.kind === ts.SyntaxKind.FalseKeyword) {
    return ir1LitBool(false);
  }
  if (ts.isNumericLiteral(expr)) {
    const n = Number(expr.text);
    if (Number.isFinite(n) && Number.isInteger(n) && n >= 0) {
      return ir1LitNat(n);
    }
    return null;
  }
  if (ts.isStringLiteral(expr) || ts.isNoSubstitutionTemplateLiteral(expr)) {
    return ir1LitString(expr.text);
  }
  if (ts.isIdentifier(expr)) {
    return ir1Var(ctx.paramNames.get(expr.text) ?? expr.text);
  }
  if (expr.kind === ts.SyntaxKind.ThisKeyword) {
    return ir1Var(ctx.paramNames.get("this") ?? "this");
  }

  if (
    (ts.isPropertyAccessExpression(expr) ||
      ts.isElementAccessExpression(expr)) &&
    (expr.flags & ts.NodeFlags.OptionalChain) === 0
  ) {
    if (
      ts.isPropertyAccessExpression(expr) ||
      elementAccessLiteralKey(expr) === "length" ||
      elementAccessLiteralKey(expr) === "size"
    ) {
      const card = tryBuildL1Cardinality(expr, ctx, {
        nativeReceiverLeaf: true,
      });
      if (card !== null) {
        return card;
      }
    }
    return buildL1MemberAccess(expr, ctx, { nativeReceiverLeaf: true });
  }

  if (ts.isPrefixUnaryExpression(expr)) {
    const op =
      expr.operator === ts.SyntaxKind.ExclamationToken
        ? "not"
        : expr.operator === ts.SyntaxKind.MinusToken
          ? "neg"
          : null;
    if (op === null) {
      return null;
    }
    const operand = tryBuildL1PureSubExpression(expr.operand, ctx);
    if (operand === null || isL1Unsupported(operand)) {
      return operand;
    }
    return ir1Unop(op, operand);
  }

  if (ts.isBinaryExpression(expr)) {
    const nullishProbe = recognizeNullishForm(expr, ctx.checker, () =>
      ir1Var("__nullish_probe"),
    );
    if (nullishProbe !== null) {
      return null;
    }
    if (
      expr.operatorToken.kind === ts.SyntaxKind.EqualsEqualsToken ||
      expr.operatorToken.kind === ts.SyntaxKind.ExclamationEqualsToken
    ) {
      return {
        unsupported: "loose equality (== / !=) is unsupported; use === / !==",
      };
    }
    if (expr.operatorToken.kind === ts.SyntaxKind.QuestionQuestionToken) {
      const left = tryBuildL1PureSubExpression(expr.left, ctx);
      if (left === null || isL1Unsupported(left)) {
        return left;
      }
      const leftTsType = getOperandDeclaredType(expr.left, ctx.checker);
      if (!isNullableTsType(leftTsType)) {
        return left;
      }
      const right = tryBuildL1PureSubExpression(expr.right, ctx);
      if (right === null || isL1Unsupported(right)) {
        return right;
      }
      const rightTsType = getOperandDeclaredType(expr.right, ctx.checker);
      const present = isNullableTsType(rightTsType)
        ? left
        : ir1App(left, [ir1LitNat(1)]);
      return ir1Cond([[ir1IsNullish(left), right]], present);
    }
    let op = binaryOperatorToL1(expr.operatorToken.kind);
    if (
      expr.operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken ||
      expr.operatorToken.kind === ts.SyntaxKind.BarBarToken
    ) {
      if (
        !isStaticallyBoolTyped(expr.left, ctx.checker) ||
        !isStaticallyBoolTyped(expr.right, ctx.checker)
      ) {
        return null;
      }
      op =
        expr.operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken
          ? "and"
          : "or";
    }
    if (op === null) {
      return null;
    }
    const left = tryBuildL1PureSubExpression(expr.left, ctx);
    if (left === null || isL1Unsupported(left)) {
      return left;
    }
    const right = tryBuildL1PureSubExpression(expr.right, ctx);
    if (right === null || isL1Unsupported(right)) {
      return right;
    }
    return ir1Binop(op, left, right);
  }

  if (ts.isCallExpression(expr)) {
    if (expr.arguments.some(ts.isSpreadElement)) {
      return { unsupported: "call with spread arguments is unsupported" };
    }
    if (expressionHasSideEffects(expr.expression, ctx.checker)) {
      return null;
    }
    if (isArrayChainCall(expr, ctx.checker)) {
      return null;
    }
    const member = callMemberName(expr.expression);
    if (
      isKnownEffectfulNativeCall(expr, ctx.checker) ||
      (member !== null &&
        expressionHasSideEffects(member.receiver, ctx.checker))
    ) {
      return null;
    }
    let callee: IR1Expr | null;
    let args: IR1Expr[];
    if (ts.isIdentifier(expr.expression)) {
      const fnName =
        ctx.paramNames.get(expr.expression.text) ?? expr.expression.text;
      callee = ir1Var(fnName);
      args = [];
    } else if (
      ts.isPropertyAccessExpression(expr.expression) ||
      ts.isElementAccessExpression(expr.expression)
    ) {
      const builtCallee = buildL1MemberAccess(expr.expression, ctx, {
        nativeReceiverLeaf: true,
      });
      if (isL1Unsupported(builtCallee)) {
        return builtCallee;
      }
      callee = builtCallee;
      args = [];
    } else {
      const builtCallee = tryBuildL1PureSubExpression(expr.expression, ctx);
      if (builtCallee === null || isL1Unsupported(builtCallee)) {
        return builtCallee;
      }
      callee = builtCallee;
      args = [];
    }
    for (const arg of expr.arguments) {
      const builtArg = tryBuildL1PureSubExpression(arg, ctx);
      if (builtArg === null || isL1Unsupported(builtArg)) {
        return builtArg;
      }
      args.push(builtArg);
    }
    return ir1App(callee, args);
  }

  return null;
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
 * non-optional property access (`xs.length` or `xs["length"]`) whose
 * receiver is a list-shaped TS type and whose property is the cardinality
 * slot for that type. Returns null when the node is not a cardinality
 * form — caller falls through to `buildL1MemberAccess`.
 *
 * `options` propagate through to the inner Member dispatch when the
 * cardinality receiver is itself a property access (e.g.,
 * `a.items.length`) and to the leaf-translator hook for non-property
 * receivers. Signature-path callers pass the same options here that
 * they pass to `buildL1MemberAccess` so cardinality and Member share
 * the signature-style receiver leaf and ambiguous-owner fallback.
 */
export function tryBuildL1Cardinality(
  node: ts.Expression,
  ctx: L1BuildContext,
  options: BuildL1MemberAccessOptions = {},
): IR1Expr | null {
  const stripped = unwrapParens(node);
  if (
    !ts.isPropertyAccessExpression(stripped) &&
    !ts.isElementAccessExpression(stripped)
  ) {
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
  const propName = ts.isPropertyAccessExpression(stripped)
    ? stripped.name.text
    : elementAccessLiteralKey(stripped);
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
  // Receiver translation. Member-surface receivers (`a.items.length`,
  // `a["items"].length`) route through `buildL1MemberAccess` so the
  // entire chain stays on the L1 path — no half-migration where the
  // outer `card` is L1 but the inner `.items` / `["items"]` round-trips
  // through L2 via `from-l2`. Both PropertyAccess and string-literal
  // ElementAccess belong to the property-access equivalence class
  // (M5 hard rule), so the recursion gate is the shared
  // `isMemberSurfaceForm` predicate. Other receiver shapes
  // (Identifier, call, binop, etc.) translate through the supplied
  // leaf translator (default `translateBodyExpr`); those aren't
  // property-access and are out of M5's equivalence class.
  if (isMemberSurfaceForm(receiverNode)) {
    const innerCard = tryBuildL1Cardinality(receiverNode, ctx, options);
    if (innerCard !== null) {
      return ir1Unop("card", innerCard);
    }
    const inner = buildL1MemberAccess(receiverNode, ctx, options);
    if (isL1Unsupported(inner)) {
      return null;
    }
    return ir1Unop("card", inner);
  }
  if (options.translateReceiverLeaf !== undefined) {
    const r = options.translateReceiverLeaf(receiverNode as ts.Expression, ctx);
    if (r.kind === "unsupported") {
      return null;
    }
    return ir1Unop("card", ir1FromL2(irWrap(r.expr)));
  }
  if (ctx.state === undefined && options.nativeReceiverLeaf === true) {
    const nativeReceiver = tryBuildL1PureSubExpression(
      receiverNode as ts.Expression,
      ctx,
    );
    if (nativeReceiver !== null && !isL1Unsupported(nativeReceiver)) {
      return ir1Unop("card", nativeReceiver);
    }
    const nativeReceiverIR = tryBuildNativeReceiverLeafIR(
      receiverNode as ts.Expression,
      ctx,
      options,
    );
    if (nativeReceiverIR !== null && !isL1Unsupported(nativeReceiverIR)) {
      return ir1Unop("card", nativeReceiverIR);
    }
  }
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
// `PropertyAccessExpression` or string-literal `ElementAccessExpression`
// (`obj["field"]`). The two surface forms collapse to the same canonical
// Member — one canonical form per construct (IRSC discipline). The
// qualified name is resolved at build time via `qualifyFieldAccess`:
// L1 `Member` is a *normalization* form whose lowering at `ir1-lower.ts`
// is mechanical (`Member(r, n) → App(n, [lower r])`), matching the
// legacy `App(qualified-rule, [receiver])` byte-for-byte.

/**
 * Computed `obj[expr]` (non-literal index) is rejected unconditionally.
 * The DoD's "unless the type system resolves to a known field set" path
 * (literal-union narrowing) is deferred to a follow-up; uniform
 * rejection is the conservative-refusal policy 3(b) answer.
 */
const COMPUTED_ELEMENT_ACCESS_UNSUPPORTED =
  "computed property access (obj[expr]) is unsupported; " +
  "use dotted property access or a string-literal index instead";

export const computedElementAccessUnsupportedReason =
  COMPUTED_ELEMENT_ACCESS_UNSUPPORTED;

/**
 * Extract the literal field name from an `ElementAccessExpression`'s
 * argument expression. Accepts both `StringLiteral` (`obj["f"]`) and
 * `NoSubstitutionTemplateLiteral` (`` obj[`f`] ``) — both produce the
 * same TS type-checker resolution and the same field name. Returns
 * null for any other argument shape (identifier, computed expression,
 * numeric literal, template with substitutions, etc.).
 */
export function elementAccessLiteralKey(
  node: ts.ElementAccessExpression,
): string | null {
  const arg = node.argumentExpression;
  if (ts.isStringLiteral(arg) || ts.isNoSubstitutionTemplateLiteral(arg)) {
    return arg.text;
  }
  return null;
}

/**
 * True when `node` is one of the two surface forms that
 * `buildL1MemberAccess` recognizes as a non-optional property access:
 * dotted `obj.field` or string-literal `obj["field"]`. Used by the
 * recursion gate so nested chains compose into nested Member trees
 * regardless of which surface form spells each step.
 */
function isMemberSurfaceForm(
  node: ts.Node,
): node is ts.PropertyAccessExpression | ts.ElementAccessExpression {
  if (
    ts.isPropertyAccessExpression(node) &&
    (node.flags & ts.NodeFlags.OptionalChain) === 0
  ) {
    return true;
  }
  if (
    ts.isElementAccessExpression(node) &&
    (node.flags & ts.NodeFlags.OptionalChain) === 0
  ) {
    return elementAccessLiteralKey(node) !== null;
  }
  return false;
}

/**
 * Receiver translation result — either an opaque expression or an
 * upstream-rejection marker. Mirrors the convention of
 * `TranslateExprResult` and the body-side `BodyResult` so signature
 * and body translators can plug into `buildL1MemberAccess` through a
 * single callback shape.
 */
export type MemberReceiverResult =
  | { kind: "expr"; expr: OpaqueExpr }
  | { kind: "unsupported"; reason: string };

/**
 * Options threaded through `buildL1MemberAccess`.
 *
 * - `ambiguousOwnerFallback: "reject"` — body-side default. Ambiguous
 *   union/intersection owners surface as `{ unsupported }` because
 *   the resulting accessor rule is load-bearing in emitted
 *   propositions.
 * - `ambiguousOwnerFallback: "bare-kebab"` — signature-side
 *   best-effort. Ambiguous owners produce a Member with the bare
 *   kebab'd field name. The signature path uses this in
 *   `extractAssertionGuard` / `buildSubstitutionMap` /
 *   `tryTranslateGuardExpr` where `translateExpr` failures bail the
 *   *optional* analysis (the would-be guard never fires) — so a bare
 *   fallback yields the same observable behavior as a hard reject
 *   without preventing other guards in the same function from
 *   extracting cleanly. Mirrors the asymmetry the deleted local
 *   `qualifyFieldAccess` in `translate-signature.ts` carried.
 *
 * - `translateReceiverLeaf` — translator for the non-property leaf at
 *   the bottom of the receiver chain (e.g., the `a` in `a.b.c.field`).
 *   Defaults to `translateBodyExpr`, which is what body-position and
 *   pure-path call sites want. The signature path passes a callback
 *   that routes through `translateExpr` (signature) so the leaf gets
 *   the same transparent-wrapper handling and signature-only operator
 *   surface (loose-eq rejection, no body-only chain fusion or
 *   Map/Set effect handling) as the rest of guard analysis.
 *
 *   The callback's input is already paren-stripped via `unwrapParens`;
 *   it is responsible for any further unwrapping (`as T`, `!`, etc.)
 *   that its respective layer applies.
 */
export interface BuildL1MemberAccessOptions {
  ambiguousOwnerFallback?: "reject" | "bare-kebab";
  nativeReceiverLeaf?: boolean;
  nativeReceiverLeafIR?: (
    expr: ts.Expression,
    ctx: L1BuildContext,
  ) => IRExpr | { unsupported: string } | null;
  translateReceiverLeaf?: (
    expr: ts.Expression,
    ctx: L1BuildContext,
  ) => MemberReceiverResult;
}

function tryBuildNativeReceiverLeafIR(
  receiverNode: ts.Expression,
  ctx: L1BuildContext,
  options: BuildL1MemberAccessOptions,
): IR1Expr | { unsupported: string } | null {
  if (
    ctx.state !== undefined ||
    options.nativeReceiverLeaf !== true ||
    options.nativeReceiverLeafIR === undefined ||
    !ts.isCallExpression(receiverNode) ||
    !isArrayChainCall(receiverNode, ctx.checker)
  ) {
    return null;
  }
  const nativeReceiver = options.nativeReceiverLeafIR(receiverNode, ctx);
  if (nativeReceiver === null) {
    return null;
  }
  if ("unsupported" in nativeReceiver) {
    return nativeReceiver;
  }
  return ir1FromL2(nativeReceiver);
}

/**
 * Build an L1 `Member(receiverL1, qualifiedName)` for a TS property
 * access. Returns an `{ unsupported }` descriptor on ambiguous owner
 * (under the body-side default), unsupported access shape, or a
 * sub-expression that itself rejects.
 *
 * Surface forms accepted (one canonical Member output for both):
 *   - `PropertyAccessExpression` — `obj.field` (with `.field` token).
 *   - `ElementAccessExpression` whose `argumentExpression` is a
 *     `StringLiteral` or `NoSubstitutionTemplateLiteral` —
 *     `obj["field"]` and `` obj[`field`] ``. Computed indices
 *     (`obj[k]`, `obj[1+1]`) reject with the
 *     `COMPUTED_ELEMENT_ACCESS_UNSUPPORTED` reason.
 *
 * Receiver translation:
 *   - State-free contexts (pure path, unit tests) recurse on
 *     property-access receivers (either surface form), producing
 *     nested L1 `Member` trees. The recursion bottoms out at a
 *     non-property receiver translated through the legacy pipeline
 *     and wrapped as `from-l2`.
 *   - State-bearing contexts (mutating-body path) translate the
 *     receiver via the legacy pipeline so symbolic-state lookup at
 *     each nested level fires identically to the pre-M5 path. This
 *     preserves byte-equality with the legacy snapshots while still
 *     routing the outer access through L1 `Member`.
 */
export function buildL1MemberAccess(
  node: ts.PropertyAccessExpression | ts.ElementAccessExpression,
  ctx: L1BuildContext,
  options: BuildL1MemberAccessOptions = {},
): L1BuildResult {
  const ambiguousMode = options.ambiguousOwnerFallback ?? "reject";
  const stripped = unwrapParens(node);
  let receiverNode: ts.Node;
  let fieldName: string;
  if (ts.isPropertyAccessExpression(stripped)) {
    // Optional-chain `.f` (`x?.f` and the chain tail) routes through
    // the optional-chain functor-lift recognizer, not Member. Caller is
    // expected to filter optional chains before invoking this helper.
    if ((stripped.flags & ts.NodeFlags.OptionalChain) !== 0) {
      return {
        unsupported:
          "optional-chain property access uses functor-lift, not Member",
      };
    }
    receiverNode = unwrapParens(stripped.expression);
    fieldName = stripped.name.text;
  } else if (ts.isElementAccessExpression(stripped)) {
    if ((stripped.flags & ts.NodeFlags.OptionalChain) !== 0) {
      return {
        unsupported:
          "optional-chain element access uses functor-lift, not Member",
      };
    }
    const literalKey = elementAccessLiteralKey(stripped);
    if (literalKey === null) {
      return { unsupported: COMPUTED_ELEMENT_ACCESS_UNSUPPORTED };
    }
    receiverNode = unwrapParens(stripped.expression);
    fieldName = literalKey;
  } else {
    return {
      unsupported: "buildL1MemberAccess: unsupported access shape",
    };
  }
  const receiverType = ctx.checker.getTypeAtLocation(receiverNode);
  const qualifiedStrict = qualifyFieldAccess(
    receiverType,
    fieldName,
    ctx.checker,
    ctx.strategy,
    ctx.supply.synthCell,
  );
  let qualified: string;
  if (qualifiedStrict !== null) {
    qualified = qualifiedStrict;
  } else if (ambiguousMode === "bare-kebab") {
    // Signature-side best-effort: ambiguous union/intersection
    // owners fall back to the bare kebab'd field name so optional
    // analyses (guard extraction, call-following) can continue past
    // an inherently-ambiguous accessor instead of bailing the whole
    // analysis on a single ambiguous read.
    qualified = toPantTermName(fieldName);
  } else {
    return {
      unsupported:
        `property access .${fieldName}: ambiguous owner — ` +
        `union/intersection members declare this field on multiple distinct ` +
        `types, so no single qualified accessor rule applies`,
    };
  }

  let receiverL1: IR1Expr;
  if (ctx.state === undefined && isMemberSurfaceForm(receiverNode)) {
    // Pure path: prefer canonical nested Member trees. Cardinality
    // dispatch fires first so a chain like `arr.length.foo` wouldn't
    // silently route `arr.length` through Member.
    const card = tryBuildL1Cardinality(receiverNode, ctx, options);
    if (card !== null) {
      receiverL1 = card;
    } else {
      const inner = buildL1MemberAccess(receiverNode, ctx, options);
      if (isL1Unsupported(inner)) {
        return inner;
      }
      receiverL1 = inner;
    }
  } else if (options.translateReceiverLeaf !== undefined) {
    // Caller-supplied leaf translator (used by the signature path so
    // the non-property leaf goes through `translateExpr` instead of
    // `translateBodyExpr` — preserves signature-only operator surface
    // like loose-eq rejection and avoids body-only branches like
    // chain fusion / Map-Set effect handling that aren't appropriate
    // in guard analysis).
    const r = options.translateReceiverLeaf(receiverNode as ts.Expression, ctx);
    if (r.kind === "unsupported") {
      return { unsupported: r.reason };
    }
    receiverL1 = ir1FromL2(irWrap(r.expr));
  } else if (ctx.state === undefined && options.nativeReceiverLeaf === true) {
    const nativeReceiver = tryBuildL1PureSubExpression(
      receiverNode as ts.Expression,
      ctx,
    );
    if (nativeReceiver !== null) {
      if (isL1Unsupported(nativeReceiver)) {
        return nativeReceiver;
      }
      receiverL1 = nativeReceiver;
    } else {
      const nativeReceiverIR = tryBuildNativeReceiverLeafIR(
        receiverNode as ts.Expression,
        ctx,
        options,
      );
      if (nativeReceiverIR !== null) {
        if (isL1Unsupported(nativeReceiverIR)) {
          return nativeReceiverIR;
        }
        receiverL1 = nativeReceiverIR;
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
  // Mirror `buildL1Conditional`'s entry-point paren-strip so a
  // parenthesized conditional (`(c ? a : b)`, `(g && h)`) classifies
  // identically to its unwrapped form. Body-side callers already
  // pre-strip via `unwrapExpression`, but defensive normalization
  // keeps the recognizer self-contained for any future caller.
  node = unwrapParens(node);
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
 * True when `expr` is a single-element-producing shape: not an array
 * literal, not a spread, and not a known multi-producing array method
 * (`.concat`, `.flat`, `.flatMap`, `.filter`, `.map`, `.slice`,
 * `.splice`). The "references the operand" half of the eligibility
 * check is operand-kind specific and lives at the recognizer level —
 * `Var` operands use `expressionReferencesNames` against the operand
 * text; `Member` operands defer to the L1 substitution-fired check.
 */
function isSingleElementProducingShape(expr: ts.Expression): boolean {
  const u = unwrapTransparentExpression(expr);
  if (ts.isArrayLiteralExpression(u)) {
    return false;
  }
  if (ts.isSpreadElement(u)) {
    return false;
  }
  if (ts.isCallExpression(u)) {
    const member = callMemberName(u.expression);
    if (
      member !== null &&
      ARRAY_MULTI_PRODUCING_METHODS.has(member.methodName)
    ) {
      return false;
    }
  }
  return true;
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

// ---------------------------------------------------------------------------
// Operand-substitution rule for the functor-lift recognizer
// ---------------------------------------------------------------------------
//
// **Standard name:** First-order term rewriting / common-subexpression
// abstraction. The lift transforms
//   `if e == null then [] else [body]` → `each $n in e | body[e := $n]`
// where `body[e := $n]` is the substitution of every syntactic
// occurrence of the operand subterm `e` with the fresh binder `$n`.
// The functor lift itself is structurally `Maybe` `fmap` — see
// CLAUDE.md § "Functor-Lift Recognizer" (Wadler POPL 1992, Hutton &
// Meijer JFP 1998). The substitution rule below is the term-rewriting
// half of that transformation.
//
// **Reference:** Baader & Nipkow, *Term Rewriting and All That*
// (Cambridge 1998), ch. 2 (basic notions of position, subterm, and
// substitution); Peyton Jones & Marlow, "Secrets of the GHC Inliner"
// JFP 2002 (hygienic-binder discipline / Barendregt convention).
//
// **Invariants justifying soundness:**
//
// 1. **Hygiene.** The replacement is a fresh `Var($n)` allocated via
//    `cellRegisterName` against the document-wide `NameRegistry`, so
//    the binder cannot collide with any other name in scope. This is
//    the Barendregt convention applied to the comprehension binder.
//
// 2. **Structural reachability.** Soundness requires that the operand
//    `e` actually occurs as a syntactic subterm of `body` — otherwise
//    the comprehension would emit a body with `$n` as a free variable
//    (substitution wouldn't fire) or, worse, with the operand still
//    referenced free of the binder. The recognizer enforces this by
//    gating on the explicit `changed` flag returned by
//    `substituteL1Subtree` for Member operands; for Var operands, the
//    TS-AST walk (`expressionReferencesNames`) is the precondition and
//    post-lowering `ast.substituteBinder` is the substitution
//    primitive.
//
// 3. **Referential transparency at the operand position.** The
//    comprehension evaluates `e` once at its source position
//    (`each $n in e | …`); each occurrence of `e` inside `body` is
//    replaced by the binder. This is sound iff `e` has no observable
//    effects whose duplication or removal would change semantics.
//    M4 P5's eligibility check enforces this implicitly — list-
//    lifted nullable types under TypeScript don't admit side effects
//    in the comparison position the recognizer matches.
//
// 4. **Closed form on Var/Member.** `structuralEqualL1` only compares
//    `Var` and `Member` shapes. The pure-L1 builder
//    `buildL1MemberOrVarForLift` produces only those shapes, so
//    operand and projection trees are exhaustively comparable along
//    the Member-operand path. For `from-l2` and other compound forms
//    the walker descends but the structural-match leaves never fire
//    for a Member needle, falling out via `changed: false`.
//
// Future changes to this rewrite should re-verify these four
// invariants. Adding new comparable forms (e.g., to support `App` as
// an eligible operand) requires extending both `structuralEqualL1`
// and the `buildL1MemberOrVarForLift` accept-set together; partial
// extension breaks invariant 4.

/**
 * Structural equality on L1 expressions, scoped to the shapes the
 * functor-lift recognizer compares: `Var` (name + primed) and `Member`
 * (name + receiver, recursing). Other forms compare false — the
 * recognizer's eligibility check builds operand and projection through
 * a pure-L1 path that produces only Var/Member trees, so any other
 * form is a structural mismatch by construction. Parens and other
 * source-level wrappers are normalized away by L1 build before this
 * function ever sees the trees.
 *
 * See the comment block above for the underlying term-rewriting
 * reference and the four soundness invariants this comparator
 * supports.
 */
function structuralEqualL1(a: IR1Expr, b: IR1Expr): boolean {
  if (a === b) {
    return true;
  }
  if (a.kind !== b.kind) {
    return false;
  }
  if (a.kind === "var" && b.kind === "var") {
    return a.name === b.name && (a.primed ?? false) === (b.primed ?? false);
  }
  if (a.kind === "member" && b.kind === "member") {
    return a.name === b.name && structuralEqualL1(a.receiver, b.receiver);
  }
  return false;
}

/**
 * Result of an `substituteL1Subtree` walk. `changed` reports whether
 * any subtree structurally matched `needle` and was replaced. The
 * Member-operand functor-lift path uses this to detect "operand not
 * structurally referenced" — a reference-equality check on the
 * returned tree is unreliable because the walker reconstructs every
 * compound node it traverses (e.g., `ir1Member(rec, name)`) regardless
 * of whether the recursive sub-call substituted. A projection like
 * `v.next.name` against operand `u.next` would otherwise rebuild a
 * fresh top-level Member tree even though no substitution fired,
 * letting projections that don't reference the operand silently emit
 * a comprehension body with a free variable.
 */
interface L1SubstResult {
  result: IR1Expr;
  changed: boolean;
}

/**
 * Replace every subtree of `root` whose L1 form structurally equals
 * `needle` with `replacement`. The walker recurses through every L1
 * form but does not enter `from-l2` wraps — those expose only an
 * opaque L2 tree to L1 callers, which the L1 walker has no handle on.
 * Returns the rewritten tree plus a `changed` flag indicating whether
 * substitution actually fired anywhere along the walk.
 *
 * This is the term-rewriting primitive that backs the functor-lift's
 * `body[e := $n]` substitution — see the comment block above
 * `structuralEqualL1` for the algorithm reference (Baader & Nipkow ch. 2)
 * and the four soundness invariants it depends on.
 *
 * Used by the functor-lift recognizer: the operand's L1 form (a `Var`
 * or `Member` chain) is replaced by a fresh `Var(binder)` inside the
 * projection's L1 form, so the comprehension body references the
 * binder rather than the operand. For `Var` operands, post-lowering
 * `ast.substituteBinder` catches references buried inside `from-l2`
 * wraps; for `Member` operands, the projection must be in pure L1 for
 * substitution to fire (no equivalent OpaqueExpr-level substitution
 * exists for arbitrary subtrees).
 */
function substituteL1Subtree(
  root: IR1Expr,
  needle: IR1Expr,
  replacement: IR1Expr,
): L1SubstResult {
  if (structuralEqualL1(root, needle)) {
    return { result: replacement, changed: true };
  }
  switch (root.kind) {
    case "var":
    case "lit":
    case "from-l2":
      return { result: root, changed: false };
    case "binop": {
      const lhs = substituteL1Subtree(root.lhs, needle, replacement);
      const rhs = substituteL1Subtree(root.rhs, needle, replacement);
      const changed = lhs.changed || rhs.changed;
      return changed
        ? { result: ir1Binop(root.op, lhs.result, rhs.result), changed: true }
        : { result: root, changed: false };
    }
    case "unop": {
      const arg = substituteL1Subtree(root.arg, needle, replacement);
      return arg.changed
        ? { result: ir1Unop(root.op, arg.result), changed: true }
        : { result: root, changed: false };
    }
    case "app": {
      const callee = substituteL1Subtree(root.callee, needle, replacement);
      const args = root.args.map((a) =>
        substituteL1Subtree(a, needle, replacement),
      );
      const changed = callee.changed || args.some((a) => a.changed);
      return changed
        ? {
            result: {
              kind: "app",
              callee: callee.result,
              args: args.map((a) => a.result),
            },
            changed: true,
          }
        : { result: root, changed: false };
    }
    case "member": {
      const recv = substituteL1Subtree(root.receiver, needle, replacement);
      return recv.changed
        ? { result: ir1Member(recv.result, root.name), changed: true }
        : { result: root, changed: false };
    }
    case "cond": {
      const armsRewritten = root.arms.map(([g, v]) => {
        const gr = substituteL1Subtree(g, needle, replacement);
        const vr = substituteL1Subtree(v, needle, replacement);
        return { gr, vr } as const;
      });
      const otherwise = substituteL1Subtree(
        root.otherwise,
        needle,
        replacement,
      );
      const changed =
        otherwise.changed ||
        armsRewritten.some(({ gr, vr }) => gr.changed || vr.changed);
      if (!changed) {
        return { result: root, changed: false };
      }
      const newArms = armsRewritten.map(
        ({ gr, vr }) => [gr.result, vr.result] as const,
      );
      return {
        result: ir1Cond(
          newArms as [
            readonly [IR1Expr, IR1Expr],
            ...ReadonlyArray<readonly [IR1Expr, IR1Expr]>,
          ],
          otherwise.result,
        ),
        changed: true,
      };
    }
    case "is-nullish": {
      const operand = substituteL1Subtree(root.operand, needle, replacement);
      return operand.changed
        ? { result: ir1IsNullish(operand.result), changed: true }
        : { result: root, changed: false };
    }
    default: {
      const _exhaustive: never = root;
      void _exhaustive;
      return { result: root, changed: false };
    }
  }
}

/**
 * Build a pure-L1 representation of `node` for the functor-lift
 * recognizer. Accepts only `Identifier` (→ `Var`) and member surface
 * forms (`PropertyAccessExpression` / string-literal
 * `ElementAccessExpression`, → `Member`); recursion bottoms out at a
 * bare `Identifier`. Returns `null` for any other shape (calls,
 * binops, optional chains, computed element access, ambiguous owner,
 * etc.).
 *
 * Distinct from `buildL1MemberAccess`: the production helper falls
 * back to `translateBodyExpr` for non-member receivers, producing
 * `from-l2` wraps that the L1 walker can't see into. The lift's
 * substitution requires the operand's structure to be visible at the
 * L1 level, so this builder either produces a fully-native chain or
 * rejects.
 *
 * Identifier names are looked up against `paramNames` to match the
 * Pant name that `translateExpr` emits — for parameters,
 * `paramNames.get(text)` (sanitized at signature translation via
 * `toPantTermName` + `cellRegisterName`); for bare locals/captures,
 * the raw TS text. The lowered operand and the lowered projection
 * must reference the same Pant name for the substitution to fire.
 */
function buildL1MemberOrVarForLift(
  node: ts.Expression,
  ctx: L1BuildContext,
): IR1Expr | null {
  // Strip the same set of transparent wrappers the recognizer's
  // outermost operand uses (`unwrapTransparentExpression`: parens +
  // `as` + `!` + `satisfies`). Stripping inside the chain keeps
  // member-lift eligibility driven by the operand's L1 shape rather
  // than re-introducing TS-syntax sensitivity for wrappers the
  // recognizer already treats as semantically neutral at the
  // boundary. `qualifyFieldAccess` is still given the *unstripped*
  // receiver via `getTypeAtLocation`, so a user's `as T` cast
  // continues to drive the type-checker resolution that picks the
  // qualified rule name.
  const stripped = unwrapTransparentExpression(node);
  if (ts.isIdentifier(stripped)) {
    const pantName = ctx.paramNames.get(stripped.text) ?? stripped.text;
    return ir1Var(pantName);
  }
  if (
    ts.isPropertyAccessExpression(stripped) &&
    (stripped.flags & ts.NodeFlags.OptionalChain) === 0
  ) {
    // For `getTypeAtLocation`, use the parens-stripped (but type-
    // erasure-preserving) receiver so the user's `as T` cast still
    // drives qualifier resolution. For the recursive L1 build, strip
    // wrappers via `unwrapTransparentExpression` so the chain
    // composes regardless of inner wrappers.
    const receiverForType = unwrapParens(stripped.expression) as ts.Expression;
    const receiverForRecurse = unwrapTransparentExpression(receiverForType);
    const receiverL1 = buildL1MemberOrVarForLift(receiverForRecurse, ctx);
    if (receiverL1 === null) {
      return null;
    }
    const receiverType = ctx.checker.getTypeAtLocation(receiverForType);
    const qualified = qualifyFieldAccess(
      receiverType,
      stripped.name.text,
      ctx.checker,
      ctx.strategy,
      ctx.supply.synthCell,
    );
    if (qualified === null) {
      return null;
    }
    return ir1Member(receiverL1, qualified);
  }
  if (
    ts.isElementAccessExpression(stripped) &&
    (stripped.flags & ts.NodeFlags.OptionalChain) === 0
  ) {
    const fieldName = elementAccessLiteralKey(stripped);
    if (fieldName === null) {
      return null;
    }
    const receiverForType = unwrapParens(stripped.expression) as ts.Expression;
    const receiverForRecurse = unwrapTransparentExpression(receiverForType);
    const receiverL1 = buildL1MemberOrVarForLift(receiverForRecurse, ctx);
    if (receiverL1 === null) {
      return null;
    }
    const receiverType = ctx.checker.getTypeAtLocation(receiverForType);
    const qualified = qualifyFieldAccess(
      receiverType,
      fieldName,
      ctx.checker,
      ctx.strategy,
      ctx.supply.synthCell,
    );
    if (qualified === null) {
      return null;
    }
    return ir1Member(receiverL1, qualified);
  }
  return null;
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
 * Operand eligibility (M5 P4): the operand's L1 form must be `Var`
 * (a simple `Identifier`) or `Member` (a `PropertyAccessExpression`
 * or string-literal `ElementAccessExpression` chain bottoming out at
 * a `Var`). Anything else — calls, binops, computed indices, optional
 * chains — falls through to the standard L1 Cond build. Transparent
 * wrappers (parens, `as`, `!`, `satisfies`) are normalized away by
 * `unwrapTransparentExpression` at every level of the chain build,
 * so eligibility stays driven by the operand's L1 shape rather than
 * its TS-AST spelling. Type-checker resolution for qualified rule
 * names still consults the unstripped (cast-bearing) receiver, so a
 * user's `as T` continues to drive qualifier picks.
 *
 * Substitution strategy. For `Var` operands the projection translates
 * through the legacy pipeline (`buildSubExpr`, producing a `from-l2`
 * wrap) and post-lowering `ast.substituteBinder` substitutes the
 * operand's Pant name with the fresh comprehension binder — exactly
 * the M4 P5 flow, preserved unchanged. For `Member` operands the
 * projection must be a pure-L1 chain (built via
 * `buildL1MemberOrVarForLift`); the L1 rewriter
 * `substituteL1Subtree` then replaces operand-shape subtrees with the
 * binder. `ast.substituteBinder` substitutes only by name and cannot
 * target arbitrary subtrees, so a Member-operand projection that
 * isn't pure L1 (e.g., a method call wrapping the operand) falls
 * through.
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

  // M5 P4: classify operand by its L1 shape (Var or Member).
  // `unwrapTransparentExpression` strips type-erasure wrappers (parens,
  // `as`, `!`, `satisfies`) from the outermost operand for the same
  // reason M4 P5 did — those wrappers are type-level only and the
  // runtime value is the inner expression. The recursive Member-chain
  // build inside `buildL1MemberOrVarForLift` strips the same set of
  // wrappers at every level so eligibility for the lift is driven by
  // the operand's L1 shape, not its TS-AST spelling.
  const operandNode = unwrapTransparentExpression(leaf.operand);

  // Snapshot supply counter and synth-cell state. Both the operand
  // build (Member operand path → `qualifyFieldAccess` → anonymous-
  // record synthesis can mutate `synthCell.recordSynth` /
  // `synthCell.registry`) and the projection build (`buildSubExpr` for
  // Var operand can advance `supply.n` via deferred-comprehension
  // allocations) are reversible at this granularity: the inner
  // synth-cell records are immutable, so saving the three field
  // references and restoring them on failure undoes any registrations
  // accumulated along the rejected path. Without this, eligibility-
  // failure paths leak anonymous-record domains into the document
  // even when the lift never fires.
  const supplyCounterSnapshot = ctx.supply.n;
  const synthCell = ctx.supply.synthCell;
  const synthSnapshot = synthCell
    ? {
        synth: synthCell.synth,
        recordSynth: synthCell.recordSynth,
        registry: synthCell.registry,
      }
    : null;
  const restore = (): null => {
    ctx.supply.n = supplyCounterSnapshot;
    if (synthCell && synthSnapshot) {
      synthCell.synth = synthSnapshot.synth;
      synthCell.recordSynth = synthSnapshot.recordSynth;
      synthCell.registry = synthSnapshot.registry;
    }
    return null;
  };

  let operandText: string | null = null;
  let operandL1: IR1Expr;
  if (ts.isIdentifier(operandNode)) {
    operandText = operandNode.text;
    const pantName = ctx.paramNames.get(operandText) ?? operandText;
    operandL1 = ir1Var(pantName);
  } else {
    const memberOperand = buildL1MemberOrVarForLift(operandNode, ctx);
    if (memberOperand === null || memberOperand.kind !== "member") {
      return restore();
    }
    operandL1 = memberOperand;
  }

  // Polarity: a positive nullish guard (`x === null`) means the
  // then-branch is the empty side; a negated guard (`x !== null`)
  // means the then-branch is the present side.
  const emptyExpr = leaf.negated ? cand.elseExpr : cand.thenExpr;
  const presentExpr = leaf.negated ? cand.thenExpr : cand.elseExpr;

  // (b) Empty side is empty-equivalent.
  if (!isEmptyEquivalent(emptyExpr, ctx.checker)) {
    return restore();
  }

  // (c) Present side, after stripping a singleton array wrapper, is
  //     not a multi-element-producing shape and references the operand.
  const projection = unwrapSingletonArray(presentExpr);
  if (!isSingleElementProducingShape(projection)) {
    return restore();
  }
  if (operandText !== null) {
    // Var operand: TS-AST text-walk catches references at any depth,
    // including those buried inside non-Member sub-expressions
    // (calls, binops, nested ternaries). The post-lower
    // `ast.substituteBinder` will pick up the same references.
    if (
      !expressionReferencesNames(
        unwrapTransparentExpression(projection),
        new Set([operandText]),
      )
    ) {
      return restore();
    }
  }
  // For Member operand, the L1 substitution-fired check below decides.

  // (d) Result type is list-lifted.
  if (!isListLiftedAtNode(cand.contextNode, ctx.checker)) {
    return restore();
  }

  // Build the projection. Path differs by operand kind:
  //   - Var: standard `buildSubExpr`. The projection comes back as
  //     `from-l2(opaque)`; post-lower `ast.substituteBinder`
  //     substitutes Var-name references at the OpaqueExpr level. This
  //     is the M4 P5 flow.
  //   - Member: pure-L1 chain via `buildL1MemberOrVarForLift`. The L1
  //     rewriter then walks the projection and replaces operand-shape
  //     subtrees with the comprehension binder. A non-pure-L1
  //     projection (e.g., a method call wrapping the operand)
  //     rejects.
  let projectionL1: IR1Expr;
  if (operandL1.kind === "var") {
    const sub = buildSubExpr(projection, ctx);
    if (isL1Unsupported(sub)) {
      return restore();
    }
    projectionL1 = sub;
  } else {
    const pure = buildL1MemberOrVarForLift(projection, ctx);
    if (pure === null) {
      return restore();
    }
    projectionL1 = pure;
  }

  const binderName = allocateLiftBinder(ctx, "n");
  const binderVar = ir1Var(binderName);

  // L1 rewriter: replace operand-shape subtrees with `Var(binder)`.
  const { result: substitutedL1, changed: substitutionFired } =
    substituteL1Subtree(projectionL1, operandL1, binderVar);

  // For Member operand: the L1 rewriter is the only substitution
  // mechanism — `ast.substituteBinder` substitutes by name and cannot
  // target a Member subtree. If the rewriter didn't fire (the
  // operand isn't structurally present in the projection's L1
  // form, e.g., projection `v.next.name` against operand `u.next`),
  // reject. Without the explicit `changed` flag the walker's
  // unconditional parent reconstruction (`ir1Member(rec, name)` and
  // siblings) breaks reference equality even when no substitution
  // fired, letting projections that don't reference the operand emit
  // a comprehension body with a free variable.
  if (operandL1.kind === "member" && !substitutionFired) {
    return restore();
  }

  // Lower.
  const ast = getAst();
  let projectionOpaque = lowerExpr(lowerL1Expr(substitutedL1));
  const operandOpaque = lowerExpr(lowerL1Expr(operandL1));

  // For Var operand: post-lower name substitution catches references
  // buried inside `from-l2` wraps (which the L1 rewriter cannot enter).
  // Mirror `translateExpr`'s identifier handling
  // (`translate-signature.ts:1122` — `paramNames.get(text) ?? text`):
  // params get sanitized; bare locals / captures emit as raw text.
  if (operandL1.kind === "var" && operandText !== null) {
    const operandPantName = ctx.paramNames.get(operandText) ?? operandText;
    projectionOpaque = ast.substituteBinder(
      projectionOpaque,
      operandPantName,
      ast.var(binderName),
    );
  }

  const opaqueEach = ast.each(
    [],
    [ast.gIn(binderName, operandOpaque)],
    projectionOpaque,
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
