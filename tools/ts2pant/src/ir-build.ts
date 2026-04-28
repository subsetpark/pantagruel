/**
 * TS-AST → IR construction.
 *
 * Stages migrated to native IR construction (latest first):
 * - Stage 2: Optional chaining `x?.f` → `Each(t, x, [], App(qualified-rule, [t]))`.
 * - Stage 1: `Var` (Identifier), `Lit` (numeric / string / boolean
 *   literal). Trivial cases of `App` (free function call with all args
 *   themselves IR-buildable). Everything else falls back to legacy
 *   `translateBodyExpr` and wraps in `IRWrap`.
 *
 * Subsequent stages (3+) migrate one recognizer at a time. By Stage 8
 * (pure-path cutover) the `IRWrap` escape hatch is deleted entirely.
 *
 * See `ir.ts` and CLAUDE.md §IR for the form table.
 */

import ts from "typescript";
import {
  type IRExpr,
  irAppExpr,
  irAppName,
  irCond,
  irEach,
  irLitBool,
  irLitNat,
  irLitString,
  irVar,
  irWrap,
} from "./ir.js";
import { ir1FromL2, ir1IsNullish } from "./ir1.js";
import {
  buildL1MemberAccess,
  type L1BuildContext,
  tryBuildL1Cardinality,
  unwrapParens,
} from "./ir1-build.js";
import { lowerL1Expr } from "./ir1-lower.js";
import {
  allocComprehensionBinder,
  ambiguousFieldMsg,
  bodyExpr,
  isBodyUnsupported,
  isNullableTsType,
  qualifyFieldAccess,
  translateBodyExpr,
  type UniqueSupply,
} from "./translate-body.js";
import type { NumericStrategy } from "./translate-types.js";

/**
 * Translate a TypeScript expression to IR.
 *
 * Returns either an `IRExpr` (success) or `{ unsupported: string }` to
 * propagate translation rejections through the existing convention.
 *
 * Non-natively-built forms fall back to `translateBodyExpr` + `IRWrap`
 * so the IR pipeline is end-to-end exercisable without committing to
 * full coverage in one stage.
 */
export function buildIR(
  expr: ts.Expression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  supply: UniqueSupply,
): IRExpr | { unsupported: string } {
  // Universal L1-layering: strip `(…)` wrappers at the build dispatch
  // entry so downstream recognizers see canonical shapes. Type-erasure
  // wrappers (`as T`, `!`, `<T>x`, `satisfies`) are NOT stripped — they
  // change the TS-checker type of the inner node and are load-bearing
  // for type-dependent dispatch like `qualifyFieldAccess`.
  expr = unwrapParens(expr) as ts.Expression;

  // L1 build context for sub-dispatchers (cardinality / Member). Pure
  // path has no symbolic state.
  const l1Ctx: L1BuildContext = {
    checker,
    strategy,
    paramNames,
    state: undefined,
    supply,
  };

  // Boolean keywords
  if (expr.kind === ts.SyntaxKind.TrueKeyword) {
    return irLitBool(true);
  }
  if (expr.kind === ts.SyntaxKind.FalseKeyword) {
    return irLitBool(false);
  }

  // Non-negative numeric literal: nat (negative literals require `Unop(Neg,
  // ...)` and aren't part of the Stage 1 scope; they fall through to
  // IRWrap via the legacy path).
  if (ts.isNumericLiteral(expr)) {
    const n = Number(expr.text);
    if (Number.isFinite(n) && Number.isInteger(n) && n >= 0) {
      return irLitNat(n);
    }
  }

  // String literal
  if (ts.isStringLiteral(expr) || ts.isNoSubstitutionTemplateLiteral(expr)) {
    return irLitString(expr.text);
  }

  // Plain identifier — resolve through the param-name map (which renames
  // TS parameter names to Pant names) so a `Var(name)` in IR refers to
  // the Pant-side name. Mirrors the existing identifier-translation site
  // in translate-body.ts.
  if (ts.isIdentifier(expr)) {
    const renamed = paramNames.get(expr.text) ?? expr.text;
    return irVar(renamed);
  }

  // M5: cardinality dispatch (`.length` / `.size` → `Unop(card, x)`).
  // Pant's primitive for list cardinality is `#x`, not a `length` / `size`
  // rule application — routing through Member would produce a dangling
  // EUF function distinct from the actual cardinality. Fires before the
  // Member dispatch below for the six list-shaped TS types.
  if (ts.isPropertyAccessExpression(expr)) {
    const card = tryBuildL1Cardinality(expr, l1Ctx);
    if (card !== null) {
      return lowerL1Expr(card);
    }
  }

  // Stage 2: Optional chain `x?.prop` (and tail of a chain marked by
  // NodeFlags.OptionalChain) under list-lift. With `x: [T]` on the Pant
  // side, the functor-lift encoding is `each $n in x | prop $n`, giving
  // `[U]` — empty when x is null/empty, singleton when x is present.
  // Chains compose as nested Each comprehensions: `x?.a.b` becomes
  // `Each($n', Each($n, x, [], a $n), [], b $n')`. When the receiver is
  // not nullable in TS, `?.` degenerates to `.` and falls through.
  if (ts.isPropertyAccessExpression(expr)) {
    const inOptionalChain = (expr.flags & ts.NodeFlags.OptionalChain) !== 0;
    if (inOptionalChain) {
      let shouldLift = false;
      if (expr.questionDotToken !== undefined) {
        const receiverTsType = checker.getTypeAtLocation(expr.expression);
        shouldLift = isNullableTsType(receiverTsType);
      } else if (ts.isOptionalChain(expr.expression)) {
        shouldLift = true;
      }
      if (shouldLift) {
        const prop = expr.name.text;
        const receiverTsType = checker.getTypeAtLocation(expr.expression);
        const ruleName = qualifyFieldAccess(
          receiverTsType,
          prop,
          checker,
          strategy,
          supply.synthCell,
        );
        if (ruleName === null) {
          return { unsupported: ambiguousFieldMsg(prop) };
        }
        const innerIR = buildIR(
          expr.expression,
          checker,
          strategy,
          paramNames,
          supply,
        );
        if (isBuildUnsupported(innerIR)) {
          return innerIR;
        }
        const binderName = allocComprehensionBinder(supply, "n");
        return irEach(
          binderName,
          innerIR,
          [],
          irAppName(ruleName, [irVar(binderName)]),
        );
      }
    }
  }

  // M5: plain (non-optional) property access → canonical L1 Member.
  // The lowering at `ir1-lower.ts` produces `App(qualifiedName,
  // [receiver])` — byte-equal to the legacy direct construction.
  // Cardinality dispatch above already handled the `.length` / `.size`
  // path, so anything reaching here is a genuine field accessor.
  if (
    ts.isPropertyAccessExpression(expr) &&
    (expr.flags & ts.NodeFlags.OptionalChain) === 0
  ) {
    const member = buildL1MemberAccess(expr, l1Ctx);
    if ("unsupported" in member) {
      return member;
    }
    return lowerL1Expr(member);
  }

  // Stage 3: Nullish coalescing `x ?? y` under list-lift. With `x: [T]`
  // on the Pant side, `#x = 0` is the null test:
  //   - `y: T` (non-nullable default) → `cond #x = 0 => y, true => (x 1)`
  //   - `y: [T]` (nested nullable) → `cond #x = 0 => y, true => x`
  //   - `x` not nullable in TS → `??` degenerates to just `x`.
  // See CLAUDE.md "Option-Type Elimination" for the broader encoding.
  if (
    ts.isBinaryExpression(expr) &&
    expr.operatorToken.kind === ts.SyntaxKind.QuestionQuestionToken
  ) {
    const leftTsType = checker.getTypeAtLocation(expr.left);
    const leftIR = buildIR(expr.left, checker, strategy, paramNames, supply);
    if (isBuildUnsupported(leftIR)) {
      return leftIR;
    }
    if (!isNullableTsType(leftTsType)) {
      // LHS can never be nullish — `??` degenerates.
      return leftIR;
    }
    const rightIR = buildIR(expr.right, checker, strategy, paramNames, supply);
    if (isBuildUnsupported(rightIR)) {
      return rightIR;
    }
    const rightTsType = checker.getTypeAtLocation(expr.right);
    // Construct the cardinality-zero null test through L1 IsNullish so
    // every nullish-shape lowering shares one source of truth (M4).
    // The lowering chain `from-l2 → IsNullish → eq(card(_), 0)` is
    // byte-identical to the inlined form, which is the cutover gate.
    const cardZero = lowerL1Expr(ir1IsNullish(ir1FromL2(leftIR)));
    const presentBranch: IRExpr = isNullableTsType(rightTsType)
      ? leftIR
      : irAppExpr(leftIR, [irLitNat(1)]);
    return irCond([
      [cardZero, rightIR],
      [irLitBool(true), presentBranch],
    ]);
  }

  // Fallback: translate via the legacy pipeline and wrap. Subsequent
  // stages migrate specific recognizers (Cond, App, Comb, Forall,
  // Exists) into native IR construction here. `state` is undefined
  // because pure-path bodies don't read symbolic state; mutating-path
  // migration happens in Stage 9.
  const legacy = translateBodyExpr(
    expr,
    checker,
    strategy,
    paramNames,
    undefined,
    supply,
  );
  if (isBodyUnsupported(legacy)) {
    return { unsupported: legacy.unsupported };
  }
  if ("effect" in legacy) {
    return {
      unsupported:
        "collection mutation in IR-build expression position (Stage 9)",
    };
  }
  return irWrap(bodyExpr(legacy));
}

export function isBuildUnsupported(
  r: IRExpr | { unsupported: string },
): r is { unsupported: string } {
  return "unsupported" in r;
}
