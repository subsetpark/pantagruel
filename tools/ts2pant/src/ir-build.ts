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
  irAppName,
  irLitBool,
  irLitNat,
  irLitString,
  irVar,
  irWrap,
} from "./ir.js";
import {
  ambiguousFieldMsg,
  bodyExpr,
  freshHygienicBinder,
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
        const binderName = freshHygienicBinder(supply);
        return {
          kind: "each",
          binder: binderName,
          src: innerIR,
          guards: [],
          proj: irAppName(ruleName, [irVar(binderName)]),
        };
      }
    }
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
