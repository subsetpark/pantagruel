/**
 * TS-AST → IR construction.
 *
 * Stage 1 scope: handles `Var` (Identifier), `Lit` (numeric / string /
 * boolean literal), and the trivial cases of `App` (free function call
 * with all args themselves IR-buildable). Everything else falls back to
 * the legacy `translateBodyExpr` and wraps the result in `IRWrap` — so
 * the IR pipeline is end-to-end exercisable without committing to
 * full coverage in one stage.
 *
 * Subsequent stages migrate one recognizer at a time from
 * `translate-body.ts` into this file; each migration removes one
 * `IRWrap` fallback and adds a native IR construction. By Stage 8
 * (pure-path cutover) the `IRWrap` escape hatch is deleted entirely.
 *
 * See `ir.ts` and CLAUDE.md §IR for the form table.
 */

import ts from "typescript";
import {
  type IRExpr,
  irLitBool,
  irLitNat,
  irLitString,
  irVar,
  irWrap,
} from "./ir.js";
import {
  bodyExpr,
  isBodyUnsupported,
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
 * Stage 1: native IR construction for Identifier / NumericLiteral /
 * StringLiteral / true / false; falls back to `translateBodyExpr` +
 * `IRWrap` for everything else (so Cond, App, etc. all go through
 * IRWrap until their dedicated stages migrate them).
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

  // Stage 1 fallback: translate via the legacy pipeline and wrap the
  // result. Subsequent stages migrate specific recognizers (Cond, App,
  // Each, Comb, Forall, Exists) into native IR construction here.
  // `state` is undefined because pure-path bodies don't read symbolic
  // state; mutating-path migration happens in Stage 9.
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
