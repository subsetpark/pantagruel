// M4 — equality and nullish normalization. Every TS expression in the
// equality/nullish equivalence class flows through one of three
// canonical Layer 1 forms:
//   - `IsNullish(x)` — null/undefined tests; positive forms here, with
//     `unop(not, IsNullish(...))` for negated. Lowers to `#x = 0`
//     under the list-lift encoding.
//   - `BinOp(eq | neq, …)` — strict equality `===` / `!==`. Lowers to
//     Pant `=` / `~=`.
//   - Loose equality `==` / `!=` is rejected unless consumed by the
//     nullish recognizer. Rejection is exercised in
//     `tests/equality-canonicalization.test.mts` — the constructs
//     runner skips functions that fail to translate, so rejected
//     forms can't be snapshot-tested here.
//
// See `workstreams/ts2pant-m4-equality-nullish.md` and CLAUDE.md
// "Option-Type Elimination" for the encoding rationale.

// ─── Strict equality canonicalization (M4 Patch 3) ─────────────────

/** `===` on numbers → canonical L1 binop(eq) → Pant `=`. */
export function strictEqNumbers(a: number, b: number): boolean {
  return a === b;
}

/** `!==` on strings → canonical L1 binop(neq) → Pant `~=`. */
export function strictNeqStrings(a: string, b: string): boolean {
  return a !== b;
}

/** `===` against a literal — sub-expression is a literal lit, not a var. */
export function strictEqAgainstLit(n: number): boolean {
  return n === 0;
}

/** `===` with arithmetic operands — verifies sub-expressions reach the
 *  L1 binop as already-translated forms. */
export function strictEqExprOperands(a: number, b: number): boolean {
  return a + 1 === b * 2;
}

/** `!==` inside an early-return arm composes with the surrounding cond. */
export function strictNeqInArm(a: number, b: number): boolean {
  if (a < 0) return a !== b;
  return a === b;
}

// ─── Nullish surface forms (M4 Patch 2) ────────────────────────────

/** `x == null` — loose-eq nullish. Recognized; folds to IsNullish(x). */
export function looseNullEq(x: number | null): boolean {
  return x == null;
}

/** `x != null` — loose-neq nullish. Recognized; folds to not(IsNullish(x)). */
export function looseNullNeq(x: number | null): boolean {
  return x != null;
}

/** `x === null` — strict-eq null. Recognized; folds to IsNullish(x). */
export function strictNullEq(x: number | null): boolean {
  return x === null;
}

/** `x === undefined` — strict-eq undefined. Recognized; folds to IsNullish(x). */
export function strictUndefinedEq(x: number | undefined): boolean {
  return x === undefined;
}

/** `x !== null` — strict-neq null. Recognized; folds to not(IsNullish(x)). */
export function strictNullNeq(x: number | null): boolean {
  return x !== null;
}

/** `x !== undefined` — strict-neq undefined. Recognized; folds to not(IsNullish(x)). */
export function strictUndefinedNeq(x: number | undefined): boolean {
  return x !== undefined;
}

/** Long form `x === null || x === undefined` — folds to IsNullish(x). */
export function longFormPositive(x: number | null | undefined): boolean {
  return x === null || x === undefined;
}

/** Negated long form `x !== null && x !== undefined` — folds to not(IsNullish(x)). */
export function longFormNegated(x: number | null | undefined): boolean {
  return x !== null && x !== undefined;
}

/** `typeof x === 'undefined'` — recognized; folds to IsNullish(x). */
export function typeofUndefinedEq(x: number | undefined): boolean {
  return typeof x === "undefined";
}

/** `typeof x !== 'undefined'` — recognized; folds to not(IsNullish(x)). */
export function typeofUndefinedNeq(x: number | undefined): boolean {
  return typeof x !== "undefined";
}

/** Operand mismatch: `a === null || b === undefined` — long-form recognizer
 *  doesn't fire (different operands), but each leaf is still recognized
 *  recursively as a per-side IsNullish, then OR'd. */
export function disjunctionDifferentOperands(
  a: number | null,
  b: number | undefined,
): boolean {
  return a === null || b === undefined;
}
