// M4 — equality and nullish normalization. This fixture exercises the
// strict-eq / strict-neq canonicalization through Layer 1. Loose-eq
// rejection is exercised in `tests/equality-canonicalization.test.mts`
// (rejection isn't snapshot-tested; the constructs runner skips
// functions that fail to translate).
//
// See `workstreams/ts2pant-m4-equality-nullish.md` for the full
// equivalence-class lock and lowering rules.

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
