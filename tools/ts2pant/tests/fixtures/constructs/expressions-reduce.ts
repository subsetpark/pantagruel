// Shape C: pure `.reduce` / `.reduceRight` → aggregate comprehension
//   `arr.reduce((a, x) => a OP f(x), init)`
//   → `init OP (combOP over each x: T | f(x))`
// `init` is elided when it equals the combiner's identity (0 for +, 1 for *,
// true for &&, false for ||). See CLAUDE.md § Structured Iteration.

interface Item {
  value: number;
  active: boolean;
}

/** explicit init = 0 (add identity) → init is elided */
export function sumAmounts(xs: Item[]): number {
  return xs.reduce((a, x) => a + x.value, 0);
}

/** explicit init = 1 (mul identity) → init is elided */
export function productValues(xs: Item[]): number {
  return xs.reduce((a, x) => a * x.value, 1);
}

/** explicit init = true (and identity) → init is elided */
export function allActive(xs: Item[]): boolean {
  return xs.reduce((a, x) => a && x.active, true);
}

/** init = false (or identity) → init is elided */
export function anyActive(xs: Item[]): boolean {
  return xs.reduce((a, x) => a || x.active, false);
}

/** init differs from identity → preserved on the outside */
export function sumFromBase(xs: Item[]): number {
  return xs.reduce((a, x) => a + x.value, 100);
}

/** non-commutative outer op (`-`) → init preserved; acc must be on the left */
export function subtractAll(xs: Item[]): number {
  return xs.reduce((a, x) => a - x.value, 1000);
}

/** reduceRight with commutative op is accepted */
export function sumAmountsRight(xs: Item[]): number {
  return xs.reduceRight((a, x) => a + x.value, 0);
}
