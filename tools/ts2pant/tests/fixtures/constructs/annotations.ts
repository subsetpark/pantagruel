// @pant annotation extraction: single-line, multi-line, type overrides

/**
 * Add two numbers.
 * @pant all a: Int, b: Int | add a b = a + b
 */
export function add(a: number, b: number): number {
  return a + b;
}

/**
 * Multiple annotations.
 * @pant all a: Int, b: Int | sum a b >= a
 * @pant all a: Int, b: Int | sum a b >= b
 */
export function sum(a: number, b: number): number {
  return a + b;
}

/**
 * Multi-line annotation block.
 * @pant-begin
 * all x: Int, x >= 0 |
 *   rangeCheck x
 * @pant-end
 */
export function rangeCheck(x: number): boolean {
  return x >= 0;
}

/** No annotations → empty check block */
export function noAnnot(x: number): number {
  return x;
}

/**
 * @pant-type amount: Nat0
 * @pant all a: Nat0 | deposited a >= 0
 */
export function deposited(amount: number): number {
  return amount;
}
