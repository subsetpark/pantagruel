// Optional parameter with a `??`-default under the general list-lift
// lowering: an optional TS param `p?: P` list-lifts to `p: [P]` and each
// `p ?? c` use expands to `cond #p = 0 => c, true => (p 1)`.

export interface Point {
  readonly x: number;
  readonly y: number;
}

/** Single optional param, literal default. Translates to:
 *    makePoint initial: [Int] => Point.
 *    x (makePoint initial) = (cond #initial = 0 => 0, true => initial 1).
 */
export function makePoint(initial?: number): Point {
  return { x: initial ?? 0, y: 0 };
}

export interface Config {
  readonly timeout: number;
}

/** Optional param after a required one — same list-lift treatment; the
 *  required param passes through unchanged. */
export function makeConfig(base: number, extra?: number): Config {
  return { timeout: base + (extra ?? 10) };
}

/** Multiple `??` uses on the same optional param — each expands
 *  independently, producing nested `cond` expressions. */
export function usesOptionalDirectly(value?: number): number {
  return value ?? 0 + (value ?? 1);
}
