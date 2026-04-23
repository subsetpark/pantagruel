// Optional parameter with a `??`-default. ts2pant detects the idiom and
// emits two Pantagruel arity overloads: one with the param, one without.
// Coherence (same name, same type at position 0 across both heads, same
// return type) is enforced by the core language.

export interface Point {
  readonly x: number;
  readonly y: number;
}

/** Single optional param, literal default. Translates to two heads:
 *    makePoint initial: Int => Point.
 *    makePoint             => Point.
 */
export function makePoint(initial?: number): Point {
  return { x: initial ?? 0, y: 0 };
}

export interface Config {
  readonly timeout: number;
}

/** Optional param comes after a required one. The required param is shared
 *  across both overloads (position 0); the optional is only in the longer
 *  head. Default is also a literal. */
export function makeConfig(base: number, extra?: number): Config {
  return { timeout: base + (extra ?? 10) };
}

/** Negative: optional param is used outside a `??` expression — ts2pant
 *  leaves the signature as the single `T + Nothing` form and body emission
 *  falls back to the existing unsupported-operator error when it encounters
 *  a bare reference it can't reduce. */
export function usesOptionalDirectly(value?: number): number {
  return value ?? 0 + (value ?? 1);
}
