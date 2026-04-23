// Nullish-coalescing (`??`) and optional chaining (`?.`) under the
// list-lift encoding. An optional TS type `T | null` translates to `[T]`,
// so `#x = 0` is the null test and `(x 1)` extracts the singleton.
//
// See CLAUDE.md "Option-Type Elimination" for the encoding; REFERENCE.md
// (Pantagruel core) documents the `[T]` + cardinality-invariant idiom as
// Alloy `lone` multiplicity.

interface Account {
  readonly balance: number;
}

/** `x ?? y` with non-nullable default → `cond #x = 0 => y, true => (x 1)`.
 *  Result type is `Int` (the non-null narrowing). */
export function defaultToZero(x: number | null): number {
  return x ?? 0;
}

/** `x ?? y` with nullable default → `cond #x = 0 => y, true => x`.
 *  Both arms stay `[Int]`; the list-lift propagates to the return. */
export function preferLeft(x: number | null, y: number | null): number | null {
  return x ?? y;
}

/** `x?.prop` functor lift → `each t in x | prop t` : `[Int]`. */
export function maybeBalance(a: Account | null): number | undefined {
  return a?.balance;
}

/** `??` on a non-nullable LHS degenerates to just the LHS. */
export function nonNullDefault(x: number): number {
  return x ?? 0;
}
