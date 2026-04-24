// Nullish-coalescing (`??`) and optional chaining (`?.`) under the
// list-lift encoding. An optional TS type `T | null` translates to `[T]`,
// so `#x = 0` is the null test and `(x 1)` extracts the singleton.
//
// See CLAUDE.md "Option-Type Elimination" for the encoding; REFERENCE.md
// (Pantagruel core) documents the `[T]` + cardinality-invariant idiom as
// Alloy `lone` multiplicity.

interface Account {
  readonly balance: number;
  readonly owner: Owner;
}

interface Owner {
  readonly id: number;
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

/** Mixed chain `x?.a.b`: TS parses this with `?.` only on the first hop;
 *  the trailing `.b` must still be lifted over the comprehension produced
 *  by `?.a`. Expect `each $n in x | id ($m ∘ owner) $n` — the tail access
 *  composes inside the same outer lift. */
export function maybeOwnerId(a: Account | null): number | undefined {
  return a?.owner.id;
}

/** Double-guarded chain `x?.a?.b`: both hops get `?.` — each step adds
 *  another comprehension layer over the list-lift result of the previous. */
export function maybeOwnerIdOptional(
  a: Account | null,
): number | undefined {
  return a?.owner?.id;
}
