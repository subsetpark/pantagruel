// ReadonlySet<T> translates identically to Set<T> — both become [T] since
// Pantagruel lists encode membership. Kept as a focused fixture so that any
// regression in ReadonlySet handling surfaces here rather than only via
// dogfood coverage.

/** .has(x) on a ReadonlySet -> x in */
export function containsReadonly(xs: ReadonlySet<string>, x: string): boolean {
  return xs.has(x);
}

/** .size on a ReadonlySet -> # */
export function cardinalityReadonly(xs: ReadonlySet<string>): number {
  return xs.size;
}
