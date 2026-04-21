// Set<T> operations: .has, .size
// Pantagruel models Set<T> as [T] (the list type already encodes membership
// via `x in xs` in the SMT backend). Uniqueness is not a tracked invariant.

/** .has(x) on a Set -> x in */
export function contains(xs: Set<string>, x: string): boolean {
  return xs.has(x);
}

/** .size on a Set -> # */
export function cardinality(xs: Set<string>): number {
  return xs.size;
}

/**
 * Both membership and cardinality in one body. The annotation is a genuine
 * (non-tautological) entailment: if two names are in the set, the first is in.
 * @pant all xs: [String], x: String, y: String | both xs x y -> x in xs
 */
export function both(xs: Set<string>, x: string, y: string): boolean {
  return xs.has(x) && xs.has(y);
}
