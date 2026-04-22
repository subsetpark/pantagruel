// Map<K, V> on an interface field expands into two Pantagruel rules:
//   entriesKey c: Cache, k: String => Bool.
//   entries c: Cache, k: String, entriesKey c k => Int.
//
// The value rule is guarded by the membership predicate. Declaration guards
// are automatically injected as antecedents in SMT queries, so absent keys
// say nothing about the value — a cleaner specification semantics than
// modeling partiality as `V + Nothing`.

interface Cache {
  entries: Map<string, number>;
}

/** .get(k)! on a Map field -> guarded rule application */
export function lookup(c: Cache, k: string): number {
  return c.entries.get(k)!;
}

/** .has(k) on a Map field -> membership predicate */
export function contains(c: Cache, k: string): boolean {
  return c.entries.has(k);
}

/**
 * Non-null-asserted field receiver on a read: `c.entries!` is a
 * NonNullExpression wrapping a PropertyAccessExpression. Stage A read
 * detection must unwrap before the PropertyAccessExpression / interface-field
 * checks, otherwise the read synthesizes a separate StringToIntMap instead
 * of using the declared `entries` rule.
 */
export function lookupNonNull(c: Cache, k: string): number {
  return c.entries!.get(k)!;
}

/**
 * Real entailment exercising both rules in the same module: when the
 * predicate holds (via `.has`), the guarded value rule has a meaning and is
 * equal to itself (EUF congruence). The body sets `congruence c k` to
 * `entriesKey c k`, so under `congruence c k`, the guard on `entries` is
 * discharged and the proposition is tautologically true.
 * @pant all c: Cache, k: String | congruence c k -> entries c k = entries c k
 */
export function congruence(c: Cache, k: string): boolean {
  return c.entries.has(k);
}
