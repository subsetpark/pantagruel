// Stage A: .set / .delete on an interface-field Map. Owner is the
// user's interface, rule name is the field name (and `${field}Key`
// for the membership predicate). Same override-based encoding as
// Stage B. See CLAUDE.md § Partial Rules / Mutation.

interface Cache {
  entries: Map<string, number>;
}

export function cachePut(c: Cache, k: string, v: number): void {
  c.entries.set(k, v);
}

export function cacheEvict(c: Cache, k: string): void {
  c.entries.delete(k);
}

/**
 * Non-null-asserted field receiver — `c.entries!` is a NonNullExpression
 * wrapping the PropertyAccessExpression. Stage A detection must unwrap
 * before checking `isPropertyAccessExpression` / `isInterfaceFieldAccess`,
 * otherwise the write falls into Stage B and synthesizes a separate
 * StringToIntMap sort instead of using the declared `entries` rule.
 */
export function cachePutNonNull(c: Cache, k: string, v: number): void {
  c.entries!.set(k, v);
}

/**
 * Parenthesized field receiver — `(c.entries)` is a ParenthesizedExpression
 * wrapping the PropertyAccessExpression. Same unwrapping requirement as the
 * non-null case.
 */
export function cacheEvictParen(c: Cache, k: string): void {
  (c.entries).delete(k);
}
