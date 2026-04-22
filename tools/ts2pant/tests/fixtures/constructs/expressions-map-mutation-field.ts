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
