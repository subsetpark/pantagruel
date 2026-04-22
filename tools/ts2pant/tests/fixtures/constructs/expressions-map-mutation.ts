// Stage C: Map<K, V> mutation via Pantagruel's N-ary override.
// .set(k, v) and .delete(k) on a Map parameter emit one override pair
// per modified rule (value rule + membership predicate), quantified
// over (m, k) with frame semantics implicit in the override's ite
// expansion. See CLAUDE.md § Partial Rules / Mutation.

/** Single .set, unconditional. */
export function put(m: Map<string, number>, k: string, v: number): void {
  m.set(k, v);
}

/** Single .delete, unconditional. */
export function remove(m: Map<string, number>, k: string): void {
  m.delete(k);
}

/**
 * Counter bump — .set reading the prior value through the guarded rule.
 * `.get(k)!` asserts membership; Pantagruel's declaration guard on
 * `stringToIntMap` means uses of the value are implicitly conditioned on
 * `stringToIntMapKey counts k`.
 */
export function bump(counts: Map<string, number>, k: string): void {
  counts.set(k, counts.get(k)! + 1);
}

/**
 * Conditional .set — exercises if-merge over map writes. The else-branch
 * has no mutation, so per-key fallback for value is `stringToIntMap m k`
 * and for membership is `stringToIntMapKey m k`.
 */
export function putIfAbsent(
  m: Map<string, number>,
  k: string,
  v: number,
): void {
  if (!m.has(k)) {
    m.set(k, v);
  }
}

/**
 * Two writes to the same map — overrides accumulate as two pairs in one
 * override expression per modified rule.
 */
export function putPair(
  m: Map<string, number>,
  k1: string,
  v1: number,
  k2: string,
  v2: number,
): void {
  m.set(k1, v1);
  m.set(k2, v2);
}

/**
 * Read-after-write — `.get(kSrc)` after `.set(kSrc, v)` must observe the
 * staged write. The second `.set`'s value threads the prior override
 * inline as `stringToIntMap[(m, kSrc) |-> v] m kSrc`, so the emitted
 * equation correctly reflects "copy the just-written value at kSrc into
 * kDst" rather than reading the pre-state rule.
 */
export function setAndCopy(
  m: Map<string, number>,
  kSrc: string,
  kDst: string,
  v: number,
): void {
  m.set(kSrc, v);
  m.set(kDst, m.get(kSrc)!);
}

/**
 * Read-after-delete — `.get(kA)` after `.set(kA, v); .delete(kA)` must not
 * pick up the staged `v`. Pantagruel's declaration guard makes the value
 * rule vacuous under false membership at emission time, but an inline
 * override read has no such guard — so the filter in readMapThroughWrites
 * drops the stale value override at `(m, kA)` once its latest membership
 * is false. The inner `m.get(kA)!` therefore lowers to the pre-state
 * `stringToIntMap m kA`.
 */
export function deleteThenCopy(
  m: Map<string, number>,
  kA: string,
  kB: string,
  v: number,
): void {
  m.set(kA, v);
  m.delete(kA);
  m.set(kB, m.get(kA)!);
}

/**
 * Repeated writes to the same key — the later write must win. Pantagruel
 * override expansion is first-pair-wins, so `installMapWrite` must drop
 * the prior `(m, k) |-> v1` pair when `(m, k) |-> v2` is appended; the
 * emitted override contains only the last write at `(m, k)`.
 */
export function setTwice(
  m: Map<string, number>,
  k: string,
  v1: number,
  v2: number,
): void {
  m.set(k, v1);
  m.set(k, v2);
}

/**
 * Set then delete on the same key — membership must end up `false` after
 * the delete (the later write), not `true` from the stale `.set`. Without
 * per-tuple dedupe in `installMapWrite`, first-pair-wins would keep the
 * `.set`'s membership `|-> true` ahead of the `.delete`'s `|-> false`.
 */
export function setThenDelete(
  m: Map<string, number>,
  k: string,
  v: number,
): void {
  m.set(k, v);
  m.delete(k);
}
