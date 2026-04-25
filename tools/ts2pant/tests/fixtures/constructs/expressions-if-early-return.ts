// If-early-return generalized to any prelude position.
// `if (P) return E;` (no else, single-statement return body) becomes one
// arm of a synthetic cond whose catch-all is the rest of the body.

/** single arm + terminal */
export function singleArm(n: number): number {
  if (n < 0) return 0;
  return n + 1;
}

/** const binding before arm — predicate sees prior binding */
export function bindingThenArm(n: number): number {
  const doubled = n + n;
  if (doubled < 0) return 0;
  return doubled;
}

/** arm between bindings — terminal sees both bindings */
export function armBetweenBindings(n: number): number {
  const a = n + 1;
  if (a < 0) return 0;
  const b = a + a;
  return b;
}

/** two arms — first match wins */
export function twoArms(n: number): number {
  if (n < 0) return -1;
  if (n === 0) return 0;
  return n + 1;
}

/** arm body uses an unbraced bare return */
export function unbracedArm(n: number): number {
  if (n < 0) return 0;
  return n;
}

/** μ-search after early-return arm */
export function armThenMuSearch(n: number, used: ReadonlySet<number>): number {
  if (n < 0) return 0;
  let j = 1;
  while (used.has(j)) {
    j++;
  }
  return j;
}
