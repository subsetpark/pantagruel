// Kleene μ-minimization: `let i = INIT; while (P(i)) i++;` recognized in
// pure function bodies and translated to `min over each $j: Nat,
// $j >= INIT, ~P($j) | $j`. Reference: Kleene, *General Recursive Functions
// of Natural Numbers*, Math. Ann. 112 (1936); Kroening & Strichman,
// *Decision Procedures* Ch. 4.
//
// The counter binding flows through the existing const-inlining substitution
// machinery, so post-loop references to the counter resolve transparently —
// the consumer sees a `min over each` expression in place of the counter.

/** Find the smallest i >= 1 not present in `used`. Canonical μ-search. */
export function firstUnusedSuffix(used: ReadonlySet<number>): number {
  let i = 1;
  while (used.has(i)) {
    i++;
  }
  return i;
}

/** Counter is referenced in a downstream expression: the substitution
 *  inlines the `min over each` directly into the `+ 1`. */
export function nextSlotPlusOne(used: ReadonlySet<number>): number {
  let i = 0;
  while (used.has(i)) {
    i++;
  }
  return i + 1;
}

/** μ-search alongside an ordinary const binding: both flow through the
 *  same prelude scan and substitution closure. */
export function offsetUnusedSuffix(
  used: ReadonlySet<number>,
  offset: number,
): number {
  const base = offset;
  let i = 1;
  while (used.has(i)) {
    i++;
  }
  return base + i;
}

/** Compound while body — recognizer rejects (more than one statement),
 *  then `extractReturnExpression` rejects on the `let` binding it can't
 *  consume. Locks in the conservative behavior. */
export function compoundWhileBody(used: ReadonlySet<number>): number {
  let i = 0;
  while (used.has(i)) {
    i++;
    i++;
  }
  return i;
}

/** Unbraced while body — the body is an ExpressionStatement directly,
 *  not wrapped in a Block. CLAUDE.md's canonical form permits this. */
export function unbracedWhileBody(used: ReadonlySet<number>): number {
  let i = 1;
  while (used.has(i)) i++;
  return i;
}

/** Compound-assignment increment step (`i += 1`). M2's L1 normalizer
 *  collapses this to the same canonical Assign as `i++` — same Pant
 *  output. */
export function compoundIncrementStep(used: ReadonlySet<number>): number {
  let i = 1;
  while (used.has(i)) {
    i += 1;
  }
  return i;
}

/** Explicit-assignment increment step (`i = i + 1`). M2 collapses to
 *  the canonical Assign. */
export function explicitIncrementStep(used: ReadonlySet<number>): number {
  let i = 1;
  while (used.has(i)) {
    i = i + 1;
  }
  return i;
}

/** Counter on the right of the explicit `+` (`i = 1 + i`) —
 *  commutative-rewrite gives the same canonical Assign. */
export function explicitIncrementStepFlipped(
  used: ReadonlySet<number>,
): number {
  let i = 1;
  while (used.has(i)) {
    i = 1 + i;
  }
  return i;
}
