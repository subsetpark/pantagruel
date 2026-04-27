// Functor-lift recognizer (M5 Patch 4) — Member operand.
//
// M5 P4 lifts the operand restriction from M4 P5: the recognizer now
// accepts a property-access (or string-literal element-access) operand
// alongside the simple-identifier operand it shipped with. The four
// canonical TS shapes still apply (positive ternary, negated ternary,
// positive if-conversion, negated if-conversion); the only change is
// the eligible operand vocabulary. All four lower to
// `each $n in (owner u) | name $n` (or equivalent).

interface User {
  readonly name: string;
}

interface Account {
  readonly owner: User | null;
}

/** Positive ternary, Member operand. */
export function userName(u: Account): string[] {
  return u.owner == null ? [] : [u.owner.name];
}

/** Negated ternary, Member operand. */
export function userNameTernaryNeg(u: Account): string[] {
  return u.owner !== null ? [u.owner.name] : [];
}

/** Positive if-conversion, Member operand. */
export function userNameIfConv(u: Account): string[] {
  if (u.owner === null) return [];
  return [u.owner.name];
}

/** Negated if-conversion, Member operand. */
export function userNameIfConvNeg(u: Account): string[] {
  if (u.owner !== null) return [u.owner.name];
  return [];
}

/** Paren-equivalence: source-level `( ... )` wrappers around the
 *  Member operand and the projection must produce the same Pant as
 *  the unwrapped form. Verifies the L1-layering invariant — paren
 *  normalization happens at L1 build, downstream recognizers don't
 *  re-implement it. */
export function userNameParens(u: Account): string[] {
  return (u.owner) == null ? [] : [(u.owner).name];
}

// The deliberate-reject case for a multi-element non-empty branch
// (`return u.owner == null ? [] : [u.owner.name, u.owner.name];`) is
// not included as a fixture entry — it has no translatable Pant target
// and would emit raw source text into the snapshot. The unit test
// `Member operand multi-element non-empty branch rejects` in
// `tests/ir1-build-functor-lift.test.mts` exercises that path
// directly.
