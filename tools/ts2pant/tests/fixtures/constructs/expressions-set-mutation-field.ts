// Stage A: .add / .delete / .clear on an interface-field Set. Owner is
// the user's interface, rule name is the field name (the Set field emits
// one arity-1 list-valued accessor rule, unlike Map's rule pair).
// Emission shape: `all y: T | y in tags' c <=> cond ...`.
// Parameter-level Set mutation is rejected (see set-mutation-rejected).

interface Tagged {
  tags: Set<string>;
}

export function tagAdd(c: Tagged, x: string): void {
  c.tags.add(x);
}

export function tagRemove(c: Tagged, x: string): void {
  c.tags.delete(x);
}

export function tagClear(c: Tagged): void {
  c.tags.clear();
}

// Sequential accumulation on one path: two `.add(x)` calls collapse into
// one membership equation whose cond has two equality arms + preState
// fallthrough.
export function tagAddTwo(c: Tagged, a: string, b: string): void {
  c.tags.add(a);
  c.tags.add(b);
}

// Later-wins: a `.delete(x)` after `.add(x)` drops the prior add arm so
// the final equation has one arm `y = x => false` (not two conflicting
// arms).
export function tagAddThenRemove(c: Tagged, x: string): void {
  c.tags.add(x);
  c.tags.delete(x);
}

// Clear-then-add: the fallthrough in the membership equation drops to
// literal `false` (not preState), so only the post-clear adds matter.
export function tagClearAndAdd(c: Tagged, x: string): void {
  c.tags.clear();
  c.tags.add(x);
}

// Conditional add: the merge folds the `.add` into a cond under the
// guard, with the identity branch returning the pre-state membership.
export function tagAddIf(c: Tagged, x: string, guard: boolean): void {
  if (guard) {
    c.tags.add(x);
  }
}

// Conditional delete alongside an unconditional add on a different
// element: two distinct override-element arms, with the delete merged
// under its guard.
export function tagRemoveIfAddElse(
  c: Tagged,
  x: string,
  y: string,
  guard: boolean,
): void {
  c.tags.add(y);
  if (guard) {
    c.tags.delete(x);
  }
}

// Non-null-asserted receiver path: same unwrapping contract as Map's
// Stage A dispatch.
export function tagAddNonNull(c: Tagged, x: string): void {
  c.tags!.add(x);
}

// Parenthesized receiver path: ParenthesizedExpression unwrapped before
// Stage A detection.
export function tagAddParen(c: Tagged, x: string): void {
  (c.tags).add(x);
}
