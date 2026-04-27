// Functor-lift recognizer (M4 Patch 5).
//
// `if (x == null) return []; return [f(x)];` and the equivalent
// ternary forms lower to `each $n in x | f $n`. Pant has no list
// literal, so the alternative cardinality-dispatch lowering for these
// shapes is untranslatable — without the recognizer these functions
// reject. See `tools/ts2pant/CLAUDE.md` "Option-Type Elimination" and
// `workstreams/ts2pant-imperative-ir.md` § "M4".

interface User {
  readonly name: string;
  readonly age: number;
}

/** Ternary, positive guard, single-element array on the present side.
 *  `optionalNames(u)` returns `[]` when u is null and `[u.name]` when
 *  it isn't. Lifts to `each $n in u | name $n`. */
export function optionalNames(u: User | null): string[] {
  return u == null ? [] : [u.name];
}

/** Ternary, positive guard, bare projection on the present side. The
 *  result type `string | null` is itself list-lifted (`[String]`), so
 *  the bare `u.name` projects through the comprehension without an
 *  array wrapper. */
export function optionalChain(u: User | null): string | null {
  return u == null ? null : u.name;
}

/** Negated guard: `(x !== null) ? f(x) : null` — present side on the
 *  then-branch, empty side on the else-branch. */
export function negatedTernary(u: User | null): number | null {
  return u !== null ? u.age : null;
}

/** If-conversion: early return for the empty side, terminal return
 *  for the present side. The recognizer fires at the prelude level. */
export function ifConversion(u: User | null): string[] {
  if (u === null) {
    return [];
  }
  return [u.name];
}

/** Negated if-conversion: early return for the present side. */
export function negatedIfConversion(u: User | null): string[] {
  if (u !== null) {
    return [u.name];
  }
  return [];
}

/** Long-form positive guard: `x === null || x === undefined` folds to
 *  `IsNullish(x)` first, then the lift fires. The functor-lift
 *  recognizer requires a *leaf* nullish form on the guard, so the
 *  caller cannot use the long-form here — but the simpler `== null`
 *  form covers both null and undefined and so it suffices. This
 *  function exercises the loose-eq leaf form on a `T | null |
 *  undefined` operand. */
export function looseGuardLift(u: User | null | undefined): string[] {
  return u == null ? [] : [u.name];
}

/** typeof-undefined leaf form on the guard. */
export function typeofGuardLift(u: User | undefined): string[] {
  return typeof u === "undefined" ? [] : [u.name];
}
