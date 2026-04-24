// Record returns: a pure function whose body is `return { f1: e1, f2: e2 }`
// decomposes into one equation per field of the return type. Return types
// may be named interfaces (accessor rules are already declared) or
// anonymous object shapes (a domain is synthesized per shape). See
// CLAUDE.md § Record Returns.

export interface Point {
  x: number;
  y: number;
}

/** Multi-field record return; each field gets its own equation. */
export function origin(): Point {
  return { x: 0, y: 0 };
}

/** Field initializers reference parameters; quantifier form is used. */
export function translate(p: Point, dx: number, dy: number): Point {
  return { x: p.x + dx, y: p.y + dy };
}

export interface Bag {
  items: ReadonlySet<string>;
}

/** `new Set()` in a set-typed field position → empty-set via membership
 *  negation. Pantagruel has no empty-set literal; the emitted form is
 *  `all x: T | ~(x in items f)`. */
export function emptyBag(): Bag {
  return { items: new Set() };
}

/** Param name `x` collides with the binder hint used for the empty-set
 *  quantifier; the binder is suffixed (e.g. `x1`) so it cannot capture the
 *  rule parameter. The param is intentionally unread — the fixture exists
 *  only to prove the name doesn't get captured. */
// biome-ignore lint/correctness/noUnusedFunctionParameters: deliberate shadowing test
export function emptyBagShadowed(x: number): Bag {
  return { items: new Set() };
}

// --- Anonymous record returns ---

export interface NameRegistry {
  readonly used: ReadonlySet<string>;
}

/** Anonymous return type → synthesized domain `NameRegRec`. Parameter
 *  names are intentionally distinct from field names to avoid collision
 *  with accessor rules (same rule applies for named interfaces). */
export function registerShape(
  r: NameRegistry,
  s: string,
): { name: string; reg: NameRegistry } {
  return { name: s, reg: r };
}

/** Field-order permutation — should reuse the same synthesized domain
 *  as `registerShape` via canonical (sorted) dedup key. */
export function registerShapeFlipped(
  r: NameRegistry,
  s: string,
): { reg: NameRegistry; name: string } {
  return { reg: r, name: s };
}

/** Nested anonymous record — the inner shape registers its own domain
 *  bottom-up, and the outer domain's accessor references it. */
export function nestPair(): { outer: { inner: string } } {
  return { outer: { inner: "hi" } };
}

/** Empty anonymous return — default synthesized name `EmptyRec`, no
 *  accessor rules. The return type is the empty object literal `{}`,
 *  not `Record<string, never>` (which carries a string index signature
 *  and is rejected as an unbounded dictionary, not a finite record). */
export function nothing(): {} {
  return {};
}

/** Anonymous return whose field type is itself a set — empty-set
 *  initializer still works via membership negation on the synthesized
 *  accessor. */
export function emptyAnonBag(): { items: ReadonlySet<string> } {
  return { items: new Set() };
}

/** Alias-backed anonymous record — `type Pt = {...}` has `aliasSymbol`
 *  but its underlying structural shape is `__type`, so record-return
 *  emission must resolve the owner through `isAnonymousRecord` /
 *  `mapTsType` rather than the alias name. If emission used the alias
 *  name directly, the body would reference `pt-x`/`pt-y` rules that
 *  were never declared (the synth registered them under the
 *  structural shape's domain). */
type PtAlias = { x: number; y: number };
export function originAliased(): PtAlias {
  return { x: 0, y: 0 };
}

/** Param named `r` collides with the synth-record binder (also `r`).
 *  `translateSignature` resolves the collision by suffixing the param
 *  to `r1`; the body translator must pick up that same suffixed name
 *  via the threaded `paramNameMap` rather than recomputing it from
 *  `toPantTermName("r")` (which would give the bare `r` and produce
 *  an unbound identifier on the RHS). */
export function readR(r: { foo: number }): number {
  return r.foo;
}
