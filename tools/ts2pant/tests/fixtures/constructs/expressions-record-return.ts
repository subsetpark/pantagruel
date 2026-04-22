// Record returns: a pure function whose body is `return { f1: e1, f2: e2 }`
// decomposes into one equation per field of the return type. The return
// type must be a named interface whose accessor rules are already declared;
// anonymous record types (synthesized result domains) are a separate stage.
// See CLAUDE.md § Record Returns.

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
