// IR anchor fixture. Exercises the *minimum interesting body* — const
// binding, `??`, property read, conditional — that the IR pipeline
// must handle via native IR construction. See CLAUDE.md §"Intermediate
// Representation".

interface Cache {
  fallback: number;
  value: number | null;
}

export function effectiveValue(c: Cache, factor: number): number {
  const base = c.value ?? c.fallback;
  return factor > 0 ? base * factor : base;
}
