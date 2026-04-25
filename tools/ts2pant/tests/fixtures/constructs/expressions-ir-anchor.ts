// Stage 1 anchor for the IR migration. Exercises the *minimum interesting
// body* — const binding, `??`, property read, conditional — that the IR
// pipeline must handle (via IRWrap fallback in Stage 1; via native IR
// construction by the time of cutover). See CLAUDE.md §"Intermediate
// Representation".

interface Cache {
  fallback: number;
  value: number | null;
}

export function effectiveValue(c: Cache, factor: number): number {
  const base = c.value ?? c.fallback;
  return factor > 0 ? base * factor : base;
}
