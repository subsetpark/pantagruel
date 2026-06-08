// @archlint.module exempt
// @archlint.exempt-reason test-support

export function constReplaceLiteral(s: string): string {
  const x = s.replace("a", "b");
  return x;
}

export function constReplaceRegex(s: string): string {
  const x = s.replace(/a/g, "b");
  return x;
}

export function constArrayFromMap(m: Map<string, number>): number {
  const xs = Array.from(m.values());
  return xs.length;
}

export function constMapEntries(m: Map<string, number>): number {
  const xs = Array.from(m.entries());
  return xs.length;
}

export function constSetHas(s: Set<string>, value: string): boolean {
  const x = s.has(value);
  return x;
}
