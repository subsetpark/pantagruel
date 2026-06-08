// @archlint.module exempt
// @archlint.exempt-reason test-support

export function chainedReplaceTrim(s: string): string {
  const a = s.replace(/\s+/gu, "");
  const b = a.trim();
  return b;
}

export function chainedArrayCount(m: Map<string, number>): number {
  const xs = Array.from(m.values());
  const n = xs.length;
  return n;
}
