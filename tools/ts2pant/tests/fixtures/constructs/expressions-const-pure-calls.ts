// Fixture: const bindings initialized with pure function calls.
// These patterns exercise the purity oracle — pure calls should eventually
// be inlineable (wired in Patch 4), impure calls should remain rejected.

export function constMathMax(a: number, b: number): number {
  const m = Math.max(a, b);
  return m + 1;
}

export function constStringMethod(s: string): number {
  const i = s.indexOf("x");
  return i;
}

export function constChainedPure(x: number, y: number): number {
  const a = Math.abs(x);
  const b = Math.max(a, y);
  return b;
}

declare function unknownFn(x: number): number;

export function constImpureCall(x: number): number {
  const r = unknownFn(x);
  return r;
}
