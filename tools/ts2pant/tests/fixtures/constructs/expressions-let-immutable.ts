// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  balance: number;
}

export function simpleLet(a: number, b: number): number {
  let x = a + b;
  return x;
}

export function chainedLet(a: number): number {
  let x = a;
  let y = x + 1;
  return y;
}

export function letWithPropAccess(a: Account): number {
  let b = a.balance;
  return b + 1;
}

export function letInTernary(a: number, b: number): number {
  let x = a + b;
  return x > 0 ? x : 0;
}

export function letMathMax(a: number, b: number): number {
  let m = Math.max(a, b);
  return m + 1;
}
