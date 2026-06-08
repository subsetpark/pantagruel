// @archlint.module exempt
// @archlint.exempt-reason test-support

export function letThenOnlyBranch(a: number, b: number, cond: boolean): number {
  let x = a;
  if (cond) {
    x = b;
  }
  return x;
}

export function letThenElseBranch(
  a: number,
  b: number,
  c: number,
  cond: boolean,
): number {
  let x = a;
  if (cond) {
    x = b;
  } else {
    x = c;
  }
  return x;
}

export function letNestedBranch(
  a: number,
  b: number,
  c: number,
  outer: boolean,
  inner: boolean,
): number {
  let x = a;
  if (outer) {
    if (inner) {
      x = b;
    } else {
      x = c;
    }
  }
  return x;
}
