// @archlint.module exempt
// @archlint.exempt-reason test-support

// Fixtures for the monomorphization-survey classifier. Each exported function
// has a top-level `any` parameter (a candidate); `driver` supplies call sites
// with typed arguments so each verdict branch is exercised.

export function allAgree(x: any): number {
  return x ? 1 : 0;
}

export function bounded(x: any): number {
  return x ? 1 : 0;
}

export function unbounded(x: any): number {
  return x ? 1 : 0;
}

export function recursiveFn(x: any): number {
  return recursiveFn(x);
}

export function uncalled(x: any): number {
  return x ? 1 : 0;
}

export function oversizedFn(x: any): number {
  // padding to exceed the OVERSIZED_LOC (30) body-line threshold:
  //  1
  //  2
  //  3
  //  4
  //  5
  //  6
  //  7
  //  8
  //  9
  // 10
  // 11
  // 12
  // 13
  // 14
  // 15
  // 16
  // 17
  // 18
  // 19
  // 20
  // 21
  // 22
  // 23
  // 24
  // 25
  // 26
  // 27
  // 28
  // 29
  // 30
  return x ? 1 : 0;
}

const s1: string = "a";
const s2: string = "b";
const n1: number = 1;
const b1: boolean = true;

export function driver(): number {
  return (
    allAgree(s1) +
    allAgree(s2) +
    bounded(s1) +
    bounded(n1) +
    unbounded(s1) +
    unbounded(n1) +
    unbounded(b1) +
    unbounded({}) +
    oversizedFn(s1)
  );
}
