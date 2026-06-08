// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  balance: number;
}

/** Unbounded fixed-point while with continue threaded to the loop header. */
export function climbUnlessZeroStep(a: Account, n: number): void {
  while (a.balance < n) {
    if (n === 0) {
      continue;
    }
    a.balance = a.balance + 1;
  }
}
