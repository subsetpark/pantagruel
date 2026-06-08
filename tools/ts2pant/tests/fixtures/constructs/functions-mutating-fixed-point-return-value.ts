// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  balance: number;
}

/** Unbounded fixed-point while with return value via the return-value location. */
export function climbAndReturn(a: Account, n: number): number {
  while (a.balance < n) {
    a.balance = a.balance + 1;
    if (a.balance >= n) {
      return a.balance;
    }
  }
}
