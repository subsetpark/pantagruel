// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  balance: number;
}

/** Unbounded fixed-point while with break consumed by the post-loop join. */
export function climbUntilBreak(a: Account, n: number): void {
  while (a.balance < n) {
    a.balance = a.balance + 1;
    if (a.balance >= n) {
      break;
    }
  }
}
