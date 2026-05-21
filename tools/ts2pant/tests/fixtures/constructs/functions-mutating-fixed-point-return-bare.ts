interface Account {
  balance: number;
}

/** Unbounded fixed-point while with bare return as a function-level exit. */
export function climbUntilReturn(a: Account, n: number): void {
  while (a.balance < n) {
    a.balance = a.balance + 1;
    if (a.balance >= n) {
      return;
    }
  }
}
