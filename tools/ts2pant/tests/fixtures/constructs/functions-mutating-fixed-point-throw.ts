interface Account {
  balance: number;
}

/** Unbounded fixed-point while with throw lowered as an iteration precondition. */
export function climbUnlessInvalid(a: Account, n: number): void {
  while (a.balance < n) {
    if (n === 0) {
      throw "invalid";
    }
    a.balance = a.balance + 1;
  }
}
