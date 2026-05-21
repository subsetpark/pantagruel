interface Account {
  balance: number;
}

/** Counter loop with break; break-bearing bounded loops route to fixed-point. */
export function addUntilLimit(a: Account, n: number): void {
  for (let i = 0; i < n; i++) {
    a.balance = a.balance + 1;
    if (a.balance >= n) {
      break;
    }
  }
}
