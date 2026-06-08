// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  balance: number;
}

/** Bounded while with break; break bumps the otherwise bounded shape to L4. */
export function addWhileBelowLimit(a: Account, n: number): void {
  let i = 0;
  while (i < n) {
    a.balance = a.balance + 1;
    if (a.balance >= n) {
      break;
    }
    i++;
  }
}
