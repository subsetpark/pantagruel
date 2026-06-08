// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  balance: number;
}

/** while(true) event-loop idiom; the reachable break is the termination signal. */
export function eventLoopUntilLimit(a: Account, n: number): void {
  while (true) {
    a.balance = a.balance + 1;
    if (a.balance >= n) {
      break;
    }
  }
}
