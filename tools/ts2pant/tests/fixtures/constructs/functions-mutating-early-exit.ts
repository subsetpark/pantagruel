// Early-exit if-conversion (Allen et al., POPL 1983, extended to early exits).
// `if (g) { return; }` lifts remaining statements into a `!g`-guarded branch.
// See CLAUDE.md § If-Conversion.

interface Account {
  balance: number;
  owner: string;
}

/** bare early return gates the subsequent write */
export function earlyReturnSimple(a: Account, g: boolean): void {
  if (g) {
    return;
  }
  a.balance = 1;
}

/** single-line if-return (no braces) */
export function earlyReturnNoBraces(a: Account, g: boolean): void {
  if (g) return;
  a.balance = 0;
}

/** early return followed by multiple writes */
export function earlyReturnMulti(a: Account, g: boolean, v: number, who: string): void {
  if (g) {
    return;
  }
  a.balance = v;
  a.owner = who;
}

/** early return followed by a regular if/else — both get gated under !g */
export function earlyReturnThenCond(a: Account, g: boolean, h: boolean): void {
  if (g) {
    return;
  }
  if (h) {
    a.balance = 1;
  } else {
    a.balance = 2;
  }
}

/** prior unconditional write, then early return gates later writes */
export function writeThenEarlyReturn(a: Account, g: boolean): void {
  a.balance = 10;
  if (g) {
    return;
  }
  a.balance = a.balance + 5;
}

/** early return lives in the else-branch — continuation is the then-branch */
export function elseBranchReturn(a: Account, g: boolean, v: number): void {
  if (g) {
    a.balance = v;
  } else {
    return;
  }
}

/** both-arm case: then has writes, else is bare return, followed by post-if */
export function elseReturnThenMore(a: Account, g: boolean, v: number): void {
  if (g) {
    a.balance = v;
  } else {
    return;
  }
  a.owner = "done";
}

/** chained early returns: second guard must still trigger if-conversion */
export function chainedEarlyReturns(a: Account, g: boolean, h: boolean): void {
  if (g) {
    return;
  }
  if (h) {
    return;
  }
  a.balance = 1;
}
