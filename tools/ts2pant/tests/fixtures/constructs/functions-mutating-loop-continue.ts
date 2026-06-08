// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  lastIndex: number;
}

/** Counter loop with continue; continue-only bounded loops stay quantified. */
export function setPositiveLastIndex(a: Account, n: number): void {
  for (let i = 0; i < n; i++) {
    if (i === 0) {
      continue;
    }
    a.lastIndex = i;
  }
}
