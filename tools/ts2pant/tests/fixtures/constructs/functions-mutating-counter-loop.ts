// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  total: number;
  lastIndex: number;
}

/**
 * Canonical bounded counter fold: the recognizer accepts the literal-zero
 * `let` init, strict `i < n` guard, and `i++` step, then lowers the mutation to
 * `account--total' a = account--total a + (+ over each i: Nat0, i >= 0,
 * i < n | i)`.
 */
export function sumFirstN(a: Account, n: number): void {
  for (let i = 0; i < n; i++) {
    a.total += i;
  }
}

/**
 * Guarded bounded counter fold: the inner counter-dependent `if` is folded
 * into the over-each guard list, yielding
 * `+ over each i: Nat0, i >= 0, i < n, i > 0`.
 */
export function sumIfPositiveFirstN(a: Account, n: number): void {
  for (let i = 0; i < n; i++) {
    if (i > 0) {
      a.total += i;
    }
  }
}

/**
 * Simple counter-only assignment: the body does not read `a.lastIndex`, so the
 * loop lowers to last-iteration-wins `cond n > 0 => n - 1, true => prior`.
 */
export function setLastIndex(a: Account, n: number): void {
  for (let i = 0; i < n; i++) {
    a.lastIndex = i;
  }
}
