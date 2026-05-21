interface Account {
  total: number;
  lastIndex: number;
}

/** Ascending bounded while accumulator fold; mirrors sumFirstN. */
export function sumFirstNWhile(a: Account, n: number): void {
  let i = 0;
  while (i < n) {
    a.total += i;
    i++;
  }
}

/** Ascending bounded while simple assignment; mirrors setLastIndex. */
export function setLastIndexWhile(a: Account, n: number): void {
  let i = 0;
  while (i < n) {
    a.lastIndex = i;
    i++;
  }
}

/** Descending bounded while accumulator fold. */
export function sumFirstNDescending(a: Account, n: number): void {
  let i = n;
  while (i > 0) {
    a.total += i;
    i--;
  }
}

/** Descending bounded while simple assignment. */
export function setLastIndexDescending(a: Account, n: number): void {
  let i = n;
  while (i > 0) {
    a.lastIndex = i;
    i--;
  }
}

/** Deliberate reject: bare while without a preceding counter let. */
export function unboundedWhile(a: Account, n: number): void {
  while (a.lastIndex < n) {
    a.total += 1;
  }
}
