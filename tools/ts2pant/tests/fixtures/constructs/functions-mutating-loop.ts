// Structured loop translation (Meijer et al., FPCA 1991). `for-of` and
// `.forEach` over an array become catamorphisms:
//   - Iterator-writes (Shape A): `all x in arr | p' x = v`
//   - Accumulator folds (Shape B): `p' a = p a OP (comb over each x in arr | f)`
// See CLAUDE.md § Structured Iteration.

interface User {
  active: boolean;
  score: number;
}

interface Account {
  total: number;
  balance: number;
}

interface Fee {
  amount: number;
}

interface Item {
  value: number;
  tagged: boolean;
}

/** Shape A: uniform iterator write */
export function activateAll(users: User[]): void {
  for (const u of users) {
    u.active = true;
  }
}

/** Shape A: conditional iterator write — path-merged via cond per element */
export function activateActive(users: User[]): void {
  for (const u of users) {
    if (u.score > 0) {
      u.active = true;
    }
  }
}

/** Shape B: accumulator sum via += */
export function sumAmounts(a: Account, items: Item[]): void {
  for (const x of items) {
    a.total += x.value;
  }
}

/** Shape B: accumulator deduction via -= */
export function deductFees(a: Account, fees: Fee[]): void {
  for (const f of fees) {
    a.balance -= f.amount;
  }
}

/** Shape B with guard — predicate folds into the comprehension */
export function sumPositive(a: Account, items: Item[]): void {
  for (const x of items) {
    if (x.value > 0) {
      a.total += x.value;
    }
  }
}

/** Shape A + Shape B in the same body — *independent*: the iterator write
 * and the fold read touch disjoint properties, so per-step order is
 * semantically irrelevant. */
export function mixedUpdates(a: Account, items: Item[]): void {
  for (const x of items) {
    x.tagged = true;
    a.total += x.value;
  }
}

/** forEach variant of Shape A */
export function forEachActivate(users: User[]): void {
  users.forEach((u) => {
    u.active = true;
  });
}

/** forEach variant of Shape B */
export function forEachSum(a: Account, items: Item[]): void {
  items.forEach((x) => {
    a.total += x.value;
  });
}
