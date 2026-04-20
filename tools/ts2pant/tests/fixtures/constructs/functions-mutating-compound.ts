// Compound assignment operators desugar to read-modify-write on the
// same location. `a.p OP= v` is translated as `a.p = a.p OP v`, which
// lets the rhs read the prior-write value (or pre-state identity)
// through the symbolic state.

interface Account {
  balance: number;
  count: number;
}

/** single compound assignment: a.balance += amount */
export function addAmount(a: Account, amount: number): void {
  a.balance += amount;
}

/** compound subtraction */
export function subtractAmount(a: Account, amount: number): void {
  a.balance -= amount;
}

/** compound multiplication */
export function scaleBalance(a: Account, factor: number): void {
  a.balance *= factor;
}

/** two compound assigns on distinct props */
export function bumpBoth(a: Account, amount: number): void {
  a.balance += amount;
  a.count += 1;
}

/** compound assign inside a conditional — sees prior unconditional write */
export function conditionalBump(a: Account, amount: number, g: boolean): void {
  a.balance = 0;
  if (g) {
    a.balance += amount;
  }
}
