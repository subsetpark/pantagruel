interface Account {
  balance: number;
}

export function depositWithConst(a: Account, amount: number): void {
  const newBal = a.balance + amount;
  a.balance = newBal;
}

export function multiConstMutating(a: Account, amount: number, rate: number): void {
  const fee = amount * rate;
  a.balance = a.balance - fee;
}

/**
 * Sequential writes through a const-aliased receiver. The second statement
 * must see the first write when reading the property, so the resulting
 * `balance' a` should read `1 + 2` — not `balance a + 2`.
 */
export function aliasedSequentialWrites(a: Account): void {
  const x = a;
  x.balance = 1;
  x.balance += 2;
}
