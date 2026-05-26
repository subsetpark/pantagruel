interface Account {
  balance: number;
  fee: number;
}

export function depositWithImmutableLet(a: Account, amount: number): void {
  let newBal = a.balance + amount;
  a.balance = newBal;
}

export function depositWithReassignedLet(a: Account, amount: number): void {
  let newBal = a.balance;
  newBal += amount;
  a.balance = newBal;
}

export function depositWithBranchLet(
  a: Account,
  amount: number,
  chargeFee: boolean,
): void {
  let delta = amount;
  if (chargeFee) {
    delta = amount - a.fee;
  }
  a.balance += delta;
}
