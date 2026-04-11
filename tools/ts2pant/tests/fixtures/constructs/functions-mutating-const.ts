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
