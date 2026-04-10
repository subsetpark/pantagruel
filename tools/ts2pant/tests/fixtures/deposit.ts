interface Account {
  balance: number;
}

/**
 * Deposit funds into an account.
 * @pant all a: Account | balance' a >= 0
 */
function deposit(account: Account, amount: number): void {
  if (amount <= 0) {
    throw new Error("Amount must be positive");
  }
  account.balance = account.balance + amount;
}
