interface Account {
  balance: number;
}

/**
 * Deposit funds into an account.
 * @pant balance' account > balance account
 */
function deposit(account: Account, amount: number): void {
  if (amount <= 0) {
    throw new Error("Amount must be positive");
  }
  account.balance = account.balance + amount;
}
