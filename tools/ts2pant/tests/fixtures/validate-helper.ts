interface Account {
  balance: number;
}

function validateAmount(amount: number): void {
  if (amount <= 0) {
    throw new Error("Amount must be positive");
  }
}

function requireNonNegativeBalance(account: Account): void {
  if (account.balance < 0) {
    throw new Error("Balance must be non-negative");
  }
}

/**
 * Deposit funds, with guards in helper functions.
 * @pant balance' account >= 0
 */
function deposit(account: Account, amount: number): void {
  validateAmount(amount);
  requireNonNegativeBalance(account);
  account.balance = account.balance + amount;
}
