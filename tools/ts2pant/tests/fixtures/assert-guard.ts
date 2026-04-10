function assert(condition: unknown, msg?: string): asserts condition {
  if (!condition) throw new Error(msg ?? "Assertion failed");
}

interface Account {
  balance: number;
}

/**
 * Deposit funds, guarded by assertion.
 * @pant balance' account > balance account
 */
function deposit(account: Account, amount: number): void {
  assert(amount > 0, "Amount must be positive");
  assert(account.balance >= 0, "Balance must be non-negative");
  account.balance = account.balance + amount;
}
