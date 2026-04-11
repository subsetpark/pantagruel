// Guard detection: assertion calls with `asserts` return type

interface Account {
  balance: number;
}

function assert(condition: unknown, msg?: string): asserts condition {
  if (!condition) {
    throw new Error(msg ?? "Assertion failed");
  }
}

/** single assertion guard */
export function deposit(account: Account, amount: number): void {
  assert(amount > 0);
  account.balance = account.balance + amount;
}

/** multiple assertion guards → combined */
export function multiAssert(account: Account, amount: number): void {
  assert(amount > 0);
  assert(account.balance >= 0);
  account.balance = account.balance + amount;
}

/** mixed: assertion + if-throw guard */
export function mixedGuard(account: Account, amount: number): void {
  if (amount <= 0) {
    throw new Error("Amount must be positive");
  }
  assert(account.balance >= 0);
  account.balance = account.balance + amount;
}
