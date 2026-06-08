// @archlint.module exempt
// @archlint.exempt-reason test-support

interface Account {
  balance: number;
}

function isPositive(n: number): boolean {
  return n > 0;
}

function isDepositAllowed(amount: number): boolean {
  const normalized = Math.max(0, amount);
  return normalized > 0;
}

let observed = 0;

function recordsObservation(n: number): boolean {
  observed = n;
  return observed > 0;
}

export function pureCallEarlyReturn(n: number): number {
  if (isPositive(n)) {
    return 1;
  }
  return 0;
}

export function pureCallMutatingIf(account: Account, amount: number): void {
  if (isDepositAllowed(amount)) {
    account.balance = account.balance + amount;
  }
}

/** effectful helper call must stay rejected */
export function effectfulCallPredicate(n: number): number {
  if (recordsObservation(n)) {
    return 1;
  }
  return 0;
}
