// Guard detection: if/throw patterns

interface Account {
  balance: number;
}

/** if/else-throw: positive guard (no-op then, throw else) */
export function withdraw(a: Account, amount: number): void {
  if (a.balance >= amount) {
    // no-op
  } else {
    throw new Error("Insufficient funds");
  }
  a.balance = a.balance - amount;
}

/** early-throw: negated guard (if-not-cond throw) */
export function earlyThrow(a: Account, amount: number): void {
  if (!(a.balance >= amount)) {
    throw new Error("Insufficient funds");
  }
  a.balance = a.balance - amount;
}

/** guard with variable declaration before throw (still a guard) */
export function guardWithLocals(a: Account, amount: number): void {
  if (!(a.balance >= amount)) {
    const msg = "Insufficient: " + amount;
    throw new Error(msg);
  }
  a.balance = a.balance - amount;
}

/** NOT a guard: then-branch has side effects */
export function notGuardSideEffects(a: Account, amount: number): void {
  if (amount > 1000) {
    a.balance = 0;
    throw new Error("too large");
  }
  a.balance = a.balance - amount;
}
