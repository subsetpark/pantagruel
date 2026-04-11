// Control flow: ternary, if/else with returns

interface Account {
  active: boolean;
  balance: number;
}

/** ternary → cond */
export function max(a: number, b: number): number {
  return a >= b ? a : b;
}

/** ternary with property access in condition */
export function effectiveBalance(a: Account): number {
  return a.active ? a.balance : 0;
}

/** if/else with returns → cond */
export function abs(n: number): number {
  if (n >= 0) {
    return n;
  } else {
    return 0 - n;
  }
}
