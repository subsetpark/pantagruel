// Pure function signatures: various param/return type combinations

interface Account {
  balance: number;
}

/** single param, number return */
export function getBalance(a: Account): number {
  return a.balance;
}

/** multi param */
export function add(a: number, b: number): number {
  return a + b;
}

/** zero param */
export function getVersion(): number {
  return 42;
}

/** boolean return */
export function isPositive(n: number): boolean {
  return n > 0;
}

/** array return */
export function getNames(names: string[]): string[] {
  return names;
}
