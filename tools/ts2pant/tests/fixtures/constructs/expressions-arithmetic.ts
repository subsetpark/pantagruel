// Arithmetic operators: +, -, *, /, %, **

interface Account {
  balance: number;
}

/** addition */
export function add(a: number, b: number): number {
  return a + b;
}

/** subtraction */
export function diff(a: number, b: number): number {
  return a - b;
}

/** multiplication */
export function double(n: number): number {
  return n * 2;
}

/** division */
export function half(n: number): number {
  return n / 2;
}

/** modulo */
export function remainder(a: number, b: number): number {
  return a % b;
}

/** exponentiation */
export function power(a: number, b: number): number {
  return a ** b;
}

/** zero-argument constant */
export function getVersion(): number {
  return 42;
}

/** arithmetic with property access */
export function netBalance(a: Account, fee: number): number {
  return a.balance - fee;
}
