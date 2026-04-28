// Miscellaneous expressions: parenthesized, type assertions, non-null, unary minus

/** parenthesized expression */
export function parenAdd(a: number, b: number): number {
  return (a + b);
}

/** type assertion (unwrapped) */
export function asNumber(x: number): number {
  return x as number;
}

/** non-null assertion (unwrapped) */
export function nonNull(x: number | null): number {
  return x!;
}

/** unary minus */
export function negate(n: number): number {
  return -n;
}
