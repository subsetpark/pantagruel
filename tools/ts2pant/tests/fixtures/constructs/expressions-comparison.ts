// Comparison operators: ===, !==, >=, <=, >, <

/** strict equality → = */
export function eq(a: number, b: number): boolean {
  return a === b;
}

/** strict inequality → ~= */
export function neq(a: number, b: number): boolean {
  return a !== b;
}

/** greater than or equal */
export function gte(a: number, b: number): boolean {
  return a >= b;
}

/** less than or equal */
export function lte(a: number, b: number): boolean {
  return a <= b;
}

/** greater than */
export function gt(a: number, b: number): boolean {
  return a > b;
}

/** less than */
export function lt(a: number, b: number): boolean {
  return a < b;
}
