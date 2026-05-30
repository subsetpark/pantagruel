/**
 * @pant true.
 */
export function strictNullRead(x: number | null, d: number): number {
  if (x !== null) return x;
  return d;
}

/**
 * @pant true.
 */
export function looseNullRead(x: number | null | undefined, d: number): number {
  if (x != null) return x;
  return d;
}

/**
 * @pant true.
 */
export function strictUndefinedRead(
  x: number | undefined,
  d: number,
): number {
  if (x !== undefined) return x;
  return d;
}

/**
 * @pant true.
 */
export function longFormRead(
  x: number | null | undefined,
  d: number,
): number {
  if (x !== null && x !== undefined) return x;
  return d;
}

/**
 * @pant true.
 */
export function earlyReturnComplement(
  x: number | null | undefined,
  d: number,
): number {
  if (x == null) return d;
  return x;
}

/**
 * @pant some n: Nat | n ~= n.
 */
export function nullBranchControl(
  x: number | null | undefined,
  d: number | null,
): number | null {
  if (x == null) return x;
  return d;
}
