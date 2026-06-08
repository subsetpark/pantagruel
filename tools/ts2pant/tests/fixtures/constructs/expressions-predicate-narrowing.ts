// @archlint.module exempt
// @archlint.exempt-reason test-support

function isNumber(x: number | null): x is number {
  return true;
}

/**
 * @pant predicate-value x fallback = cond is-number x => x 1, true => fallback.
 */
export function predicateValue(x: number | null, fallback: number): number {
  if (isNumber(x)) return x;
  return fallback;
}

type Shape = { kind: "circle"; r: number } | { kind: "square"; s: number };

function isCircle(s: Shape): s is Extract<Shape, { kind: "circle" }> {
  return false;
}

/**
 * @pant shape--kind s = "circle".
 */
export function predicateCircleRadius(s: Shape): number {
  if (isCircle(s)) return s.r;
  return 0;
}
