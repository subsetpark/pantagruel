// @archlint.module exempt
// @archlint.exempt-reason test-support

type Shape =
  | { kind: "circle"; r: number; shared: string }
  | { kind: "square"; s: number; shared: string };

/**
 * @pant getRadius s = cond shape--kind s = "circle" => shape--r s, true => 0.
 */
export function getRadius(s: Shape): number {
  if (s.kind === "circle") return s.r;
  return 0;
}

/**
 * @pant dispatchOnKind s = cond shape--kind s = "circle" => shape--r s, shape--kind s = "square" => shape--s s, true => 0.
 */
export function dispatchOnKind(s: Shape): number {
  switch (s.kind) {
    case "circle":
      return s.r;
    case "square":
      return s.s;
    default:
      return 0;
  }
}

/**
 * @pant circleOnly s = cond shape--kind s = "circle" => shape--r s, true => 0.
 */
export function circleOnly(s: Shape): number {
  if (s.kind !== "circle") return 0;
  return s.r;
}
