type Shape =
  | { kind: "circle"; r: number }
  | { kind: "square"; s: number };

export function getRadius(s: Shape): number {
  if (s.kind === "circle") return s.r;
  return 0;
}

export function getOther(s: Shape): number {
  if (s.kind === "circle") return s.r;
  return s.s;
}

export function dispatch(s: Shape): number {
  switch (s.kind) {
    case "circle":
      return s.r;
    case "square":
      return s.s;
    default:
      return 0;
  }
}

export function circleOrZero(s: Shape): number {
  if (s.kind !== "circle") return 0;
  return s.r;
}

export function readUnchecked(s: Shape): number {
  return s.r;
}
