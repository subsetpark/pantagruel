// @archlint.module exempt
// @archlint.exempt-reason test-support

// `Tree` is a self-referential discriminated union (the `node` variant's
// `left`/`right` fields are `Tree`). Mapping it used to recurse forever in the
// DU synthesis (stack overflow); it must now refuse gracefully.
type Tree =
  | { kind: "leaf"; value: number }
  | { kind: "node"; left: Tree; right: Tree };

export function treeKind(t: Tree): string {
  return t.kind;
}

// `Shape` is a NON-recursive discriminated union — the control. It must keep
// registering to a synthesized domain (the cycle guard never fires for it).
type Shape =
  | { kind: "circle"; r: number }
  | { kind: "square"; s: number };

export function shapeKind(s: Shape): string {
  return s.kind;
}
