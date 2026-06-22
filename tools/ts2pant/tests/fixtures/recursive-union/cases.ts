// @archlint.module exempt
// @archlint.exempt-reason test-support

// `Tree` is a self-referential discriminated union (the `node` variant's
// `left`/`right` fields are `Tree`). Mapping it once crashed (stack overflow),
// then refused gracefully; it now translates to a `Tree` domain whose
// `left`/`right` accessors return `Tree` (a sound self-referential encoding).
type Tree =
  | { kind: "leaf"; value: number }
  | { kind: "node"; left: Tree; right: Tree };

/**
 * @pant treeKind t = tree--kind t.
 */
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
