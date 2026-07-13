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

/**
 * Field-reader on a recursive DU: reads the `leaf`-variant `value` field,
 * discharged by the `kind === "leaf"` discriminant narrowing (same shape as
 * the non-recursive `getRadius`, now over a self-referential domain).
 *
 * @pant leafValue t = cond tree--kind t = "leaf" => tree--value t, true => 0.
 */
export function leafValue(t: Tree): number {
  if (t.kind === "leaf") {
    return t.value;
  }
  return 0;
}

/**
 * Reads a self-referential field (`node.left: Tree`) under narrowing, then its
 * discriminant — exercising the recursive accessor `tree--left … => Tree`.
 *
 * @pant leftChildKind t = cond tree--kind t = "node" => tree--kind (tree--left t), true => tree--kind t.
 */
export function leftChildKind(t: Tree): string {
  if (t.kind === "node") {
    return t.left.kind;
  }
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

// Mirrors the essential `IR1ForeachCondStmt` / `IR1ForeachBody` shape without
// importing the production IR1 types. The recursive branch is nullable so the
// body field exercises the reserved-domain knot, while the tuple fields mirror
// the required-head/rest variadics that Patch 2 and Patch 3 split apart.
type ForeachCondStmt = {
  kind: "cond-stmt";
  arms: readonly [
    readonly [string, ForeachBody],
    ...ReadonlyArray<readonly [string, ForeachBody]>,
  ];
  otherwise: ForeachBody | null;
};

type ForeachBody =
  | { kind: "assign"; value: number }
  | ForeachCondStmt
  | {
      kind: "block";
      stmts: readonly [ForeachBody, ...ForeachBody[]];
    };

/**
 * @pant foreachBodyKind body = foreach-body--kind body.
 */
export function foreachBodyKind(body: ForeachBody): string {
  return body.kind;
}
