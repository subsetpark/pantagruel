// @archlint.module exempt
// @archlint.exempt-reason test-support

type Shape =
  | { kind: "circle"; r: number; shared: string }
  | { kind: "square"; s: number; shared: string };

type NonDiscriminated =
  | { owner: string; left: number }
  | { owner: string; right: number };

interface HasId {
  id: number;
}

interface HasLabel {
  label: string;
}

/** PENDING Patch 2: discriminant access on a DU receiver resolves. */
export function readDiscriminant(x: Shape): string {
  return x.kind;
}

/** PENDING Patch 2: variant field access emits the guarded rule. */
export function readVariantField(x: Shape): number {
  return x.r;
}

/** PENDING Patch 2: common non-discriminant field access resolves. */
export function readSharedField(x: Shape): string {
  return x.shared;
}

/** Non-discriminated unions keep the ambiguous-owner refusal. */
export function ambiguousOwner(x: NonDiscriminated): number {
  return x.left;
}

/** Intersections keep AND-semantics field access. */
/** @pant-type x: HasId */
export function intersectionField(x: HasId & HasLabel): number {
  return x.id;
}
