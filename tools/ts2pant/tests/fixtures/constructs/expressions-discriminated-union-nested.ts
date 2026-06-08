// @archlint.module exempt
// @archlint.exempt-reason test-support

type Leaf =
  | { tag: "num"; value: number; score: number }
  | { tag: "text"; text: string; score: number };

type Envelope =
  | { kind: "leaf"; leaf: Leaf }
  | { kind: "empty"; count: number };

interface RecordHolder {
  leaf: Leaf;
}

interface MapHolder {
  byId: Map<string, Leaf>;
}

interface ArrayHolder {
  items: Leaf[];
}

export function nestedVariantFieldNarrowed(e: Envelope): number {
  if (e.kind === "leaf") return e.leaf.score;
  return 0;
}

export function recordFieldNarrowed(h: RecordHolder): number {
  if (h.leaf.tag === "num") return h.leaf.value;
  return 0;
}

export function mapValueNarrowed(_h: MapHolder, leaf: Leaf): number {
  if (leaf.tag === "num") return leaf.value;
  return 0;
}

export function arrayElementNarrowed(_h: ArrayHolder, leaf: Leaf): number {
  if (leaf.tag === "num") return leaf.value;
  return 0;
}

export function recordFieldUnchecked(h: RecordHolder): number {
  return h.leaf.value;
}
