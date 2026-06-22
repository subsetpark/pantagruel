// @archlint.module exempt
// @archlint.exempt-reason test-support

// For-of build-list shapes targeted by the Patch 1/2/3 for-of comprehension
// workstream. These functions stay unexported so the auto-discovered
// constructs snapshot suite does not pick them up before the dedicated tests
// land.

interface Item {
  active: boolean;
  label: string;
  value: number;
}

/**
 * Imperative map shape:
 *   `const acc = []; for (const x of xs) acc.push(f(x)); return acc;`
 * Target Pantagruel encoding: `each x in xs | f(x)`.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function mapLabels(xs: Item[]): string[] {
  const acc: string[] = [];
  for (const x of xs) {
    acc.push(x.label);
  }
  return acc;
}

/**
 * Imperative filter shape with a leading guard:
 *   `const acc = []; for (const x of xs) { if (P) acc.push(x); } return acc;`
 * Target Pantagruel encoding: `each x in xs | x, P`.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function filterIfActive(xs: Item[]): Item[] {
  const acc: Item[] = [];
  for (const x of xs) {
    if (x.active) {
      acc.push(x);
    }
  }
  return acc;
}

/**
 * Imperative filter shape with `continue`:
 *   `const acc = []; for (const x of xs) { if (!P) continue; acc.push(x); } return acc;`
 * Target Pantagruel encoding: `each x in xs | x, P`.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function filterContinueActive(xs: Item[]): Item[] {
  const acc: Item[] = [];
  for (const x of xs) {
    if (!x.active) {
      continue;
    }
    acc.push(x);
  }
  return acc;
}

/**
 * Non-array iterable build-list source (`Set<T>`).
 * Target Pantagruel encoding: `each x in xs | x`.
 *
 * @pant collectSet xs = (each x in xs | x)
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function collectSet(xs: Set<string>): string[] {
  const acc: string[] = [];
  for (const x of xs) {
    acc.push(x);
  }
  return acc;
}

/**
 * Out-of-scope control: a scalar fold with `+=` must keep refusing.
 * Target Pantagruel encoding: none (explicitly out of scope).
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function sumLengths(xs: string[]): number {
  let total = 0;
  for (const x of xs) {
    total += x.length;
  }
  return total;
}

/**
 * Out-of-scope control: Map-entry destructuring must keep refusing.
 * Target Pantagruel encoding: none (explicitly out of scope).
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function mapEntryCopy(m: Map<string, number>): number[] {
  const acc: number[] = [];
  for (const [k, v] of m) {
    void k;
    acc.push(v);
  }
  return acc;
}
