// @archlint.module exempt
// @archlint.exempt-reason test-support

// Iteration-builder shapes targeted by the Patch 2/3/4 iteration builder
// completeness work. These functions stay unexported so the generic constructs
// snapshot suite does not discover them before dedicated tests are unskipped.

interface IterationItem {
  active: boolean;
  ready: boolean;
  id: string;
  label: string;
  count: number;
}

/**
 * For-of list builder with a loop-local const projection before the push.
 * Target Pantagruel encoding: `each item in items | item--label item`.
 *
 * @pant list-const-projection items = (each item in items | iteration-item--label item).
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listConstProjection(items: IterationItem[]): string[] {
  const out: string[] = [];
  for (const item of items) {
    const projected = item.label;
    out.push(projected);
  }
  return out;
}

/**
 * For-of list builder with a compound guard.
 * Target Pantagruel encoding folds both conjuncts into the comprehension guard.
 *
 * @pant list-compound-guard items = (each item in items, iteration-item--active item, iteration-item--ready item | item).
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listCompoundGuard(items: IterationItem[]): IterationItem[] {
  const out: IterationItem[] = [];
  for (const item of items) {
    if (item.active && item.ready) {
      out.push(item);
    }
  }
  return out;
}

/**
 * For-of list builder with nested guard blocks.
 * Target Pantagruel encoding folds both nested guards into the comprehension.
 *
 * @pant list-nested-guard items = (each item in items, iteration-item--active item, iteration-item--ready item | iteration-item--id item).
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listNestedGuard(items: IterationItem[]): string[] {
  const out: string[] = [];
  for (const item of items) {
    if (item.active) {
      if (item.ready) {
        out.push(item.id);
      }
    }
  }
  return out;
}

/**
 * For-of Set builder.
 * Target Pantagruel encoding: membership equivalence over a `some`
 * comprehension, not ordered list equality.
 *
 * @pant all x: String | x in set-add-for-of items <-> (some item in items | x = iteration-item--id item).
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function setAddForOf(items: IterationItem[]): Set<string> {
  const out = new Set<string>();
  for (const item of items) {
    out.add(item.id);
  }
  return out;
}

/**
 * Pure additive scalar fold.
 * Target Pantagruel encoding: identity plus `+ over each`.
 *
 * @pant sum-int-fold items = + over each item in items | iteration-item--count item.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function sumIntFold(items: IterationItem[]): number {
  let total = 0;
  for (const item of items) {
    total += item.count;
  }
  return total;
}

/**
 * Guarded count fold.
 * Target Pantagruel encoding: guarded `+ over each` of one per matching item.
 *
 * @pant count-guarded-fold items = + over each item in items, iteration-item--active item | 1.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function countGuardedFold(items: IterationItem[]): number {
  let count = 0;
  for (const item of items) {
    if (item.active) {
      count++;
    }
  }
  return count;
}

/**
 * Boolean-and scalar fold.
 * Target Pantagruel encoding: `and over each`.
 *
 * @pant all-active-fold items = and over each item in items | iteration-item--active item.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function allActiveFold(items: IterationItem[]): boolean {
  let allActive = true;
  for (const item of items) {
    allActive &&= item.active;
  }
  return allActive;
}

/**
 * Out-of-scope control: early return exits the outer function from inside the
 * loop, so it is not a pure build-list comprehension.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listEarlyReturnInLoopRejected(items: IterationItem[]): string[] {
  const out: string[] = [];
  for (const item of items) {
    if (!item.active) {
      return out;
    }
    out.push(item.id);
  }
  return out;
}

/**
 * Out-of-scope control: break makes the result prefix-dependent.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listBreakRejected(items: IterationItem[]): string[] {
  const out: string[] = [];
  for (const item of items) {
    if (!item.ready) {
      break;
    }
    out.push(item.id);
  }
  return out;
}

/**
 * Out-of-scope control: nested loops are not a single-source builder.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listNestedLoopRejected(items: IterationItem[][]): string[] {
  const out: string[] = [];
  for (const group of items) {
    for (const item of group) {
      out.push(item.id);
    }
  }
  return out;
}

/**
 * Out-of-scope control: accumulator aliasing hides the builder write target.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listAccumulatorAliasRejected(items: IterationItem[]): string[] {
  const out: string[] = [];
  const alias = out;
  for (const item of items) {
    alias.push(item.id);
  }
  return out;
}

/**
 * Out-of-scope control: Map construction is deferred to a Map-specific target.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function mapBuilderForOfRejected(items: IterationItem[]): Map<string, number> {
  const out = new Map<string, number>();
  for (const item of items) {
    out.set(item.id, item.count);
  }
  return out;
}

/**
 * Out-of-scope control: Set delete/clear mutation is not a Set add builder.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function setDeleteForOfRejected(items: IterationItem[]): Set<string> {
  const out = new Set<string>();
  for (const item of items) {
    out.add(item.id);
    if (!item.active) {
      out.delete(item.id);
    }
  }
  return out;
}

/**
 * Out-of-scope control: Set accumulator aliases must not flow into the lowered
 * prelude around the loop builder.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function setAccumulatorAliasForOfRejected(items: IterationItem[]): Set<string> {
  const out = new Set<string>();
  const alias = out;
  for (const item of items) {
    alias.add(item.id);
  }
  return out;
}

/**
 * Out-of-scope control: Set builder projections may not read the accumulator
 * they are defining.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function setAccumulatorReadForOfRejected(items: IterationItem[]): Set<number> {
  const out = new Set<number>();
  for (const _item of items) {
    out.add(out.size);
  }
  return out;
}

/**
 * Out-of-scope control: recognized for-of builder pieces may not reference a
 * binding declared later in the prelude.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function setTdzSourceForOfRejected(items: IterationItem[]): Set<string> {
  const out = new Set<string>();
  for (const item of laterItems) {
    out.add(item.id);
  }
  const laterItems = items;
  return out;
}

/**
 * Out-of-scope scalar fold: null-seeded conjoin has no identity-bearing target
 * in the current scalar fold lowering.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function conjoinNoIdentityRejected(items: IterationItem[]): boolean | null {
  let result: boolean | null = null;
  for (const item of items) {
    result = result === null ? item.active : result && item.active;
  }
  return result;
}

/**
 * Out-of-scope scalar fold: subtraction is order-dependent and
 * non-commutative.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function foldNonCommutativeRejected(items: IterationItem[]): number {
  let total = 0;
  for (const item of items) {
    total -= item.count;
  }
  return total;
}
