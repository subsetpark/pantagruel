// @archlint.module exempt
// @archlint.exempt-reason test-support

import type { DependencyItem } from "../foreign-dependency/index.js";

interface Account {
  balance: number;
}

/**
 * PENDING Patch 3: `item.isLabeled()` should lower as an uninterpreted Bool
 * rule head (`is-labeled item`) used as the cond antecedent.
 *
 * @pant foreign-call-early-return item = cond is-labeled item => 1, true => 0.
 */
export function foreignCallEarlyReturn(item: DependencyItem): number {
  if (item.isLabeled()) {
    return 1;
  }
  return 0;
}

/**
 * PENDING Patch 3: both declaration-file Bool method calls should lower as
 * EUF Bool rule heads, with the disjunction used as the cond antecedent.
 *
 * @pant foreign-call-compound-predicate item = cond is-labeled item or has-ready-flag item => 1, true => 0.
 */
export function foreignCallCompoundPredicate(item: DependencyItem): number {
  if (item.isLabeled() || item.hasReadyFlag()) {
    return 1;
  }
  return 0;
}

/**
 * PENDING Patch 3: the counter-dependent declaration-file Bool method call
 * should be admitted at the pure μ-search predicate gate and lower as an EUF
 * Bool rule head inside the bounded-search guard.
 */
export function foreignCallWhilePredicate(item: DependencyItem): number {
  let i = 0;
  while (item.hasIndex(i)) {
    i++;
  }
  return i;
}

/** NEGATIVE Decision B: mutating conditions must not admit foreign Bool calls. */
export function foreignCallMutatingIf(
  account: Account,
  item: DependencyItem,
): void {
  if (item.isLabeled()) {
    account.balance = account.balance + 1;
  }
}

/** NEGATIVE out of scope: non-Bool foreign value calls stay rejected. */
export function foreignNonBoolCallPredicate(item: DependencyItem): number {
  if (item.getLabel()) {
    return 1;
  }
  return 0;
}
