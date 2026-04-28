// String-literal element access (`obj["field"]`) collapses to the same
// canonical L1 Member as dotted access. Computed indices (`obj[k]`)
// reject with a specific unsupported reason — the deliberate-reject
// case at the bottom is skipped by the constructs runner.

interface User {
  name: string;
}

interface Account {
  balance: number;
  owner: User;
  active: boolean;
}

/** simple string-literal element access → same Member as `a.owner` */
export function getOwnerByLiteral(a: Account): User {
  return a["owner"];
}

/** nested string-literal element access composes nested Members */
export function getOwnerNameByLiteral(a: Account): string {
  return a["owner"]["name"];
}

/** string-literal element access in a ternary condition */
export function effectiveBalanceByLiteral(a: Account): number {
  return a["active"] ? a["balance"] : 0;
}

/**
 * Deliberate reject: computed element access (`obj[expr]`) cannot
 * resolve to a single qualified rule without literal-union narrowing,
 * which is deferred. The constructs runner snapshots the emitted
 * `> UNSUPPORTED: ...` line for functions whose translation reports
 * unsupported, so the rejection reason is locked into the fixture's
 * snapshot rather than dropped.
 */
export function getByDynamicKey(a: Account, k: keyof Account): unknown {
  return a[k];
}
