// @archlint.module core
// @archlint.domain ts2pant.supply

import type ts from "typescript";

import type { SourceRef } from "./ir1.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import type { SynthCell } from "./translate-types.js";

// Leaf module: the document-wide unique-id supply and the opaque-alias
// side-channel. Lives below the build pipeline (no dependency on
// translate-body / ir1-build) so any module can draw fresh ids without
// importing into an import cycle.

/**
 * Per-body translation context threaded through every `translateBodyExpr`
 * call. Carries the hygienic-binder counter and (optionally) the
 * module-wide `SynthCell` so `.get`/`.has` on non-field Map receivers
 * can resolve to the synthesized rule names.
 *
 * Mutable 2-field record: `n` is reassigned in place by `nextSupply`. This
 * is within ts2pant's self-translation envelope (cell-field reassignment
 * translates to primed rules on the cell), unlike the prior closure over a
 * `let counter = 0`.
 */
export interface UniqueSupply {
  n: number;
  synthCell?: SynthCell | undefined;
  program?: ts.Program | undefined;
  /**
   * Side-channel registry of fresh hygienic names that bind a
   * pre-built `OpaqueExpr` value. Populated by the mutating-body
   * property-write cache inside `buildL1MemberAccess` when a property read
   * resolves to a previously-recorded write — the build allocates a
   * fresh `$N` name, registers `($N → OpaqueExpr)` here, and returns
   * `Var($N)` in L1. This keeps L1 free of OpaqueExpr-bearing nodes
   * while still surfacing read-after-write semantics.
   *
   * Lower sites apply these substitutions via `applyOpaqueAliases`, so the
   * final Pantagruel text contains the recorded value rather than the `$N`
   * reference.
   */
  opaqueAliases?: Map<string, OpaqueExpr>;
}

export function makeUniqueSupply(
  synthCell?: SynthCell,
  program?: ts.Program,
): UniqueSupply {
  return { n: 0, synthCell, program };
}

/**
 * Register a fresh hygienic name as an alias for a pre-built
 * `OpaqueExpr`. The alias is consumed by `applyOpaqueAliases` at
 * lower-to-opaque sites and substituted out before Pantagruel
 * emission, so the alias name never reaches the parser.
 *
 * The stored value is *eagerly resolved* against any aliases already
 * in the map: if `value` itself contains references to earlier
 * aliases, those get substituted out before insertion.
 * This keeps the alias map flat — no `B → Var(A)` chain pointing at
 * another alias — so `applyOpaqueAliases` can substitute in a single
 * pass instead of running to fixpoint.
 */
export function registerOpaqueAlias(
  supply: UniqueSupply,
  name: string,
  value: OpaqueExpr,
): void {
  if (supply.opaqueAliases === undefined) {
    supply.opaqueAliases = new Map();
  }
  supply.opaqueAliases.set(name, applyOpaqueAliases(value, supply));
}

/**
 * Apply every alias in `supply.opaqueAliases` to `expr` via Pant's
 * capture-avoiding `substituteBinder`. Idempotent for fresh
 * expressions that contain no alias references; otherwise replaces
 * each `Var(aliasName)` occurrence with the registered OpaqueExpr.
 *
 * Single-pass — alias values are flattened at registration time
 * (`registerOpaqueAlias` resolves prior aliases before storing), so
 * the map never carries `B → Var(A)` chains that would require a
 * fixpoint iteration here.
 */
export function applyOpaqueAliases(
  expr: OpaqueExpr,
  supply: UniqueSupply | undefined,
): OpaqueExpr {
  if (supply?.opaqueAliases === undefined || supply.opaqueAliases.size === 0) {
    return expr;
  }
  const ast = getAst();
  let r = expr;
  for (const [name, value] of supply.opaqueAliases) {
    r = ast.substituteBinder(r, name, value);
  }
  return r;
}

export function nextSupply(supply: UniqueSupply): number {
  const value = supply.n;
  supply.n = value + 1;
  return value;
}

export function freshHygienicBinder(supply: UniqueSupply): string {
  return `$${nextSupply(supply)}`;
}

// Synthetic file name for origins minted when no real source position can be
// recovered. Combined with a fresh supply counter it yields a unique opaque
// identity (see `ir1OpaqueOriginId`), never colliding with another value.
export const SYNTHETIC_ORIGIN_FILE = "<synthetic>";

/**
 * Mint a guaranteed-unique `SourceRef` for a node with no recoverable source
 * position (e.g. a factory-synthesized SSA desugaring that carries no
 * original node). Uniqueness via the supply counter keeps distinct opaque
 * values from collapsing onto one Pant identity.
 */
export function freshSyntheticOrigin(supply: UniqueSupply): SourceRef {
  return { file: SYNTHETIC_ORIGIN_FILE, line: nextSupply(supply) };
}
