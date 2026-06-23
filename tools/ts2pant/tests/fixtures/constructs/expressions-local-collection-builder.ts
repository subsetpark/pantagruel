// @archlint.module exempt
// @archlint.exempt-reason test-support

// Local collection builder shapes targeted by the Patch 2/3 local collection
// builder sequencing work. These functions stay unexported so the generic
// constructs snapshot suite does not discover them before dedicated tests are
// unskipped.

interface BuilderItem {
  id: string;
  label: string;
  rank: number;
}

/**
 * Straight-line single-push list builder:
 *   `const out = []; out.push(seed); return out;`
 * Target Pantagruel encoding: cardinality plus positional assertions.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listSinglePush(seed: string): string[] {
  const out: string[] = [];
  out.push(seed);
  return out;
}

/**
 * Straight-line multi-push list builder.
 * Target Pantagruel encoding must preserve push order in positional
 * assertions over the returned rule.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listMultiplePushes(first: string, second: string): string[] {
  const out: string[] = [];
  out.push(first);
  out.push(second);
  return out;
}

/**
 * Push values through preceding pure const bindings and property projection.
 * Target Pantagruel encoding should inline or name the const-bound projection
 * without treating `push` as a pure expression.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listPushConstProjection(item: BuilderItem): string[] {
  const projected = item.label;
  const out: string[] = [];
  out.push(projected);
  return out;
}

/**
 * Straight-line Set add builder.
 * Target Pantagruel encoding: membership equivalence, not ordered positions.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function setAddBuilder(first: string, second: string): Set<string> {
  const out = new Set<string>();
  out.add(first);
  out.add(second);
  return out;
}

/**
 * Out-of-scope control: an alias escapes the local builder identity.
 * Target Pantagruel encoding: none (must keep refusing).
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listAliasEscapeRejected(seed: string): string[] {
  const out: string[] = [];
  const alias = out;
  alias.push(seed);
  return out;
}

/**
 * Out-of-scope control: unknown mutating methods are not collection builders.
 * Target Pantagruel encoding: none (must keep refusing).
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function listUnknownMutationRejected(seed: string): string[] {
  const out: string[] = [];
  out.unshift(seed);
  return out;
}

/**
 * Out-of-scope control for this milestone: Map construction is deferred until
 * a Map-specific plan handles the guarded key/value rule-pair contract.
 */
// biome-ignore lint/correctness/noUnusedVariables: unexported fixture corpus
function mapBuilderRejected(key: string, value: number): Map<string, number> {
  const out = new Map<string, number>();
  out.set(key, value);
  return out;
}
