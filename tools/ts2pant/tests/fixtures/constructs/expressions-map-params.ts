// Map<K, V> in non-field type positions synthesizes a domain handle per
// (K, V) pair per module. McCarthy's theory of arrays (Kroening & Strichman
// Ch. 7): synthesized sort is the handle domain; distinct parameter values
// are distinct elements; .get is a partial function guarded by .has
// membership (Dafny-style precondition discipline).

/** Single Map parameter. */
export function lookup(m: Map<string, number>, k: string): number {
  return m.get(k)!;
}

/** .has on a Map parameter -> synthesized key predicate. */
export function contains(m: Map<string, number>, k: string): boolean {
  return m.has(k);
}

/**
 * Two parameters of the *same* Map type: one shared synthesized domain.
 * Two distinct elements; EUF keeps their lookups independent. The
 * annotation exercises both guards together.
 * @pant all m1: StringToIntMap, m2: StringToIntMap, k: String | string-to-int-map-key m1 k and string-to-int-map-key m2 k -> sumAt m1 m2 k = string-to-int-map m1 k + string-to-int-map m2 k
 */
export function sumAt(
  m1: Map<string, number>,
  m2: Map<string, number>,
  k: string,
): number {
  return m1.get(k)! + m2.get(k)!;
}

/** Two different Map types -> two independent synthesized domains. */
export function pickByFlag(
  flags: Map<string, boolean>,
  values: Map<string, number>,
  k: string,
): number {
  return flags.has(k) ? values.get(k)! : 0;
}

/** Compound V: domain name StringToListStringMap; return type [String]. */
export function tagsFor(tags: Map<string, string[]>, k: string): string[] {
  return tags.get(k)!;
}

/** Map in return position (identity): domain flows through signature. */
export function passThrough(m: Map<string, number>): Map<string, number> {
  return m;
}

/**
 * Nested Map parameter: outer K=String, V=Map<string, number>. Synthesis
 * is bottom-up: inner StringToIntMap registers first, then the outer
 * StringToStringToIntMapMap whose V references it. The body does a
 * double .get(k)!, composing two synthesized guarded rules.
 */
export function nestedLookup(
  m: Map<string, Map<string, number>>,
  k1: string,
  k2: string,
): number {
  return m.get(k1)!.get(k2)!;
}
