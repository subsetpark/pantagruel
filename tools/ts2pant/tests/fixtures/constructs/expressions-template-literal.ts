// Template literal lowering. Pant's `+` overload (PR #170) gives us
// `(String, String) -> String` lowering to `(str.++ a b)` in SMT.
// ts2pant lowers TS template literals into a left-folded `+` chain.
// Non-string substitutions route through TS_PRELUDE EUF rules
// (`int-to-string`, `real-to-string`); Bool inlines a cond.

/** Pure string substitution: no stringify wrapping. */
export function joinKey(k: string, v: string): string {
  return `${k}|${v}`;
}

/** Int substitution: routes through int-to-string + adds TS_PRELUDE import. */
export function labelCount(n: number): string {
  return `count: ${n}`;
}

/** Multiple substitutions, mixed text. */
export function fmtPair(label: string, n: number): string {
  return `[${label}=${n}]`;
}

/** Bool substitution: inline cond, no TS_PRELUDE needed. */
export function flagLine(active: boolean): string {
  return `flag=${active}`;
}

/** Adjacent substitutions with empty separator. */
export function concatTwo(a: string, b: string): string {
  return `${a}${b}`;
}

/** Single string substitution: degenerates to the substitution. */
export function wrapId(s: string): string {
  return `${s}`;
}

/** Substituting a list-typed value: rejected — Pantagruel has no
 * `[Int] -> String` stringify rule, and the user should be explicit
 * about how to format a collection. Verifies the unsupported-type
 * branch in the recognizer fires. */
export function rejectListSubstitution(xs: number[]): string {
  return `count: ${xs}`;
}
