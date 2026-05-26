export const OPAQUE_DOMAIN = "Opaque";

/**
 * Return the Pant-safe nullary rule name for one opaque value identity.
 *
 * The encoding is injective over JavaScript UTF-16 strings: it records the
 * code-unit length and every code unit in fixed-width hexadecimal form.
 */
export function opaqueValueRuleName(id: string): string {
  const codeUnits: string[] = [];
  for (let i = 0; i < id.length; i += 1) {
    codeUnits.push(id.charCodeAt(i).toString(16).padStart(4, "0"));
  }
  return codeUnits.length === 0
    ? "opaque_value_0"
    : `opaque_value_${id.length}_${codeUnits.join("_")}`;
}
