/**
 * Document-wide name registry ensuring unique variable names across
 * all rule declarations in a generated Pantagruel document.
 *
 * Each rule introduces its own bindings. No two rules should share
 * a parameter name, since they represent distinct variables that may
 * have different guards or invariants.
 */
export class NameRegistry {
  private used = new Set<string>();

  /**
   * Register a name. If already used, appends numeric suffixes (1, 2, ...)
   * until unique. Returns the actual name used.
   */
  register(name: string): string {
    if (!this.used.has(name)) {
      this.used.add(name);
      return name;
    }
    let suffix = 1;
    while (this.used.has(`${name}${suffix}`)) {
      suffix++;
    }
    const actual = `${name}${suffix}`;
    this.used.add(actual);
    return actual;
  }

  /** Check whether a name is already registered. */
  isUsed(name: string): boolean {
    return this.used.has(name);
  }
}
