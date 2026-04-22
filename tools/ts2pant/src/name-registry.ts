/**
 * Document-wide name registry ensuring unique variable names across
 * all rule declarations in a generated Pantagruel document.
 *
 * Each rule introduces its own bindings. No two rules should share
 * a parameter name, since they represent distinct variables that may
 * have different guards or invariants.
 *
 * Represented as an immutable record: each `registerName` call returns
 * a fresh registry plus the chosen name. Threading the returned registry
 * is the caller's responsibility.
 */
export interface NameRegistry {
  readonly used: ReadonlySet<string>;
}

export function emptyNameRegistry(): NameRegistry {
  return { used: new Set() };
}

/** Check whether a name is already registered. */
export function isUsed(registry: NameRegistry, name: string): boolean {
  return registry.used.has(name);
}

/**
 * Register a name. If already used, appends numeric suffixes (1, 2, ...)
 * until unique. Returns the chosen name and the updated registry.
 */
export function registerName(
  registry: NameRegistry,
  name: string,
): { name: string; registry: NameRegistry } {
  if (!registry.used.has(name)) {
    const used = new Set(registry.used);
    used.add(name);
    return { name, registry: { used } };
  }
  let suffix = 1;
  while (registry.used.has(`${name}${suffix}`)) {
    suffix++;
  }
  const actual = `${name}${suffix}`;
  const used = new Set(registry.used);
  used.add(actual);
  return { name: actual, registry: { used } };
}
