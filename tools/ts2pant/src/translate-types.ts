import ts from "typescript";
import type { ExtractedTypes } from "./extract.js";
import type { NameRegistry } from "./name-registry.js";
import { getAst } from "./pant-wasm.js";
import type { PantDeclaration } from "./types.js";

/**
 * Mangle a Pantagruel type string into an identifier-safe fragment suitable
 * for embedding inside a synthesized Map domain name.
 *   "String"           → "String"
 *   "[String]"         → "ListString"
 *   "String + Nothing" → "StringOrNothing"
 *   "A * B"            → "AAndB"
 * Returns null if the mangled result still contains non-identifier chars
 * (e.g., a bare `Map<...>` literal from a Map in value position with no
 * synthesizer upstream). Distinct Pantagruel types produce distinct
 * fragments.
 */
export function manglePantTypeToFragment(pantType: string): string | null {
  const mangled = pantType
    .replace(/\s+/gu, "")
    .replace(/\+/gu, "Or")
    .replace(/\*/gu, "And")
    .replace(/\[/gu, "List")
    .replace(/\]/gu, "");
  return /^[A-Za-z_][A-Za-z0-9_]*$/u.test(mangled) ? mangled : null;
}

export interface MapSynthNames {
  domain: string;
  rule: string;
  keyPred: string;
}

export interface MapSynthEntry {
  names: MapSynthNames;
  kType: string;
  vType: string;
}

/**
 * Accumulates `Map<K, V>` occurrences encountered anywhere in the module's
 * type positions (parameters, return types, nested inside another Map's V,
 * inside arrays / tuples / unions) and synthesizes one domain + guarded-rule
 * pair per unique `(K, V)`. McCarthy's theory of arrays applied via
 * Pantagruel rules: synthesized domain is the array sort, distinct elements
 * are distinct maps, `.get` is a partial function guarded by `.has`.
 */
export class MapSynthesizer {
  private byKV = new Map<string, MapSynthEntry>();

  constructor(private registry: NameRegistry) {}

  register(kType: string, vType: string): string {
    const key = `${kType}|${vType}`;
    const cached = this.byKV.get(key);
    if (cached) {
      return cached.names.domain;
    }
    const kFrag = manglePantTypeToFragment(kType);
    const vFrag = manglePantTypeToFragment(vType);
    if (!kFrag || !vFrag) {
      throw new Error(
        `Cannot synthesize Map domain for K=${kType}, V=${vType}`,
      );
    }
    const baseDomain = `${kFrag}To${vFrag}Map`;
    const domain = this.registry.register(baseDomain);
    const rule = domain[0]!.toLowerCase() + domain.slice(1);
    const entry: MapSynthEntry = {
      names: { domain, rule, keyPred: `${rule}Key` },
      kType,
      vType,
    };
    this.byKV.set(key, entry);
    return domain;
  }

  lookup(kType: string, vType: string): MapSynthEntry | undefined {
    return this.byKV.get(`${kType}|${vType}`);
  }

  /**
   * Materialize accumulated decls (domain + membership predicate + guarded
   * value rule) in registration order. Rule-internal binder names are
   * registered *here*, after callers have claimed their own param names,
   * so the synth decls get hygienic suffixes (e.g., `m1`, `k1`).
   */
  emit(): PantDeclaration[] {
    const decls: PantDeclaration[] = [];
    const ast = getAst();
    for (const entry of this.byKV.values()) {
      const { domain, rule, keyPred } = entry.names;
      const mName = this.registry.register("m");
      const kName = this.registry.register("k");
      decls.push({ kind: "domain", name: domain });
      decls.push({
        kind: "rule",
        name: keyPred,
        params: [
          { name: mName, type: domain },
          { name: kName, type: entry.kType },
        ],
        returnType: "Bool",
      });
      decls.push({
        kind: "rule",
        name: rule,
        params: [
          { name: mName, type: domain },
          { name: kName, type: entry.kType },
        ],
        returnType: entry.vType,
        guard: ast.app(ast.var(keyPred), [ast.var(mName), ast.var(kName)]),
      });
    }
    return decls;
  }
}

/** Strategy for mapping TS `number` to a Pantagruel numeric type. */
export interface NumericStrategy {
  mapNumber: (context?: string) => string;
}

export const IntStrategy: NumericStrategy = {
  mapNumber() {
    return "Int";
  },
};

export const RealStrategy: NumericStrategy = {
  mapNumber() {
    return "Real";
  },
};

/**
 * Map a TypeScript type to a Pantagruel type string.
 *
 * When `synth` is provided, `Map<K, V>` in any type position (parameter,
 * return, nested in another Map's V, inside an array/tuple/union) is
 * registered with the synthesizer and replaced with the synthesized domain
 * name. Without `synth`, Map types fall through to the caller's fallback
 * (typically `checker.typeToString()` which yields unparseable output).
 * The `synth`-less behavior is preserved for Stage A: interface-field Maps
 * are handled specially in `translateTypes` and must not be synthesized.
 */
export function mapTsType(
  type: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synth?: MapSynthesizer,
): string {
  const flags = type.flags;

  if (flags & ts.TypeFlags.String || flags & ts.TypeFlags.StringLiteral) {
    return "String";
  }
  if (flags & ts.TypeFlags.Number || flags & ts.TypeFlags.NumberLiteral) {
    return strategy.mapNumber();
  }
  if (flags & ts.TypeFlags.Boolean || flags & ts.TypeFlags.BooleanLiteral) {
    return "Bool";
  }
  if (
    flags & ts.TypeFlags.Null ||
    flags & ts.TypeFlags.Undefined ||
    flags & ts.TypeFlags.Void
  ) {
    return "Nothing";
  }

  // Tuple (check before array since tuples are also type references)
  if (checker.isTupleType(type)) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    return typeArgs
      .map((t) => mapTsType(t, checker, strategy, synth))
      .join(" * ");
  }

  // Array
  if (checker.isArrayType(type)) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    if (typeArgs.length === 1) {
      return `[${mapTsType(typeArgs[0]!, checker, strategy, synth)}]`;
    }
    return checker.typeToString(type);
  }

  // Set — modeled as a list. Pantagruel lists already encode membership
  // (smt_types.ml: Array elem_sort Bool), so `s.has(x)` can become `x in s`
  // with list semantics. Uniqueness is not tracked as a logical invariant.
  if (isSetType(type)) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    if (typeArgs.length === 1) {
      return `[${mapTsType(typeArgs[0]!, checker, strategy, synth)}]`;
    }
    return checker.typeToString(type);
  }

  // Map — synthesize a domain when a synthesizer is provided.
  if (isMapType(type) && synth) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    if (typeArgs.length === 2) {
      const kType = mapTsType(typeArgs[0]!, checker, strategy, synth);
      const vType = mapTsType(typeArgs[1]!, checker, strategy, synth);
      return synth.register(kType, vType);
    }
  }

  // Union
  if (type.isUnion()) {
    // Boolean is represented as true | false union
    if (type.types.every((t) => t.flags & ts.TypeFlags.BooleanLiteral)) {
      return "Bool";
    }
    const parts = type.types.map((t) => mapTsType(t, checker, strategy, synth));
    // Deduplicate (e.g. boolean literal collapse)
    const unique = parts.filter((v, i, a) => a.indexOf(v) === i);
    // Sort Nothing to the end for consistent output
    unique.sort((a, b) => (a === "Nothing" ? 1 : b === "Nothing" ? -1 : 0));
    return unique.join(" + ");
  }

  // Named type (interface, class, enum, type alias)
  const symbol = type.aliasSymbol ?? type.symbol;
  if (symbol) {
    return symbol.getName();
  }

  return checker.typeToString(type);
}

/**
 * Detect a TypeScript `Set<T>` by symbol name. Brittle against user-defined
 * classes named `Set`, but matches how `isArrayType` effectively works and is
 * the pragmatic choice — a user class named `Set` is vanishingly rare.
 */
export function isSetType(type: ts.Type): boolean {
  const symbol = type.getSymbol();
  return symbol?.getName() === "Set";
}

/**
 * Detect a TypeScript `Map<K, V>` by symbol name. Same caveat as `isSetType`.
 */
export function isMapType(type: ts.Type): boolean {
  const symbol = type.getSymbol();
  return symbol?.getName() === "Map";
}

/** Derive a short parameter name from a type name (first letter, lowercased). */
function paramName(typeName: string): string {
  if (!typeName) {
    return "x";
  }
  return typeName[0]!.toLowerCase();
}

/**
 * Translate extracted TypeScript types into Pantagruel declarations.
 *
 * - Interfaces -> domain + one rule per property
 * - Type aliases -> alias declaration
 * - Enums -> domain declaration
 */
export function translateTypes(
  extracted: ExtractedTypes,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  registry?: NameRegistry,
  synth?: MapSynthesizer,
): PantDeclaration[] {
  const decls: PantDeclaration[] = [];

  for (const iface of extracted.interfaces) {
    decls.push({ kind: "domain", name: iface.name });
    const candidate = paramName(iface.name);
    const pName = registry ? registry.register(candidate) : candidate;
    for (const prop of iface.properties) {
      if (isMapType(prop.type)) {
        const typeArgs = checker.getTypeArguments(
          prop.type as ts.TypeReference,
        );
        if (typeArgs.length === 2) {
          // Stage A field-Map encoding: rule name is the field name; the
          // domain is the user's interface. K and V are still translated
          // via mapTsType with the synth passed through, so a nested Map
          // inside V (e.g., `inner: Map<K, Map<K', V'>>`) registers its
          // own synthesized domain and the Stage A rule's V references it.
          const kType = mapTsType(typeArgs[0]!, checker, strategy, synth);
          const vType = mapTsType(typeArgs[1]!, checker, strategy, synth);
          const kName = registry ? registry.register("k") : "k";
          const keyPredName = `${prop.name}Key`;
          decls.push({
            kind: "rule",
            name: keyPredName,
            params: [
              { name: pName, type: iface.name },
              { name: kName, type: kType },
            ],
            returnType: "Bool",
          });
          const ast = getAst();
          decls.push({
            kind: "rule",
            name: prop.name,
            params: [
              { name: pName, type: iface.name },
              { name: kName, type: kType },
            ],
            returnType: vType,
            guard: ast.app(ast.var(keyPredName), [
              ast.var(pName),
              ast.var(kName),
            ]),
          });
          continue;
        }
      }
      decls.push({
        kind: "rule",
        name: prop.name,
        params: [{ name: pName, type: iface.name }],
        returnType: mapTsType(prop.type, checker, strategy, synth),
      });
    }
  }

  for (const alias of extracted.aliases) {
    decls.push({
      kind: "alias",
      name: alias.name,
      type: mapTsType(alias.type, checker, strategy, synth),
    });
  }

  for (const enumDecl of extracted.enums) {
    decls.push({ kind: "domain", name: enumDecl.name });
  }

  return decls;
}
