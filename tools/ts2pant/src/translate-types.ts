import ts from "typescript";
import type { ExtractedTypes } from "./extract.js";
import {
  emptyNameRegistry,
  type NameRegistry,
  registerName,
} from "./name-registry.js";
import { getAst } from "./pant-wasm.js";
import type { PantDeclaration } from "./types.js";

/**
 * Sentinel returned by `mapTsType` when an anonymous-record type is
 * encountered but synthesis is unavailable or fails. Distinct from the
 * compiler's `__type` symbol name so that downstream code (notably
 * `translateRecordReturn`) can detect synthesis failure rather than
 * silently treat the unmangleable shape as a supported anonymous record.
 * The value is not a valid Pantagruel identifier — emission of a signature
 * containing it will be visibly broken rather than referring to an
 * undeclared domain.
 */
export const UNSUPPORTED_ANONYMOUS_RECORD = "__unsupported_anon_record__";

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
 *
 * Immutable record: pure `registerMapKV` / `lookupMapKV` / `emitSynthDecls`
 * helpers operate on it, returning fresh records. Deep call sites that need
 * to register on demand (e.g., `translateBody` via `.get(k)` on an
 * expression-computed Map receiver) wrap synth + registry in a `SynthCell`
 * to avoid threading updates through every `BodyResult`.
 */
export interface MapSynth {
  readonly byKV: ReadonlyMap<string, MapSynthEntry>;
  readonly emitted: ReadonlySet<string>;
}

export function emptyMapSynth(): MapSynth {
  return { byKV: new Map(), emitted: new Set() };
}

/**
 * Register a `Map<K, V>` occurrence. Idempotent: re-registering the same
 * `(kType, vType)` returns the cached domain. Returns `{domain: null, ...}`
 * when either fragment is unmangleable (unsupported upstream type); callers
 * fall back to `checker.typeToString` in that case.
 */
export function registerMapKV(
  synth: MapSynth,
  registry: NameRegistry,
  kType: string,
  vType: string,
): { domain: string | null; synth: MapSynth; registry: NameRegistry } {
  const key = `${kType}|${vType}`;
  const cached = synth.byKV.get(key);
  if (cached) {
    return { domain: cached.names.domain, synth, registry };
  }
  const kFrag = manglePantTypeToFragment(kType);
  const vFrag = manglePantTypeToFragment(vType);
  if (!kFrag || !vFrag) {
    return { domain: null, synth, registry };
  }
  const baseDomain = `${kFrag}To${vFrag}Map`;
  const reg1 = registerName(registry, baseDomain);
  const domain = reg1.name;
  const rule = domain[0]!.toLowerCase() + domain.slice(1);
  const entry: MapSynthEntry = {
    names: { domain, rule, keyPred: `${rule}Key` },
    kType,
    vType,
  };
  const newByKV = new Map(synth.byKV);
  newByKV.set(key, entry);
  return {
    domain,
    synth: { byKV: newByKV, emitted: synth.emitted },
    registry: reg1.registry,
  };
}

export function lookupMapKV(
  synth: MapSynth,
  kType: string,
  vType: string,
): MapSynthEntry | undefined {
  return synth.byKV.get(`${kType}|${vType}`);
}

/**
 * Materialize accumulated decls (domain + membership predicate + guarded
 * value rule) in registration order. Rule-internal binder names are
 * registered *here*, after callers have claimed their own param names,
 * so the synth decls get hygienic suffixes (e.g., `m1`, `k1`). Incremental:
 * only entries not in `synth.emitted` are emitted, so the pipeline can drain
 * new body-level registrations after signature/type translation has already
 * run. Callers must thread the returned `synth` back to preserve the
 * emitted-set invariant.
 */
export function emitSynthDecls(
  synth: MapSynth,
  registry: NameRegistry,
): { decls: PantDeclaration[]; synth: MapSynth; registry: NameRegistry } {
  const decls: PantDeclaration[] = [];
  const ast = getAst();
  const newEmitted = new Set(synth.emitted);
  let currentRegistry = registry;
  for (const [key, entry] of synth.byKV) {
    if (newEmitted.has(key)) {
      continue;
    }
    newEmitted.add(key);
    const { domain, rule, keyPred } = entry.names;
    const mReg = registerName(currentRegistry, "m");
    const mName = mReg.name;
    currentRegistry = mReg.registry;
    const kReg = registerName(currentRegistry, "k");
    const kName = kReg.name;
    currentRegistry = kReg.registry;
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
  return {
    decls,
    synth: { byKV: synth.byKV, emitted: newEmitted },
    registry: currentRegistry,
  };
}

/** Fields of a synthesized record, in canonical (alphabetically sorted) order. */
export interface RecordSynthField {
  name: string;
  type: string;
}

export interface RecordSynthEntry {
  domain: string;
  /** Binder name for accessor-rule parameters on this domain
   *  (e.g., `r` in `name r: NameRegRec => String.`). Registry-allocated so
   *  binders across multiple synthesized record domains don't collide. */
  binder: string;
  fields: RecordSynthField[];
}

/**
 * Accumulates anonymous object-literal type occurrences encountered anywhere
 * in the module's type positions (parameters, return types, nested fields)
 * and synthesizes one domain + one accessor rule per field per unique
 * shape. Records are specified *observationally* — Pantagruel has no
 * record-constructor expression syntax, so a function returning a record
 * literal is axiomatized by what each accessor returns when applied.
 *
 * Dedup by canonical shape string: fields sorted alphabetically by name,
 * each paired with its Pantagruel type, joined with `|`. Field-order
 * permutations in source (`{a, b}` vs `{b, a}`) hash to the same key.
 *
 * Immutable record (same discipline as `MapSynth`): pure register / lookup /
 * emit helpers return fresh records. Deep call sites wrap synth + registry
 * in a `SynthCell` to avoid threading updates through every return value.
 */
export interface RecordSynth {
  readonly byShape: ReadonlyMap<string, RecordSynthEntry>;
  readonly emitted: ReadonlySet<string>;
}

export function emptyRecordSynth(): RecordSynth {
  return { byShape: new Map(), emitted: new Set() };
}

/** Canonical shape string: sorted `<name>:<type>|<name>:<type>|...`. */
function recordShapeKey(fields: ReadonlyArray<RecordSynthField>): string {
  return fields.map((f) => `${f.name}:${f.type}`).join("|");
}

/** Capitalize first letter. `"name"` → `"Name"`. */
function capitalize(s: string): string {
  return s.length === 0 ? s : s[0]!.toUpperCase() + s.slice(1);
}

/**
 * Register an anonymous record shape. Idempotent: re-registering the same
 * sorted-field set returns the cached domain. Returns `{domain: null, ...}`
 * when any field name or type fragment is unmangleable.
 *
 * `fields` must already be in canonical (alphabetically sorted) order with
 * Pantagruel type strings; the caller (`cellRegisterRecord`) is responsible
 * for that normalization.
 */
export function registerRecordShape(
  synth: RecordSynth,
  registry: NameRegistry,
  fields: ReadonlyArray<RecordSynthField>,
): { domain: string | null; synth: RecordSynth; registry: NameRegistry } {
  const key = recordShapeKey(fields);
  const cached = synth.byShape.get(key);
  if (cached) {
    return { domain: cached.domain, synth, registry };
  }
  // Validate field names (must be valid identifiers) and base name
  // mangleability.
  for (const f of fields) {
    if (!/^[A-Za-z_][A-Za-z0-9_]*$/u.test(f.name)) {
      return { domain: null, synth, registry };
    }
    // Field types need not mangle to a single identifier (e.g. `[String]`
    // is a valid accessor-rule return type); we just need them to be
    // legal Pantagruel type strings. `mapTsType` already produces those
    // or falls through to `checker.typeToString` which yields unparseable
    // output — that latter case is the only real failure mode. Accept
    // anything for now and let upstream mangleability checks gate.
  }
  const baseDomain =
    fields.length === 0
      ? "EmptyRec"
      : `${fields.map((f) => capitalize(f.name)).join("")}Rec`;
  const reg1 = registerName(registry, baseDomain);
  const domain = reg1.name;
  // Allocate a shared binder name for this domain's accessor rules.
  // Using "r" as the mnemonic (matching Map synth's "m"/"k" convention).
  const bReg = registerName(reg1.registry, "r");
  const binder = bReg.name;
  const entry: RecordSynthEntry = {
    domain,
    binder,
    fields: fields.map((f) => ({ ...f })),
  };
  const newByShape = new Map(synth.byShape);
  newByShape.set(key, entry);
  return {
    domain,
    synth: { byShape: newByShape, emitted: synth.emitted },
    registry: bReg.registry,
  };
}

export function lookupRecordShape(
  synth: RecordSynth,
  fields: ReadonlyArray<RecordSynthField>,
): RecordSynthEntry | undefined {
  return synth.byShape.get(recordShapeKey(fields));
}

/**
 * Materialize accumulated record decls (domain + one accessor rule per
 * field) in registration order. Incremental like `emitSynthDecls`: only
 * entries not in `synth.emitted` are emitted, so the pipeline can drain
 * new body-level registrations after signature/type translation has
 * already run.
 */
export function emitRecordSynthDecls(
  synth: RecordSynth,
  registry: NameRegistry,
): { decls: PantDeclaration[]; synth: RecordSynth; registry: NameRegistry } {
  const decls: PantDeclaration[] = [];
  const newEmitted = new Set(synth.emitted);
  for (const [key, entry] of synth.byShape) {
    if (newEmitted.has(key)) {
      continue;
    }
    newEmitted.add(key);
    decls.push({ kind: "domain", name: entry.domain });
    for (const field of entry.fields) {
      decls.push({
        kind: "rule",
        name: field.name,
        params: [{ name: entry.binder, type: entry.domain }],
        returnType: field.type,
      });
    }
  }
  return {
    decls,
    synth: { byShape: synth.byShape, emitted: newEmitted },
    registry,
  };
}

/**
 * Mutable 3-field cell bundling a `MapSynth`, `RecordSynth` and
 * `NameRegistry`. Used by deep call sites (mapTsType recursion, body
 * translation) that would otherwise have to thread state through every
 * return value. The fields are reassigned in place with freshly-computed
 * immutable records; the inner values remain pure. Cell-field assignment
 * is itself within ts2pant's self-translation envelope (translatable as
 * primed rules on the cell).
 */
export interface SynthCell {
  synth: MapSynth;
  recordSynth: RecordSynth;
  registry: NameRegistry;
}

export function newSynthCell(registry?: NameRegistry): SynthCell {
  return {
    synth: emptyMapSynth(),
    recordSynth: emptyRecordSynth(),
    registry: registry ?? emptyNameRegistry(),
  };
}

/** Cell-mutating wrapper around `registerMapKV` for the legacy call shape. */
export function cellRegisterMap(
  cell: SynthCell,
  kType: string,
  vType: string,
): string | null {
  const r = registerMapKV(cell.synth, cell.registry, kType, vType);
  cell.synth = r.synth;
  cell.registry = r.registry;
  return r.domain;
}

/** Cell-mutating wrapper around `registerRecordShape`. `fields` must be in
 *  canonical (alphabetically sorted) order. */
export function cellRegisterRecord(
  cell: SynthCell,
  fields: ReadonlyArray<RecordSynthField>,
): string | null {
  const r = registerRecordShape(cell.recordSynth, cell.registry, fields);
  cell.recordSynth = r.synth;
  cell.registry = r.registry;
  return r.domain;
}

/** Cell read-through for `lookupRecordShape`. `fields` must be canonical. */
export function cellLookupRecord(
  cell: SynthCell,
  fields: ReadonlyArray<RecordSynthField>,
): RecordSynthEntry | undefined {
  return lookupRecordShape(cell.recordSynth, fields);
}

/** Cell-mutating wrapper around `registerName`. */
export function cellRegisterName(cell: SynthCell, name: string): string {
  const r = registerName(cell.registry, name);
  cell.registry = r.registry;
  return r.name;
}

/** Cell read-through for `isUsed`. */
export function cellIsUsed(cell: SynthCell, name: string): boolean {
  return cell.registry.used.has(name);
}

/** Cell-mutating wrapper that drains both Map and Record synth decls.
 *  Emits Maps first so Record accessor-rule return types can reference
 *  any Map domain registered bottom-up. Incremental: each call returns
 *  only the entries added since the previous drain. */
export function cellEmitSynth(cell: SynthCell): PantDeclaration[] {
  const mapR = emitSynthDecls(cell.synth, cell.registry);
  cell.synth = mapR.synth;
  cell.registry = mapR.registry;
  const recR = emitRecordSynthDecls(cell.recordSynth, cell.registry);
  cell.recordSynth = recR.synth;
  cell.registry = recR.registry;
  return [...mapR.decls, ...recR.decls];
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
 * When `synthCell` is provided, `Map<K, V>` in any type position (parameter,
 * return, nested in another Map's V, inside an array/tuple/union) is
 * registered with the synthesizer and replaced with the synthesized domain
 * name. Without `synthCell`, Map types fall through to the caller's fallback
 * (typically `checker.typeToString()` which yields unparseable output).
 * The cell-less behavior is preserved for Stage A: interface-field Maps
 * are handled specially in `translateTypes` and must not be synthesized.
 */
export function mapTsType(
  type: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell?: SynthCell,
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
      .map((t) => mapTsType(t, checker, strategy, synthCell))
      .join(" * ");
  }

  // Array
  if (checker.isArrayType(type)) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    if (typeArgs.length === 1) {
      return `[${mapTsType(typeArgs[0]!, checker, strategy, synthCell)}]`;
    }
    return checker.typeToString(type);
  }

  // Set — modeled as a list. Pantagruel lists already encode membership
  // (smt_types.ml: Array elem_sort Bool), so `s.has(x)` can become `x in s`
  // with list semantics. Uniqueness is not tracked as a logical invariant.
  if (isSetType(type)) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    if (typeArgs.length === 1) {
      return `[${mapTsType(typeArgs[0]!, checker, strategy, synthCell)}]`;
    }
    return checker.typeToString(type);
  }

  // Map — synthesize a domain when a synthesizer cell is provided. If the
  // K or V type is unmangleable (e.g., contains an unsupported TS type),
  // `cellRegisterMap` returns null and we fall through to checker.typeToString
  // — the same unsupported-type fallback used by the array and set branches.
  if (isMapType(type) && synthCell) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    if (typeArgs.length === 2) {
      const kType = mapTsType(typeArgs[0]!, checker, strategy, synthCell);
      const vType = mapTsType(typeArgs[1]!, checker, strategy, synthCell);
      const domain = cellRegisterMap(synthCell, kType, vType);
      if (domain !== null) {
        return domain;
      }
    }
  }

  // Union
  if (type.isUnion()) {
    // Boolean is represented as true | false union
    if (type.types.every((t) => t.flags & ts.TypeFlags.BooleanLiteral)) {
      return "Bool";
    }
    const parts = type.types.map((t) =>
      mapTsType(t, checker, strategy, synthCell),
    );
    // Deduplicate (e.g. boolean literal collapse)
    const unique = parts.filter((v, i, a) => a.indexOf(v) === i);
    // Sort Nothing to the end for consistent output
    unique.sort((a, b) => (a === "Nothing" ? 1 : b === "Nothing" ? -1 : 0));
    return unique.join(" + ");
  }

  // Anonymous record type — synthesize a domain + accessor rules per
  // unique shape. Mirrors the `Map<K, V>` synth pattern: registration is
  // idempotent on canonical (sorted-field) shape, one domain per shape,
  // nested anonymous records compose bottom-up via recursive mapTsType.
  // Never fall through to the generic symbol branch on failure — that
  // branch returns `__type`, which is the same sentinel
  // `translateRecordReturn` uses to detect anonymous returns and would
  // silently mark the function as a supported record return whose
  // synthesized domain has not actually been registered.
  if (isAnonymousRecord(type)) {
    if (synthCell) {
      const domain = registerAnonymousRecord(
        type,
        checker,
        strategy,
        synthCell,
      );
      if (domain !== null) {
        return domain;
      }
    }
    return UNSUPPORTED_ANONYMOUS_RECORD;
  }

  // Named type (interface, class, enum, type alias)
  const symbol = type.aliasSymbol ?? type.symbol;
  if (symbol) {
    return symbol.getName();
  }

  return checker.typeToString(type);
}

/**
 * Collect declared fields from an anonymous object type, map each field's
 * type via `mapTsType`, sort fields alphabetically by name, and register
 * with the synth cell. Returns the synthesized domain name, or null if
 * any field's type is unmangleable or a field name is a non-identifier.
 *
 * Callers must have already checked `isAnonymousRecord(type)`.
 */
function registerAnonymousRecord(
  type: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell: SynthCell,
): string | null {
  const properties = type.getProperties();
  const fields: RecordSynthField[] = [];
  for (const prop of properties) {
    // Resolve the property's declared type. For anonymous object types,
    // `PropertySignature` is the usual declaration kind; fall back to the
    // checker when the property has no source declaration (structural
    // shapes from mapped / indexed-access types). The fallback cast
    // reaches an internal TypeScript compiler property and should be
    // revisited if the compiler internals change.
    const decl = prop.getDeclarations()?.[0];
    const propType = decl
      ? checker.getTypeOfSymbolAtLocation(prop, decl)
      : (prop as unknown as { type?: ts.Type }).type;
    if (!propType) {
      return null;
    }
    const mapped = mapTsType(propType, checker, strategy, synthCell);
    // Propagate failure from a nested anonymous-record synthesis.
    // Without this, the parent shape would register with the sentinel
    // string as a field type and emit invalid output.
    if (mapped === UNSUPPORTED_ANONYMOUS_RECORD) {
      return null;
    }
    fields.push({ name: prop.getName(), type: mapped });
  }
  // Canonical order: sort alphabetically by field name.
  fields.sort((a, b) => a.name.localeCompare(b.name));
  return cellRegisterRecord(synthCell, fields);
}

/**
 * Detect a TypeScript `Set<T>` or `ReadonlySet<T>` by symbol name. Both
 * translate to `[T]` since Pantagruel lists encode membership. Brittle
 * against user-defined classes by the same name, but matches how
 * `isArrayType` effectively works and is the pragmatic choice.
 */
export function isSetType(type: ts.Type): boolean {
  const symbol = type.getSymbol();
  const name = symbol?.getName();
  return name === "Set" || name === "ReadonlySet";
}

/**
 * Detect an anonymous object/record type — a TS inline shape with no
 * declared interface / alias. These surface with the compiler-assigned
 * symbol name `__type`. Callable types (`{ (): void }`) would also
 * match on the name alone; we guard against those by rejecting shapes
 * with call signatures. Zero-property records (`{}`) are intentionally
 * supported and synthesize as `EmptyRec` via `registerRecordShape`.
 */
export function isAnonymousRecord(type: ts.Type): boolean {
  const symbol = type.getSymbol();
  if (symbol?.getName() !== "__type") {
    return false;
  }
  if (type.getCallSignatures().length > 0) {
    return false;
  }
  return true;
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
  synthCell?: SynthCell,
): PantDeclaration[] {
  const decls: PantDeclaration[] = [];

  for (const iface of extracted.interfaces) {
    decls.push({ kind: "domain", name: iface.name });
    const candidate = paramName(iface.name);
    const pName = synthCell
      ? cellRegisterName(synthCell, candidate)
      : candidate;
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
          const kType = mapTsType(typeArgs[0]!, checker, strategy, synthCell);
          const vType = mapTsType(typeArgs[1]!, checker, strategy, synthCell);
          const kName = synthCell ? cellRegisterName(synthCell, "k") : "k";
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
        returnType: mapTsType(prop.type, checker, strategy, synthCell),
      });
    }
  }

  for (const alias of extracted.aliases) {
    decls.push({
      kind: "alias",
      name: alias.name,
      type: mapTsType(alias.type, checker, strategy, synthCell),
    });
  }

  for (const enumDecl of extracted.enums) {
    decls.push({ kind: "domain", name: enumDecl.name });
  }

  return decls;
}
