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
 * TypeScript type flags for `null` / `undefined` / `void`. Pantagruel
 * retired `Nothing` from the user-facing type surface — there is no
 * writable type for "absence". `mapTsType` handles these only inside a
 * union (`T | null` → `[T]` via list-lift / Alloy's `lone` multiplicity).
 * A top-level `null` / `undefined` / `void` has no Pantagruel encoding
 * and falls through to `checker.typeToString`, matching the generic
 * unsupported-shape fallback and keeping any internal marker from
 * leaking into emitted declarations.
 */
const NULLISH_FLAGS =
  ts.TypeFlags.Null | ts.TypeFlags.Undefined | ts.TypeFlags.Void;

/** True for TypeScript `null` / `undefined` / `void`. */
function isTsNullish(type: ts.Type): boolean {
  return (type.flags & NULLISH_FLAGS) !== 0;
}

/**
 * Mangle a Pantagruel type string into an identifier-safe fragment suitable
 * for embedding inside a synthesized Map domain name.
 *   "String"           → "String"
 *   "[String]"         → "ListString"
 *   "String + Int"     → "StringOrInt"
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
  const rule = toPantTermName(domain);
  const entry: MapSynthEntry = {
    names: { domain, rule, keyPred: `${rule}-key` },
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

/** Canonical Pantagruel spelling for lowercase term identifiers.
 *  Pantagruel identifiers come from `[a-zA-Z0-9-_?!]` (see
 *  `lib/smt_types.ml`), so any character outside that alphabet — `$`,
 *  non-ASCII letters, punctuation — would be rejected at parse time.
 *  `?` and `!` are preserved as they're permitted by the Pantagruel
 *  lexer for predicate-style naming.
 *
 *  Pipeline order (contract):
 *   1. Replace any character outside the Pantagruel alphabet with `-`.
 *      Runs first so everything downstream operates on ASCII-only text
 *      drawn from the identifier alphabet.
 *   2. Insert camelCase word boundaries (`URLFoo` → `URL-Foo`,
 *      `fooBar` → `foo-Bar`). The ASCII-only regexes can't introduce
 *      dashes inside the sanitized-away regions.
 *   3. Normalize underscores/whitespace to `-`, then collapse runs of
 *      `-` and trim the edges. (The `--` boundary in `fieldRuleName`
 *      relies on step 3's collapse to ensure single-hyphen runs inside
 *      each half.)
 *
 *  Inputs that normalize to the empty string (e.g., `"_"`, `"$"`,
 *  all-punctuation identifiers) would otherwise leak through as `""`
 *  and produce unparseable downstream names like `fieldRuleName("_",
 *  "id")` → `"-id"`. Fall back to a deterministic non-empty token so
 *  every output is a valid Pantagruel identifier. */
export function toPantTermName(name: string): string {
  const kebab = name
    .replace(/[^A-Za-z0-9?!_-]+/gu, "-")
    .replace(/([A-Z]+)([A-Z][a-z])/gu, "$1-$2")
    .replace(/([a-z0-9])([A-Z])/gu, "$1-$2")
    .replace(/[_\s]+/gu, "-")
    .replace(/-+/gu, "-")
    .replace(/^-|-$/gu, "")
    .toLowerCase();
  if (kebab.length > 0) {
    return kebab;
  }
  // All-punctuation / all-underscore input. Emit a stable fallback:
  // `t_` + hex of each codepoint, joined by `_`. Underscores cannot
  // appear in kebab output (the `[_\s]+` step rewrites them to `-`),
  // so the `t_`-prefixed namespace is disjoint from every normal
  // normalization — `toPantTermName("$")` -> `t_24` no longer collides
  // with `toPantTermName("t24")` -> `t24`. Hex-joined codepoints are
  // injective across distinct inputs.
  const hex = Array.from(name)
    .map((c) => c.codePointAt(0)!.toString(16))
    .join("_");
  return `t_${hex || "0"}`;
}

/**
 * Conservative gate against `mapTsType` fallback strings reaching synth
 * registration. Pantagruel type expressions are built from identifiers,
 * the list bracket `[T]`, sum `T + U`, product `T * U`, parens, the
 * module qualifier `::`, and whitespace. Compiler-text fallbacks
 * (`Map<string, number>`, `{ x: number }`, `() => void`) contain
 * characters outside that set and must be rejected at synth time.
 */
function isValidPantFieldType(s: string): boolean {
  return s.length > 0 && /^[A-Za-z0-9_:\s[\]+*()]+$/u.test(s);
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
  // Validate field names (must be valid identifiers) and field types
  // (must be parseable Pantagruel type expressions). `mapTsType` falls
  // back to `checker.typeToString` for unsupported types, yielding
  // strings like `Map<string, number>` or `{ x: number }` that contain
  // TS-compiler artifacts (`<`, `>`, `{`, `}`, `,`, etc.) — never legal
  // Pantagruel. Reject any field whose type isn't drawn from the
  // Pantagruel type-expression character set so the broken text can't
  // reach `emitRecordSynthDecls`.
  for (const f of fields) {
    if (!/^[A-Za-z_][A-Za-z0-9_]*$/u.test(f.name)) {
      return { domain: null, synth, registry };
    }
    if (!isValidPantFieldType(f.type)) {
      return { domain: null, synth, registry };
    }
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
 *
 * Accessor rules are qualified with the synthesized domain via
 * `fieldRuleName`, matching how interface-field accessors are qualified
 * in `translateTypes`. Two distinct record shapes that share a field
 * name (e.g., both have `name`) therefore produce distinct arity-1 rules
 * and don't collide under Pantagruel's positional coherence check. Use
 * sites (`translateRecordReturn`, `emitRecordEquations`, `qualifyFieldAccess`)
 * resolve to the same qualified symbol.
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
        name: fieldRuleName(entry.domain, field.name),
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

/** Pantagruel rule symbol for an interface field. Qualified so two
 *  interfaces that happen to share a field name produce distinct arity-1
 *  rules — Pantagruel's positional coherence rejects same-(name, arity)
 *  declarations with disagreeing position-0 types, so the disambiguation
 *  has to happen at emission time rather than relying on the checker.
 *  The same resolver runs at declaration sites (translateTypes) and at
 *  use sites (property-access in translateBody), so the two stay in
 *  lockstep.
 *
 *  Encoding must be *injective* on `(owner, field)` pairs: a bare
 *  `<owner>-<field>` hyphen-join collides after kebab normalization —
 *  e.g., `("FooBar", "baz")` and `("Foo", "barBaz")` both reduce to
 *  `foo-bar-baz`. Use `--` (double hyphen) as the boundary: `toPantTermName`
 *  collapses `-+` to single `-` and trims, so its output never contains
 *  `--`. That makes the double-hyphen separator reversible — splitting
 *  the result on the first `--` recovers the exact `(owner, field)`
 *  pair. Pantagruel's lexer accepts `-` in identifier continuations,
 *  so `foo--bar` is a single legal rule name. */
export function fieldRuleName(
  interfaceName: string,
  fieldName: string,
): string {
  return `${toPantTermName(interfaceName)}--${toPantTermName(fieldName)}`;
}

/** Resolution result for field-owner lookups. Declaration- and use-site
 *  emission must pick the same owner so the qualified rule symbol stays
 *  coherent; ambiguity (e.g., `A | B` where both declare `x`) means no
 *  single owner exists, and the access must be rejected rather than
 *  silently bound to one branch. */
export type FieldOwnerResolution =
  | { kind: "resolved"; owner: string }
  | { kind: "none" }
  | { kind: "ambiguous"; owners: string[] };

/** Find the interface/class that *declares* a field on a receiver type.
 *  Uses `checker.getPropertyOfType` + each property declaration's parent
 *  node to resolve the owner — that walks the inheritance chain so
 *  `Sub extends Base` with `x.baseField` resolves to `Base` (the owner
 *  that translate-types emits the accessor rule under), not `Sub`.
 *  Anonymous record receivers resolve to their synthesized domain via
 *  the synth cell. For unions/intersections, collects distinct owners
 *  across members; >1 distinct owner returns `ambiguous` so callers can
 *  refuse the access rather than emit a semantically wrong qualified
 *  symbol. */
export function resolveFieldOwner(
  receiverType: ts.Type,
  fieldName: string,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell: SynthCell | undefined,
): FieldOwnerResolution {
  const owners = new Set<string>();
  collectFieldOwners(
    receiverType,
    fieldName,
    checker,
    strategy,
    synthCell,
    owners,
  );
  if (owners.size === 0) {
    return { kind: "none" };
  }
  if (owners.size === 1) {
    return { kind: "resolved", owner: [...owners][0]! };
  }
  return { kind: "ambiguous", owners: [...owners] };
}

function collectFieldOwners(
  ty: ts.Type,
  fieldName: string,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell: SynthCell | undefined,
  out: Set<string>,
): void {
  // Anonymous record (including alias-backed `type Point = {...}`): route
  // through the synth cell so the use-site qualified symbol matches the
  // accessor rule declared in `emitRecordSynthDecls`.
  if (
    synthCell &&
    isAnonymousRecord(ty) &&
    ty.getProperty(fieldName) !== undefined
  ) {
    const owner = resolveRecordOwner(ty, checker, strategy, synthCell);
    if (owner) {
      out.add(owner);
      return;
    }
  }

  if (ty.isUnionOrIntersection()) {
    for (const sub of ty.types) {
      collectFieldOwners(sub, fieldName, checker, strategy, synthCell, out);
    }
    return;
  }

  // Named type: resolve the field's declaring owner from its property
  // symbol's declaration chain. `getPropertyOfType` respects inheritance
  // (interface/class extends), so `Sub.inheritedField` returns the symbol
  // declared on the base.
  const prop = checker.getPropertyOfType(ty, fieldName);
  if (!prop) {
    return;
  }
  for (const decl of prop.declarations ?? []) {
    const owner = ownerFromDeclaration(decl);
    if (owner) {
      out.add(owner);
    }
  }
}

function ownerFromDeclaration(decl: ts.Declaration): string | null {
  // Skip declarations from .d.ts files: built-in types (String, Array,
  // ReadonlyMap, etc.) declare `length`, `size`, `get`, and friends, but
  // ts2pant never emits accessor rules for them — they're either handled
  // specially (`#` for array length, Stage A/B encoding for Map) or fall
  // through as uninterpreted references. Qualifying them as
  // `string--length` would produce a dangling symbol; let the non-lib
  // path decide whether to qualify or fall back to the bare kebab name.
  if (decl.getSourceFile().isDeclarationFile) {
    return null;
  }
  const parent = decl.parent;
  if (!parent) {
    return null;
  }
  if (ts.isInterfaceDeclaration(parent) && parent.name) {
    return parent.name.text;
  }
  if (ts.isClassDeclaration(parent) && parent.name) {
    return parent.name.text;
  }
  // Anonymous type literal, library declaration, etc. — no named owner.
  return null;
}

/**
 * Resolve the owner-domain name for a record-typed receiver. Returns the
 * interface/class/alias name for named types, the synthesized domain name
 * for anonymous records (looked up through the synth cell), or null when
 * the type is neither. The resulting owner is the first argument to
 * `fieldRuleName`, so declaration- and use-site emission stay in lockstep
 * for both named and synthesized owners.
 */
export function resolveRecordOwner(
  type: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell: SynthCell | undefined,
): string | null {
  // Anonymous record shapes — including alias-backed ones like
  // `type Point = {x, y}` — resolve to the synthesized domain, not the
  // alias name. `isAnonymousRecord` inspects the underlying structural
  // symbol (`type.getSymbol()?.getName() === "__type"`), so it matches
  // both bare anonymous `__type` and alias-backed anonymous shapes. The
  // alias name does not correspond to any declared Pantagruel domain, so
  // preferring it here would break lockstep with the synth's emission.
  if (synthCell && isAnonymousRecord(type)) {
    const mapped = mapTsType(type, checker, strategy, synthCell);
    if (mapped !== UNSUPPORTED_ANONYMOUS_RECORD) {
      return mapped;
    }
  }
  const sym = type.symbol ?? type.aliasSymbol;
  const symName = sym?.getName();
  if (sym && symName && symName !== "__type") {
    return symName;
  }
  return null;
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
  const r = registerName(cell.registry, toPantTermName(name));
  cell.registry = r.registry;
  return r.name;
}

/** Cell read-through for `isUsed`. */
export function cellIsUsed(cell: SynthCell, name: string): boolean {
  return cell.registry.used.has(toPantTermName(name));
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
  // Top-level `null` / `undefined` / `void` have no Pantagruel encoding.
  // Fall through to the generic `checker.typeToString` fallback below so
  // the unsupported string reflects the source rather than an internal
  // sentinel. Null handling for unions happens in the union branch.

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
    // List-lift encoding for optionality: strip null/undefined/void before
    // recursing so the internal "nothing" marker never escapes to callers.
    // `T | null` → `[T]`, `A | B | null` → `[A + B]`. Pantagruel has no
    // `Nothing` at the user surface — a union with null/undefined/void
    // wraps the rest in a list of length 0 or 1 (Alloy `lone`).
    const hasNullish = type.types.some(isTsNullish);
    const nonNullTypes = type.types.filter((t) => !isTsNullish(t));
    if (nonNullTypes.length === 0) {
      // Degenerate `null | undefined` — no non-null members. Fall through
      // to the generic checker fallback below so the broken output mirrors
      // the source rather than an internal sentinel.
      return checker.typeToString(type);
    }
    const parts = nonNullTypes.map((t) =>
      mapTsType(t, checker, strategy, synthCell),
    );
    const unique = parts.filter((v, i, a) => a.indexOf(v) === i);
    if (hasNullish) {
      return `[${unique.join(" + ")}]`;
    }
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
 * symbol name `__type`. Several other shapes match on the name alone
 * and must be rejected because record synthesis is only for finite
 * field-based shapes:
 *   - Callable / constructor types (`{ (): T }`, `{ new(): T }`) —
 *     detected via call/construct signatures.
 *   - Index-signature dictionaries (`{ [k: string]: T }`,
 *     `{ [k: number]: T }`) — `getProperties()` returns empty for
 *     these, so without an explicit guard they would synthesize as
 *     `EmptyRec`, misclassifying an unbounded dictionary as a finite
 *     empty record.
 * Zero-property records (`{}`) are intentionally supported and
 * synthesize as `EmptyRec` via `registerRecordShape`.
 */
export function isAnonymousRecord(type: ts.Type): boolean {
  const symbol = type.getSymbol();
  if (symbol?.getName() !== "__type") {
    return false;
  }
  if (
    type.getCallSignatures().length > 0 ||
    type.getConstructSignatures().length > 0
  ) {
    return false;
  }
  if (
    type.getStringIndexType() !== undefined ||
    type.getNumberIndexType() !== undefined
  ) {
    return false;
  }
  return true;
}

/**
 * Detect a TypeScript `Map<K, V>` or `ReadonlyMap<K, V>` by symbol name.
 * Same caveat as `isSetType`.
 */
export function isMapType(type: ts.Type): boolean {
  const symbol = type.getSymbol();
  const name = symbol?.getName();
  return name === "Map" || name === "ReadonlyMap";
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
      // Every field accessor rule is qualified with its owning interface so
      // that two interfaces with a field of the same name produce distinct
      // arity-1 rules under Pantagruel's positional coherence. The body
      // translator applies the same [fieldRuleName] at use sites to stay
      // in lockstep with these declarations.
      const ruleName = fieldRuleName(iface.name, prop.name);
      if (isMapType(prop.type)) {
        const typeArgs = checker.getTypeArguments(
          prop.type as ts.TypeReference,
        );
        if (typeArgs.length === 2) {
          // Stage A field-Map encoding: rule name is the qualified field
          // name; the domain is the user's interface. K and V are still
          // translated via mapTsType with the synth passed through, so a
          // nested Map inside V (e.g., `inner: Map<K, Map<K', V'>>`)
          // registers its own synthesized domain and the Stage A rule's V
          // references it.
          const kType = mapTsType(typeArgs[0]!, checker, strategy, synthCell);
          const vType = mapTsType(typeArgs[1]!, checker, strategy, synthCell);
          const kName = synthCell ? cellRegisterName(synthCell, "k") : "k";
          const keyPredName = `${ruleName}-key`;
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
            name: ruleName,
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
        name: ruleName,
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
