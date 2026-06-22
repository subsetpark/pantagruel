// @archlint.module core
// @archlint.domain ts2pant.translate-types

import ts from "typescript";
import { effectBrandTypeArgument } from "./brand-precondition.js";
import type { DepModuleName } from "./builtins.js";
import type { ExtractedTypes } from "./extract.js";
import {
  emptyNameRegistry,
  type NameRegistry,
  registerName,
} from "./name-registry.js";
import { OPAQUE_DOMAIN, opaqueValueRuleName } from "./opaque.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import type { PantDeclaration, PropResult } from "./types.js";

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

export const UNSUPPORTED_NON_DISCRIMINATED_UNION_FIELD_ACCESS_REASON =
  "field access on a non-discriminated union is not expressible in Pantagruel";
export const UNSUPPORTED_DISCRIMINATED_UNION_REGISTRATION_REASON =
  "discriminated union could not be registered for tagged Pantagruel encoding";

/**
 * Result of mapping a TypeScript type to a Pantagruel sort. Failure is
 * carried out-of-band (the `ok: false` variant) so a refusal reason can never
 * be emitted as if it were a real sort — the structural replacement for the
 * old in-band sentinel strings (`__unsupported_unknown__` etc.). Mirrors the
 * tagged-union result discipline already used by `BodyResult`
 * (`translate-body.ts`) and `PantDeclaration` (`types.ts`).
 */
export type MapTsTypeResult =
  | { readonly ok: true; readonly sort: string }
  | { readonly ok: false; readonly reason: string };

export function okSort(sort: string): MapTsTypeResult {
  return { ok: true, sort };
}

export function unsupportedType(reason: string): MapTsTypeResult {
  return { ok: false, reason };
}

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
export function isTsNullish(type: ts.Type): boolean {
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
 *
 * @pant manglePantTypeToFragment pantType = manglePantTypeToFragmentImpl pantType.
 * @pant manglePantTypeToFragment pantType = manglePantTypeToFragment pantType.
 */
export function manglePantTypeToFragment(pantType: string): string | null {
  return manglePantTypeToFragmentImpl(pantType);
}

function manglePantTypeToFragmentImpl(pantType: string): string | null {
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
 * @pant mapSynthKey kType vType = kType + "|" + vType.
 */
function mapSynthKey(kType: string, vType: string): string {
  // biome-ignore lint/style/useConst: deliberate immutable-let dogfood fixture
  let separator = "|";
  return `${kType}${separator}${vType}`;
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

/**
 * @pant all s: String | ~(s in emitted emptyMapSynth).
 */
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
  const key = mapSynthKey(kType, vType);
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

/**
 * @pant lookupMapKV synth kType vType = byKV synth (mapSynthKey kType vType).
 */
export function lookupMapKV(
  synth: MapSynth,
  kType: string,
  vType: string,
): MapSynthEntry | undefined {
  return synth.byKV.get(mapSynthKey(kType, vType));
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

/**
 * @pant recordFieldShapeKey field = (name field) + ":" + (type field).
 */
function recordFieldShapeKey(field: RecordSynthField): string {
  // biome-ignore lint/style/useConst: deliberate immutable-let dogfood fixture
  let separator = ":";
  return `${field.name}${separator}${field.type}`;
}

/**
 * @pant all s: String | ~(s in emitted emptyRecordSynth).
 */
export function emptyRecordSynth(): RecordSynth {
  return { byShape: new Map(), emitted: new Set() };
}

/** Canonical shape string: sorted `<name>:<type>|<name>:<type>|...`. */
function recordShapeKey(fields: ReadonlyArray<RecordSynthField>): string {
  return fields.map(recordFieldShapeKey).join("|");
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
 *  Inputs that normalize to a digit-leading or empty string (e.g.,
 *  `"_0"`, `"_"`, `"$"`) would otherwise produce unparseable downstream
 *  names like `fieldRuleName("_", "id")` → `"-id"`. Fall back to a
 *  deterministic lowercase-prefixed token so every output is a valid
 *  Pantagruel identifier. */
export function toPantTermName(name: string): string {
  const kebab = name
    .replace(/[^A-Za-z0-9?!_-]+/gu, "-")
    .replace(/([A-Z]+)([A-Z][a-z])/gu, "$1-$2")
    .replace(/([a-z0-9])([A-Z])/gu, "$1-$2")
    .replace(/[_\s]+/gu, "-")
    .replace(/-+/gu, "-")
    .replace(/^-|-$/gu, "")
    .toLowerCase();
  if (/^[a-z]/u.test(kebab)) {
    return kebab;
  }
  if (kebab.length > 0) {
    return `t-${kebab}`;
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

/**
 * @pant lookupRecordShape synth fields = byShape synth (recordShapeKey fields).
 */
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

export type DiscriminantLiteral =
  | { kind: "string"; value: string }
  | { kind: "number"; value: number }
  | { kind: "boolean"; value: boolean };

export interface DiscriminatedUnionField {
  name: string;
  type: ts.Type;
}

export interface DiscriminatedUnionVariant {
  literal: DiscriminantLiteral;
  fields: DiscriminatedUnionField[];
}

export interface DiscriminatedUnionDetection {
  discriminant: string;
  variants: DiscriminatedUnionVariant[];
}

export interface DiscriminatedUnionSynthField {
  name: string;
  type: string;
  variantKeys: string[];
}

export interface DiscriminatedUnionSynthEntry {
  domain: string;
  binder: string;
  discriminant: string;
  discriminantType: string;
  variants: Array<{ key: string; literal: DiscriminantLiteral }>;
  fields: DiscriminatedUnionSynthField[];
}

export interface DiscriminatedUnionSynth {
  readonly byShape: ReadonlyMap<string, DiscriminatedUnionSynthEntry>;
  readonly emitted: ReadonlySet<string>;
}

export function emptyDiscriminatedUnionSynth(): DiscriminatedUnionSynth {
  return { byShape: new Map(), emitted: new Set() };
}

function literalKey(lit: DiscriminantLiteral): string {
  return `${lit.kind}:${String(lit.value)}`;
}

function literalPantType(
  lit: DiscriminantLiteral,
  strategy: NumericStrategy,
): string {
  switch (lit.kind) {
    case "string":
      return "String";
    case "number":
      return strategy.mapNumber();
    case "boolean":
      return "Bool";
    default: {
      const _exhaustive: never = lit;
      return _exhaustive;
    }
  }
}

function literalExpr(lit: DiscriminantLiteral): OpaqueExpr | null {
  const ast = getAst();
  switch (lit.kind) {
    case "string":
      return ast.litString(lit.value);
    case "number":
      return Number.isInteger(lit.value) && lit.value >= 0
        ? ast.litNat(lit.value)
        : null;
    case "boolean":
      return ast.litBool(lit.value);
    default: {
      const _exhaustive: never = lit;
      return _exhaustive;
    }
  }
}

function literalCanEmit(lit: DiscriminantLiteral): boolean {
  return (
    lit.kind !== "number" || (Number.isInteger(lit.value) && lit.value >= 0)
  );
}

function literalFromType(type: ts.Type): DiscriminantLiteral | null {
  if (type.flags & ts.TypeFlags.StringLiteral) {
    return { kind: "string", value: (type as ts.StringLiteralType).value };
  }
  if (type.flags & ts.TypeFlags.NumberLiteral) {
    return { kind: "number", value: (type as ts.NumberLiteralType).value };
  }
  if (type.flags & ts.TypeFlags.BooleanLiteral) {
    const intrinsicName = (type as unknown as { intrinsicName?: string })
      .intrinsicName;
    if (intrinsicName === "true") {
      return { kind: "boolean", value: true };
    }
    if (intrinsicName === "false") {
      return { kind: "boolean", value: false };
    }
  }
  return null;
}

function getPropertyType(
  prop: ts.Symbol,
  checker: ts.TypeChecker,
): ts.Type | null {
  const decl = prop.getDeclarations()?.[0];
  if (decl) {
    return checker.getTypeOfSymbolAtLocation(prop, decl);
  }
  return (prop as unknown as { type?: ts.Type }).type ?? null;
}

/**
 * Structural TypeScript discriminated-union detection. A union qualifies
 * when some field is present on every member and has a distinct literal
 * type on each member. The chosen discriminant is deterministic: the first
 * qualifying field in sorted field-name order.
 */
export function detectDiscriminatedUnion(
  type: ts.Type,
  checker: ts.TypeChecker,
): DiscriminatedUnionDetection | null {
  if (!type.isUnion() || type.types.length < 2) {
    return null;
  }
  const members = type.types.filter((t) => !isTsNullish(t));
  if (members.length < 2 || members.length !== type.types.length) {
    return null;
  }
  const memberFields = members.map((member) => {
    const fields = new Map<string, { symbol: ts.Symbol; type: ts.Type }>();
    for (const prop of member.getProperties()) {
      const propType = getPropertyType(prop, checker);
      if (propType) {
        fields.set(prop.getName(), { symbol: prop, type: propType });
      }
    }
    return fields;
  });
  if (memberFields.some((fields) => fields.size === 0)) {
    return null;
  }
  const [firstFields, ...restFields] = memberFields;
  const commonNames = [...firstFields!.keys()]
    .filter((name) => restFields.every((fields) => fields.has(name)))
    .sort((a, b) => a.localeCompare(b));

  for (const name of commonNames) {
    const literals: DiscriminantLiteral[] = [];
    const seen = new Set<string>();
    let qualifies = true;
    for (const fields of memberFields) {
      const lit = literalFromType(fields.get(name)!.type);
      if (!lit) {
        qualifies = false;
        break;
      }
      const key = literalKey(lit);
      if (seen.has(key)) {
        qualifies = false;
        break;
      }
      seen.add(key);
      literals.push(lit);
    }
    if (!qualifies) {
      continue;
    }
    return {
      discriminant: name,
      variants: memberFields.map((fields, i) => ({
        literal: literals[i]!,
        fields: [...fields.entries()].map(([fieldName, entry]) => ({
          name: fieldName,
          type: entry.type,
        })),
      })),
    };
  }
  return null;
}

function disjunction(exprs: OpaqueExpr[], ast = getAst()): OpaqueExpr | null {
  if (exprs.length === 0) {
    return null;
  }
  return exprs
    .slice(1)
    .reduce((acc, expr) => ast.binop(ast.opOr(), acc, expr), exprs[0]!);
}

export function buildDiscriminatedUnionTotalityAssertion(
  entry: DiscriminatedUnionSynthEntry,
  ast = getAst(),
): PropResult {
  const comparisons: OpaqueExpr[] = [];
  for (const variant of entry.variants) {
    const lit = literalExpr(variant.literal);
    if (!lit) {
      return {
        kind: "unsupported",
        reason: `unsupported discriminant literal in ${entry.domain}`,
      };
    }
    comparisons.push(
      ast.binop(
        ast.opEq(),
        ast.app(ast.var(fieldRuleName(entry.domain, entry.discriminant)), [
          ast.var(entry.binder),
        ]),
        lit,
      ),
    );
  }
  const body = disjunction(comparisons, ast);
  if (!body) {
    return {
      kind: "unsupported",
      reason: `discriminated union ${entry.domain} has no variants`,
    };
  }
  return {
    kind: "assertion",
    quantifiers: [ast.param(entry.binder, ast.tName(entry.domain))],
    body,
  };
}

function discriminatedUnionShapeKey(
  discriminant: string,
  variants: ReadonlyArray<{
    key: string;
    fields: ReadonlyArray<RecordSynthField>;
  }>,
): string {
  const variantKeys = variants
    .map(
      (variant) =>
        `${variant.key}{${variant.fields.map(recordFieldShapeKey).join("|")}}`,
    )
    .sort();
  return `${discriminant}::${variantKeys.join("||")}`;
}

// True when a variant field's type contains `needle` — directly, or through
// array / set / map / tuple / union type-arguments — i.e. the union is
// directly self-referential. Object-property nesting is deliberately not
// traversed, so mutual recursion (A holds B holds A) is not treated as
// self-referential and falls to the graceful-refusal backstop. The `seen` set
// bounds this pre-check itself against the recursion it is detecting.
function typeContains(
  haystack: ts.Type,
  needle: ts.Type,
  checker: ts.TypeChecker,
  seen: Set<ts.Type>,
): boolean {
  if (haystack === needle) {
    return true;
  }
  if (seen.has(haystack)) {
    return false;
  }
  seen.add(haystack);
  if (haystack.isUnionOrIntersection()) {
    return haystack.types.some((t) => typeContains(t, needle, checker, seen));
  }
  if (
    haystack.flags & ts.TypeFlags.Object &&
    (haystack as ts.ObjectType).objectFlags & ts.ObjectFlags.Reference
  ) {
    return checker
      .getTypeArguments(haystack as ts.TypeReference)
      .some((a) => typeContains(a, needle, checker, seen));
  }
  return false;
}

function unionIsSelfReferential(
  type: ts.Type,
  detection: DiscriminatedUnionDetection,
  checker: ts.TypeChecker,
): boolean {
  const seen = new Set<ts.Type>();
  return detection.variants.some((variant) =>
    variant.fields.some((field) =>
      typeContains(field.type, type, checker, seen),
    ),
  );
}

/**
 * Single entry point for registering a discriminated-union domain, owning the
 * recursion bookkeeping so the type-mapping branch and field-owner resolution
 * share it. Returns the synthesized domain name, or null on refusal.
 *
 * A directly self-referential union reserves its domain name before mapping
 * fields (via `cell.recursiveDomains`) so a self-referential field resolves to that
 * domain — a sound encoding, since the field accessor is a total uninterpreted
 * function in EUF. Recursion the self-reference pre-check does not cover
 * (mutual / indirect) is refused gracefully via the `cell.visiting` backstop.
 */
function registerDiscriminatedUnionDomain(
  cell: SynthCell,
  type: ts.Type,
  detection: DiscriminatedUnionDetection,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
): string | null {
  // Same recursive type seen before (in-flight self-reference, or an earlier
  // completed registration): reuse its domain by identity.
  const reserved = cell.recursiveDomains.get(type);
  if (
    reserved !== undefined &&
    unionIsSelfReferential(type, detection, checker)
  ) {
    return reserved;
  }
  if (cell.visiting.has(type)) {
    return null;
  }
  if (unionIsSelfReferential(type, detection, checker)) {
    const reg = registerName(
      cell.registry,
      discriminatedUnionPreferredDomain(type),
    );
    cell.registry = reg.registry;
    cell.recursiveDomains.set(type, reg.name);
    const domain = cellRegisterDiscriminatedUnion(
      cell,
      detection,
      checker,
      strategy,
      reg.name,
      reg.name,
    );
    if (domain === null) {
      cell.recursiveDomains.delete(type);
      return null;
    }
    cell.recursiveDomains.set(type, domain);
    return domain;
  }
  cell.visiting.add(type);
  try {
    return cellRegisterDiscriminatedUnion(
      cell,
      detection,
      checker,
      strategy,
      discriminatedUnionPreferredDomain(type),
      undefined,
    );
  } finally {
    cell.visiting.delete(type);
  }
}

export function cellRegisterDiscriminatedUnion(
  cell: SynthCell,
  detection: DiscriminatedUnionDetection,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  preferredDomain = "DiscriminatedUnion",
  // When set, the domain name was pre-reserved by the caller before the field
  // loop (so a self-referential field could resolve to it). Use it verbatim
  // rather than allocating a fresh name; the caller already advanced the
  // registry.
  reservedDomain?: string,
): string | null {
  const variants: Array<{
    key: string;
    literal: DiscriminantLiteral;
    fields: RecordSynthField[];
  }> = [];
  const fieldVariants = new Map<
    string,
    { variantKeys: string[]; types: string[] }
  >();
  let discriminantType: string | null = null;

  for (const variant of detection.variants) {
    const key = literalKey(variant.literal);
    if (!literalCanEmit(variant.literal)) {
      return null;
    }
    const litType = literalPantType(variant.literal, strategy);
    discriminantType ??= litType;
    if (discriminantType !== litType) {
      return null;
    }
    const fields: RecordSynthField[] = [];
    for (const field of variant.fields) {
      if (field.name === detection.discriminant) {
        continue;
      }
      const mapped = mapTsType(field.type, checker, strategy, cell);
      if (
        !mapped.ok ||
        !isValidPantFieldType(mapped.sort) ||
        !/^[A-Za-z_][A-Za-z0-9_]*$/u.test(field.name)
      ) {
        return null;
      }
      fields.push({ name: field.name, type: mapped.sort });
      const slot = fieldVariants.get(field.name) ?? {
        variantKeys: [],
        types: [],
      };
      slot.variantKeys.push(key);
      if (!slot.types.includes(mapped.sort)) {
        slot.types.push(mapped.sort);
      }
      fieldVariants.set(field.name, slot);
    }
    fields.sort((a, b) => a.name.localeCompare(b.name));
    variants.push({ key, literal: variant.literal, fields });
  }
  if (!discriminantType) {
    return null;
  }

  const key = discriminatedUnionShapeKey(
    detection.discriminant,
    variants.map((variant) => ({ key: variant.key, fields: variant.fields })),
  );
  const cached = cell.discriminatedUnionSynth.byShape.get(key);
  if (cached) {
    return cached.domain;
  }

  let domainName: string;
  let registryForBinder: NameRegistry;
  if (reservedDomain === undefined) {
    const reg1 = registerName(cell.registry, preferredDomain);
    domainName = reg1.name;
    registryForBinder = reg1.registry;
  } else {
    domainName = reservedDomain;
    registryForBinder = cell.registry;
  }
  const bReg = registerName(registryForBinder, "s");
  const entry: DiscriminatedUnionSynthEntry = {
    domain: domainName,
    binder: bReg.name,
    discriminant: detection.discriminant,
    discriminantType,
    variants: variants.map((variant) => ({
      key: variant.key,
      literal: variant.literal,
    })),
    fields: [...fieldVariants.entries()]
      .map(([name, info]) => ({
        name,
        type: info.types.join(" + "),
        variantKeys: info.variantKeys,
      }))
      .sort((a, b) => a.name.localeCompare(b.name)),
  };
  const newByShape = new Map(cell.discriminatedUnionSynth.byShape);
  newByShape.set(key, entry);
  cell.discriminatedUnionSynth = {
    byShape: newByShape,
    emitted: cell.discriminatedUnionSynth.emitted,
  };
  cell.registry = bReg.registry;
  return entry.domain;
}

export function emitDiscriminatedUnionSynthDecls(
  synth: DiscriminatedUnionSynth,
  registry: NameRegistry,
): {
  decls: PantDeclaration[];
  assertions: PropResult[];
  synth: DiscriminatedUnionSynth;
  registry: NameRegistry;
} {
  const decls: PantDeclaration[] = [];
  const assertions: PropResult[] = [];
  const ast = getAst();
  const newEmitted = new Set(synth.emitted);
  for (const [key, entry] of synth.byShape) {
    if (newEmitted.has(key)) {
      continue;
    }
    newEmitted.add(key);
    assertions.push(buildDiscriminatedUnionTotalityAssertion(entry, ast));
    const discRule = fieldRuleName(entry.domain, entry.discriminant);
    const guardByVariant = new Map<string, OpaqueExpr>();
    for (const variant of entry.variants) {
      const lit = literalExpr(variant.literal);
      if (!lit) {
        continue;
      }
      guardByVariant.set(
        variant.key,
        ast.binop(
          ast.opEq(),
          ast.app(ast.var(discRule), [ast.var(entry.binder)]),
          lit,
        ),
      );
    }
    decls.push({ kind: "domain", name: entry.domain });
    decls.push({
      kind: "rule",
      name: discRule,
      params: [{ name: entry.binder, type: entry.domain }],
      returnType: entry.discriminantType,
    });
    for (const field of entry.fields) {
      const guards = field.variantKeys
        .map((variantKey) => guardByVariant.get(variantKey))
        .filter((guard): guard is OpaqueExpr => guard !== undefined);
      const guard = disjunction(guards);
      if (!guard) {
        continue;
      }
      decls.push({
        kind: "rule",
        name: fieldRuleName(entry.domain, field.name),
        params: [{ name: entry.binder, type: entry.domain }],
        returnType: field.type,
        guard,
      });
    }
  }
  return {
    decls,
    assertions,
    synth: { byShape: synth.byShape, emitted: newEmitted },
    registry,
  };
}

export interface OpaqueSynthEntry {
  readonly id: string;
  readonly rule: string;
  readonly sort: string;
}

/**
 * Accumulates opaque-value identities used by a module and synthesizes the
 * shared `Opaque` domain plus one nullary constant per identity. Like the
 * other synths, registration is idempotent and emission is incremental.
 *
 * No distinctness axiom is emitted: different ids receive different rule
 * names, but the solver remains free to unify or separate their values.
 */
export interface OpaqueSynth {
  readonly byId: ReadonlyMap<string, OpaqueSynthEntry>;
  readonly emitted: ReadonlySet<string>;
  readonly needsDomain: boolean;
  readonly domainEmitted: boolean;
}

/**
 * @pant all s: String | ~(s in emitted emptyOpaqueSynth).
 * @pant ~(needsDomain emptyOpaqueSynth).
 * @pant ~(domainEmitted emptyOpaqueSynth).
 */
export function emptyOpaqueSynth(): OpaqueSynth {
  return {
    byId: new Map(),
    emitted: new Set(),
    needsDomain: false,
    domainEmitted: false,
  };
}

export function registerOpaqueDomain(synth: OpaqueSynth): OpaqueSynth {
  if (synth.needsDomain) {
    return synth;
  }
  return { ...synth, needsDomain: true };
}

export function registerOpaqueValue(
  synth: OpaqueSynth,
  id: string,
): { entry: OpaqueSynthEntry; synth: OpaqueSynth } {
  return registerSynthesizedValue(synth, id, OPAQUE_DOMAIN);
}

export function registerSynthesizedValue(
  synth: OpaqueSynth,
  id: string,
  sort: string,
): { entry: OpaqueSynthEntry; synth: OpaqueSynth } {
  const cached = synth.byId.get(id);
  if (cached) {
    return { entry: cached, synth };
  }
  const entry: OpaqueSynthEntry = {
    id,
    rule: opaqueValueRuleName(id),
    sort,
  };
  const newById = new Map(synth.byId);
  newById.set(id, entry);
  return {
    entry,
    synth: {
      ...synth,
      byId: newById,
      needsDomain: synth.needsDomain || sort === OPAQUE_DOMAIN,
    },
  };
}

/**
 * @pant lookupOpaqueValue synth id = byId synth id.
 */
export function lookupOpaqueValue(
  synth: OpaqueSynth,
  id: string,
): OpaqueSynthEntry | undefined {
  return synth.byId.get(id);
}

/**
 * Materialize accumulated synthesized-value decls in registration order.
 * The `Opaque` domain is emitted only when at least one registered value
 * carries the `Opaque` sort, and only on the first drain that needs it.
 * Each registered id emits one nullary constant
 * `opaqueValueRuleName(id) => <sort>.`.
 */
export function emitOpaqueSynthDecls(
  synth: OpaqueSynth,
  registry: NameRegistry,
): { decls: PantDeclaration[]; synth: OpaqueSynth; registry: NameRegistry } {
  const decls: PantDeclaration[] = [];
  const newEmitted = new Set(synth.emitted);
  const hasUnemittedOpaqueValue = [...synth.byId.entries()].some(
    ([id, entry]) => !newEmitted.has(id) && entry.sort === OPAQUE_DOMAIN,
  );
  let domainEmitted = synth.domainEmitted;
  if (!domainEmitted && (synth.needsDomain || hasUnemittedOpaqueValue)) {
    decls.push({ kind: "domain", name: OPAQUE_DOMAIN });
    domainEmitted = true;
  }
  for (const [id, entry] of synth.byId) {
    if (newEmitted.has(id)) {
      continue;
    }
    newEmitted.add(id);
    decls.push({
      kind: "rule",
      name: entry.rule,
      params: [],
      returnType: entry.sort,
    });
  }
  return {
    decls,
    synth: { ...synth, emitted: newEmitted, domainEmitted },
    registry,
  };
}

export interface ForeignDomainEntry {
  readonly key: string;
  readonly domain: string;
}

export interface ForeignDomainSynth {
  readonly byKey: ReadonlyMap<string, ForeignDomainEntry>;
  readonly emitted: ReadonlySet<string>;
}

/**
 * @pant all k: String | ~(foreign-domain-synth--by-key-key emptyForeignDomainSynth k).
 * @pant all k: String | ~(k in foreign-domain-synth--emitted emptyForeignDomainSynth).
 */
export function emptyForeignDomainSynth(): ForeignDomainSynth {
  return { byKey: new Map(), emitted: new Set() };
}

function toPantDomainName(name: string): string {
  const words = toPantTermName(name)
    .split("-")
    .filter((part) => part.length > 0);
  const stem = words.map(capitalize).join("");
  return stem.length > 0 ? stem : "Foreign";
}

export function registerForeignDomain(
  synth: ForeignDomainSynth,
  registry: NameRegistry,
  key: string,
  preferredName: string,
): {
  domain: string;
  synth: ForeignDomainSynth;
  registry: NameRegistry;
} {
  const cached = synth.byKey.get(key);
  if (cached) {
    return { domain: cached.domain, synth, registry };
  }
  const reg = registerName(
    registry,
    `Foreign${toPantDomainName(preferredName)}`,
  );
  const entry: ForeignDomainEntry = { key, domain: reg.name };
  const byKey = new Map(synth.byKey);
  byKey.set(key, entry);
  return {
    domain: entry.domain,
    synth: { ...synth, byKey },
    registry: reg.registry,
  };
}

export function emitForeignDomainSynthDecls(synth: ForeignDomainSynth): {
  decls: PantDeclaration[];
  synth: ForeignDomainSynth;
} {
  const decls: PantDeclaration[] = [];
  const emitted = new Set(synth.emitted);
  for (const [key, entry] of synth.byKey) {
    if (emitted.has(key)) {
      continue;
    }
    emitted.add(key);
    decls.push({ kind: "domain", name: entry.domain });
  }
  return { decls, synth: { ...synth, emitted } };
}

export interface ForeignAccessorEntry {
  readonly key: string;
  readonly ownerDomain: string;
  readonly fieldName: string;
  readonly returnSort: string;
  readonly ruleName: string;
}

export interface ForeignAccessorSynth {
  readonly byKey: ReadonlyMap<string, ForeignAccessorEntry>;
  readonly emitted: ReadonlySet<string>;
}

/**
 * @pant all k: String | ~(foreign-accessor-synth--by-key-key emptyForeignAccessorSynth k).
 * @pant all k: String | ~(k in foreign-accessor-synth--emitted emptyForeignAccessorSynth).
 */
export function emptyForeignAccessorSynth(): ForeignAccessorSynth {
  return { byKey: new Map(), emitted: new Set() };
}

/**
 * Key shallow foreign accessors by the full typed read shape. The return sort
 * is part of the key so a checker-observed type change cannot silently reuse a
 * stale declaration with a different result type.
 */
function foreignAccessorKey(
  ownerDomain: string,
  fieldName: string,
  returnSort: string,
): string {
  return JSON.stringify([ownerDomain, fieldName, returnSort]);
}

function foreignOwnerLeaf(ownerDomain: string): string {
  const unprefixed = ownerDomain.replace(/^Foreign/u, "");
  const words = toPantTermName(unprefixed || ownerDomain)
    .split("-")
    .filter((part) => part.length > 0);
  if (words.length === 0) {
    return "foreign";
  }
  const last = words[words.length - 1]!;
  if (last === "list" && words.length > 1) {
    return `${words[words.length - 2]!}-list`;
  }
  return last;
}

function foreignAccessorBinderName(ownerDomain: string): string {
  const leaf = foreignOwnerLeaf(ownerDomain);
  const parts = leaf.split("-");
  return parts.map((part) => part[0] ?? "").join("");
}

function isForeignReturnSort(returnSort: string): boolean {
  return returnSort
    .replace(/\s+/gu, "")
    .split("+")
    .every((part) => {
      const unlisted = part.replace(/^\[+/u, "").replace(/\]+$/u, "");
      return unlisted.startsWith("Foreign");
    });
}

/**
 * Stable base rule name for a synthesized foreign accessor. Accessors that
 * project another foreign value keep the TypeScript property name (`items`,
 * `name`, `declarations`), while primitive projections include the receiver
 * leaf (`item-label`, `identifier-text`) so common field names remain readable
 * and less collision-prone. `registerForeignAccessor` still routes this base
 * through the document name registry; on actual collisions it falls back to
 * the same owner/field boundary used by `fieldRuleName`.
 */
export function foreignAccessorRuleName(
  ownerDomain: string,
  fieldName: string,
  returnSort: string,
): string {
  const field = toPantTermName(fieldName);
  if (isForeignReturnSort(returnSort)) {
    return field;
  }
  return `${foreignOwnerLeaf(ownerDomain)}-${field}`;
}

export function registerForeignAccessor(
  synth: ForeignAccessorSynth,
  registry: NameRegistry,
  ownerDomain: string,
  fieldName: string,
  returnSort: string,
): {
  entry: ForeignAccessorEntry | null;
  synth: ForeignAccessorSynth;
  registry: NameRegistry;
} {
  if (!isValidPantFieldType(ownerDomain) || !isValidPantFieldType(returnSort)) {
    return { entry: null, synth, registry };
  }
  const key = foreignAccessorKey(ownerDomain, fieldName, returnSort);
  const cached = synth.byKey.get(key);
  if (cached) {
    return { entry: cached, synth, registry };
  }
  const preferred = foreignAccessorRuleName(ownerDomain, fieldName, returnSort);
  const claimed = registerName(registry, preferred);
  const final =
    claimed.name === preferred
      ? claimed
      : registerName(registry, fieldRuleName(ownerDomain, fieldName));
  const entry: ForeignAccessorEntry = {
    key,
    ownerDomain,
    fieldName,
    returnSort,
    ruleName: final.name,
  };
  const byKey = new Map(synth.byKey);
  byKey.set(key, entry);
  return {
    entry,
    synth: { ...synth, byKey },
    registry: final.registry,
  };
}

export function emitForeignAccessorSynthDecls(synth: ForeignAccessorSynth): {
  decls: PantDeclaration[];
  synth: ForeignAccessorSynth;
} {
  const decls: PantDeclaration[] = [];
  const emitted = new Set(synth.emitted);
  for (const [key, entry] of synth.byKey) {
    if (emitted.has(key)) {
      continue;
    }
    emitted.add(key);
    decls.push({
      kind: "rule",
      name: entry.ruleName,
      params: [
        {
          name: foreignAccessorBinderName(entry.ownerDomain),
          type: entry.ownerDomain,
        },
      ],
      returnType: entry.returnSort,
    });
  }
  return { decls, synth: { ...synth, emitted } };
}

/**
 * Canonical tuple shape: the ordered Pantagruel element types that a
 * tuple constructor builds. Two TS aliases that share the same shape
 * — `Point = [number, number]` and `Vec2 = [number, number]` both map
 * to `["Int", "Int"]` — must share one constructor. Otherwise the
 * SMT solver cannot prove `make-point 0 0 = make-vec2 0 0` despite
 * identical operands; EUF congruence (Kroening & Strichman, *Decision
 * Procedures*, Ch. 8) requires dispatch on shape, not on alias.
 */
export interface TupleShape {
  readonly elementPantTypes: readonly string[];
}

/**
 * Reference to a synthesized tuple constructor returned by
 * `cellRegisterTupleConstructor`. The `ctorRuleName` is the bare rule
 * name in the dep module (e.g. `make-int-int`); the `depModuleName` is
 * the per-source-file dep module that owns it (e.g. `FOO_TUPLES`).
 * Consumers emit `import ${depModuleName}.` at the module head and
 * reference the constructor as `${depModuleName}::${ctorRuleName}` at
 * call sites, mirroring the cross-module shape Pantagruel's `import`
 * machinery exercises in `lib/module.ml`.
 */
export interface TupleCtorRef {
  readonly ctorRuleName: string;
  readonly depModuleName: string;
}

interface TupleCtorEntry {
  readonly shape: TupleShape;
  readonly ctorRuleName: string;
}

/**
 * Accumulates anonymous tuple-shape occurrences encountered anywhere in
 * the module's expression positions and synthesizes one constructor rule
 * + projection axioms per unique shape. Like `MapSynth` and `RecordSynth`,
 * registration is idempotent on canonical shape: re-registering the same
 * `elementPantTypes` returns the cached constructor name. The synthesized
 * decls are emitted to a *separate* dep module via `emitTupleCtorModule`,
 * not folded into the consumer document; that's the per-source-file scope
 * decision (one `<FILE_BASE_NAME>_TUPLES` module per source file regardless
 * of how many shapes it uses).
 *
 * `ctorRegistry` is a *tuple-module-local* `NameRegistry` used to
 * disambiguate constructor names within the dep module's namespace.
 * Routing ctor allocation through the consumer's `cell.registry` would
 * leak unrelated local declarations (e.g. a user function named
 * `make-int-int`) into the suffixing chain — yielding `make-int-int1`
 * even though the dep-module qualifier `FOO_TUPLES::` already isolates
 * the namespace. The local registry produces stable canonical names
 * (`make-int-int` regardless of what's registered in the consumer)
 * while still resolving the rare case where two distinct shapes mangle
 * to the same kebab fragment (e.g. element types whose canonical names
 * collide after kebab normalization).
 */
export interface TupleSynth {
  readonly byShape: ReadonlyMap<string, TupleCtorEntry>;
  readonly ctorRegistry: NameRegistry;
}

/**
 * @pant ctorRegistry emptyTupleSynth = emptyNameRegistry.
 */
export function emptyTupleSynth(): TupleSynth {
  return { byShape: new Map(), ctorRegistry: emptyNameRegistry() };
}

/**
 * Canonical key for tuple-shape dedup: a delimiter-safe encoding of
 * the element Pant types. Each element is whitespace-stripped first
 * so `Int * Int` and `Int*Int` hash identically (the canonical key
 * is purely structural). The encoding uses `JSON.stringify` rather
 * than a join because `*` is *also* legal inside an element type
 * (a nested tuple like `Int*Int`), and a bare `*` join would collide
 * `[Int, Int*Int]` with `[Int*Int, Int]` on the same `Int*Int*Int`
 * key. JSON's quoting and escaping make the per-element boundaries
 * unambiguous.
 */
function tupleShapeKey(shape: TupleShape): string {
  return JSON.stringify(
    shape.elementPantTypes.map((t) => t.replace(/\s+/gu, "")),
  );
}

/**
 * Compute the kebab-cased constructor name for a tuple shape. Each
 * element type is mangled to a fragment (`[String]` → `ListString`,
 * `A * B` → `AAndB`, etc.) and then kebab-cased; fragments are joined
 * with `-` and prefixed with `make-`. Returns null if any element
 * type is unmangleable (e.g. contains TS-compiler fallback text like
 * `Map<string, number>`).
 */
function tupleCtorBaseName(shape: TupleShape): string | null {
  const fragments: string[] = [];
  for (const elem of shape.elementPantTypes) {
    const frag = manglePantTypeToFragment(elem);
    if (!frag) {
      return null;
    }
    fragments.push(toPantTermName(frag));
  }
  return `make-${fragments.join("-")}`;
}

/**
 * @pant prefixedDigitStem stem = "F_" + stem.
 */
function prefixedDigitStem(stem: string): string {
  // biome-ignore lint/style/useConst: deliberate immutable-let dogfood fixture
  let prefix = "F_";
  return `${prefix}${stem}`;
}

/**
 * @pant tupleDepModuleName safeStem = safeStem + "_TUPLES".
 */
function tupleDepModuleName(safeStem: string): string {
  // biome-ignore lint/style/useConst: deliberate immutable-let dogfood fixture
  let suffix = "_TUPLES";
  return `${safeStem}${suffix}`;
}

/**
 * Derive the per-source-file dep module name for tuple constructors.
 * `<FILE_BASE_NAME>_TUPLES` in ALL_CAPS_SNAKE — every emitted module
 * name in this codebase uses the screaming-snake convention (matches
 * the existing `samples/*.pant` corpus).
 *
 * The result must be a legal Pantagruel UPPER_IDENT — the lexer's
 * `upper_start = 'A' .. 'Z'` rules out:
 *   - empty stems (`.ts`, `""` → `_TUPLES`),
 *   - underscore-leading stems (`_foo.ts` → `_FOO_TUPLES`),
 *   - digit-leading stems (`123-foo.ts` → `123_FOO_TUPLES`).
 * Strip leading underscores; if the remaining stem starts with a
 * digit, prefix `F_` (the `F` is just a stable letter — no semantics).
 * If the stem is empty after stripping, fall back to the unprefixed
 * `TUPLES`, matching the no-sourceFile fallback in
 * `cellRegisterTupleConstructor`.
 *
 * @pant depModuleNameForFile fileName = depModuleNameForFileImpl fileName.
 * @pant depModuleNameForFile fileName = depModuleNameForFile fileName.
 */
export function depModuleNameForFile(fileName: string): string {
  return depModuleNameForFileImpl(fileName);
}

function depModuleNameForFileImpl(fileName: string): string {
  const base = fileName.replace(/^.*[/\\]/u, "").replace(/\.[^.]+$/u, "");
  const upper = base.replace(/[^A-Za-z0-9]+/gu, "_").toUpperCase();
  const stem = upper.replace(/^_+/u, "");
  if (stem.length === 0) {
    return "TUPLES";
  }
  const safeStem = /^[A-Z]/u.test(stem) ? stem : prefixedDigitStem(stem);
  return tupleDepModuleName(safeStem);
}

/**
 * Mutable cell bundling per-document synth state and the name registry.
 * Used by deep call sites (mapTsType recursion, body translation) that
 * would otherwise have to thread state through every return value. The
 * fields are reassigned in place with freshly-computed immutable records;
 * the inner values remain pure. Cell-field assignment is itself within
 * ts2pant's self-translation envelope (translatable as primed rules on
 * the cell).
 *
 * `sourceFile` is optional and carries the file currently being
 * translated so per-file dep-module names (`<FILE_BASE_NAME>_TUPLES`,
 * `<FILE_BASE_NAME>_AMBIENT`) can be derived without threading the file
 * through every registration call. Tests and standalone callers may
 * leave it undefined; tuple-constructor registration falls back to a
 * generic `TUPLES` module name in that case.
 */
export interface SynthCell {
  synth: MapSynth;
  recordSynth: RecordSynth;
  discriminatedUnionSynth: DiscriminatedUnionSynth;
  opaqueSynth: OpaqueSynth;
  foreignDomainSynth: ForeignDomainSynth;
  foreignAccessorSynth: ForeignAccessorSynth;
  registry: NameRegistry;
  tupleSynth: TupleSynth;
  /**
   * Types currently mid-registration as a synthesized domain (discriminated
   * union or anonymous record). Bounds synthesis-time recursion: a
   * variant/field whose type is the container being registered re-enters
   * `mapTsType` on the same `ts.Type`, so without this guard a self-referential
   * type (e.g. `IR1Expr`) recurses forever. Transient — added before a
   * container's field loop and removed (in a `finally`) after, so it is empty
   * between top-level registrations.
   */
  visiting: Set<ts.Type>;
  /**
   * Self-referential discriminated-union types, mapped to their synthesized
   * domain name. The name is reserved before the field loop so a
   * self-referential field resolves to the in-flight domain (a sound
   * synthesized domain — the accessor is a total uninterpreted function in
   * EUF) rather than recursing. The entry is *retained after registration* as
   * an identity cache: a recursive type's self-referential field poisons the
   * structural `byShape` key (it embeds the reserved name), so re-encounters
   * of the same `ts.Type` must dedupe by identity here, not by shape. Removed
   * only if registration fails.
   */
  recursiveDomains: Map<ts.Type, string>;
  sourceFile?: ts.SourceFile;
  /**
   * Dep modules that build sites have requested at translation time.
   * Populated by recognizers that emit references against bundled
   * stdlib modules (e.g., the template-literal recognizer registering
   * `TS_PRELUDE` when it lowers a non-string substitution through
   * `int-to-string` / `real-to-string`). The pipeline drains this set
   * into `doc.imports` + `doc.bundleModules` after body translation.
   *
   * Typed as `Set<DepModuleName>` so registrations are constrained to
   * the closed bundled-stdlib enum and `pipeline.ts`'s consumer can
   * skip the `as DepModuleName` widening cast. Snapshotted in
   * `tryRecognizeFunctorLift`'s rollback path so a probe that fires a
   * stringification (and registers `TS_PRELUDE`) before failing later
   * checks doesn't leak the import into the consumer document.
   */
  imports: Set<DepModuleName>;
  definednessObligations: { text: string }[];
}

export function newSynthCell(
  registry?: NameRegistry,
  sourceFile?: ts.SourceFile,
): SynthCell {
  const cell: SynthCell = {
    synth: emptyMapSynth(),
    recordSynth: emptyRecordSynth(),
    discriminatedUnionSynth: emptyDiscriminatedUnionSynth(),
    opaqueSynth: emptyOpaqueSynth(),
    foreignDomainSynth: emptyForeignDomainSynth(),
    foreignAccessorSynth: emptyForeignAccessorSynth(),
    registry: registry ?? emptyNameRegistry(),
    tupleSynth: emptyTupleSynth(),
    visiting: new Set(),
    recursiveDomains: new Map(),
    imports: new Set(),
    definednessObligations: [],
  };
  if (sourceFile !== undefined) {
    cell.sourceFile = sourceFile;
  }
  return cell;
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
/**
 * @pant fieldRuleName interfaceName fieldName = (toPantTermName interfaceName) + "--" + (toPantTermName fieldName).
 */
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
 *  the synth cell. Intersections collect distinct owners across members.
 *  Non-discriminated unions resolve only when every non-nullish member
 *  resolves to the same single owner; otherwise they return `ambiguous`
 *  so callers refuse the access rather than emit a semantically wrong
 *  qualified symbol. */
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
  if (
    owners.size === 0 &&
    receiverType.isUnion() &&
    detectDiscriminatedUnion(receiverType, checker) === null &&
    receiverType.types.some((t) => !isTsNullish(t))
  ) {
    return { kind: "ambiguous", owners: [] };
  }
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

  if (synthCell && ty.isUnion()) {
    const detection = detectDiscriminatedUnion(ty, checker);
    if (
      detection &&
      (fieldName === detection.discriminant ||
        detection.variants.some((variant) =>
          variant.fields.some((field) => field.name === fieldName),
        ))
    ) {
      const owner = registerDiscriminatedUnionDomain(
        synthCell,
        ty,
        detection,
        checker,
        strategy,
      );
      if (owner) {
        out.add(owner);
        return;
      }
    }
  }

  if (ty.isUnion()) {
    const nonNullTypes = ty.types.filter((sub) => !isTsNullish(sub));
    if (nonNullTypes.length === 0) {
      return;
    }
    let commonOwner: string | null = null;
    for (const sub of nonNullTypes) {
      const memberOwners = new Set<string>();
      collectFieldOwners(
        sub,
        fieldName,
        checker,
        strategy,
        synthCell,
        memberOwners,
      );
      if (memberOwners.size !== 1) {
        return;
      }
      const owner = [...memberOwners][0]!;
      if (commonOwner === null) {
        commonOwner = owner;
      } else if (commonOwner !== owner) {
        return;
      }
    }
    if (commonOwner !== null) {
      out.add(commonOwner);
    }
    return;
  }

  if (ty.isIntersection()) {
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

function discriminatedUnionPreferredDomain(type: ts.Type): string {
  return (
    type.aliasSymbol?.getName() ??
    type.symbol?.getName() ??
    "DiscriminatedUnion"
  );
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
    // Only a successfully-synthesized domain is a usable record owner. Any
    // failure (synth-unavailable, or a field typed `unknown`/recursive)
    // falls through to the named-symbol path; for an anonymous shape there is
    // no meaningful fallback owner, so the resolver returns null and the
    // caller rejects the field access.
    if (mapped.ok) {
      return mapped.sort;
    }
  }
  const sym = type.symbol ?? type.aliasSymbol;
  const symName = sym?.getName();
  if (sym && symName && symName !== "__type") {
    return symName;
  }
  return null;
}

/** Cell-mutating wrapper around `registerMapKV` for the legacy call shape.
 *  Callers pass already-mapped sorts (unwrapped from `mapTsType`'s Result), so
 *  a refusal can no longer reach this synthesizer as a sentinel string. */
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
 *  canonical (alphabetically sorted) order. Field types are already-mapped
 *  sorts (unwrapped from `mapTsType`'s Result). */
export function cellRegisterRecord(
  cell: SynthCell,
  fields: ReadonlyArray<RecordSynthField>,
): string | null {
  const r = registerRecordShape(cell.recordSynth, cell.registry, fields);
  cell.recordSynth = r.synth;
  cell.registry = r.registry;
  return r.domain;
}

/**
 * Cell read-through for `lookupRecordShape`. `fields` must be canonical.
 *
 * @pant cellLookupRecord cell fields = lookupRecordShape (recordSynth cell) fields.
 */
export function cellLookupRecord(
  cell: SynthCell,
  fields: ReadonlyArray<RecordSynthField>,
): RecordSynthEntry | undefined {
  return lookupRecordShape(cell.recordSynth, fields);
}

/** Cell-mutating wrapper around `registerOpaqueValue`. */
export function cellRegisterOpaqueValue(
  cell: SynthCell,
  id: string,
): OpaqueSynthEntry {
  return cellRegisterSynthesizedValue(cell, id, OPAQUE_DOMAIN);
}

/** Cell-mutating wrapper around `registerSynthesizedValue`. */
export function cellRegisterSynthesizedValue(
  cell: SynthCell,
  id: string,
  sort: string,
): OpaqueSynthEntry {
  const r = registerSynthesizedValue(cell.opaqueSynth, id, sort);
  cell.opaqueSynth = r.synth;
  return r.entry;
}

/** Cell-mutating wrapper around `registerOpaqueDomain`. */
export function cellRegisterOpaqueDomain(cell: SynthCell): void {
  cell.opaqueSynth = registerOpaqueDomain(cell.opaqueSynth);
}

/**
 * Cell read-through for `lookupOpaqueValue`.
 *
 * @pant cellLookupOpaqueValue cell id = lookupOpaqueValue (opaqueSynth cell) id.
 */
export function cellLookupOpaqueValue(
  cell: SynthCell,
  id: string,
): OpaqueSynthEntry | undefined {
  return lookupOpaqueValue(cell.opaqueSynth, id);
}

/** Cell-mutating wrapper around `registerForeignAccessor`. */
export function cellRegisterForeignAccessor(
  cell: SynthCell,
  ownerDomain: string,
  fieldName: string,
  returnSort: string,
): ForeignAccessorEntry | null {
  const r = registerForeignAccessor(
    cell.foreignAccessorSynth,
    cell.registry,
    ownerDomain,
    fieldName,
    returnSort,
  );
  cell.foreignAccessorSynth = r.synth;
  cell.registry = r.registry;
  return r.entry;
}

/** Cell-mutating wrapper around `registerName`. */
export function cellRegisterName(cell: SynthCell, name: string): string {
  const r = registerName(cell.registry, toPantTermName(name));
  cell.registry = r.registry;
  return r.name;
}

/**
 * Cell read-through for `isUsed`.
 *
 * @pant cellIsUsed cell name <-> (toPantTermName name) in used (registry cell).
 * @pant (cellIsUsed cell name) or ~(cellIsUsed cell name).
 */
export function cellIsUsed(cell: SynthCell, name: string): boolean {
  return cell.registry.used.has(toPantTermName(name));
}

/** Cell-mutating wrapper that drains Opaque, foreign domains/accessors, Map,
 *  discriminated-union, and Record synth decls. Emits Opaque first so later
 *  decls can reference it, then foreign domains before foreign accessors, and
 *  then Maps so Record/DU accessor-rule return types can reference any Map
 *  domain registered bottom-up. Incremental: each call returns
 *  only the entries added since the previous drain.
 *
 *  Tuple-constructor synth is *not* drained here — tuple constructors
 *  emit to a separate per-source-file dep module via
 *  `emitTupleCtorModule`, not into the consumer document's head. */
export function cellEmitSynth(cell: SynthCell): {
  decls: PantDeclaration[];
  assertions: PropResult[];
} {
  const opaqueR = emitOpaqueSynthDecls(cell.opaqueSynth, cell.registry);
  cell.opaqueSynth = opaqueR.synth;
  cell.registry = opaqueR.registry;
  const foreignR = emitForeignDomainSynthDecls(cell.foreignDomainSynth);
  cell.foreignDomainSynth = foreignR.synth;
  const foreignAccessorR = emitForeignAccessorSynthDecls(
    cell.foreignAccessorSynth,
  );
  cell.foreignAccessorSynth = foreignAccessorR.synth;
  const mapR = emitSynthDecls(cell.synth, cell.registry);
  cell.synth = mapR.synth;
  cell.registry = mapR.registry;
  const duR = emitDiscriminatedUnionSynthDecls(
    cell.discriminatedUnionSynth,
    cell.registry,
  );
  cell.discriminatedUnionSynth = duR.synth;
  cell.registry = duR.registry;
  const recR = emitRecordSynthDecls(cell.recordSynth, cell.registry);
  cell.recordSynth = recR.synth;
  cell.registry = recR.registry;
  return {
    decls: [
      ...opaqueR.decls,
      ...foreignR.decls,
      ...foreignAccessorR.decls,
      ...mapR.decls,
      ...duR.decls,
      ...recR.decls,
    ],
    assertions: duR.assertions,
  };
}

/**
 * Register a tuple shape and return the constructor reference. Idempotent
 * on canonical shape (whitespace-stripped element types joined by `*`):
 * re-registering the same shape returns the cached constructor name and
 * dep-module name. Returns null when any element type is unmangleable
 * (e.g. contains TS-compiler fallback text from an unsupported type
 * upstream); callers fall back to inline tuple emission or reject.
 *
 * The dep-module name is `<FILE_BASE_NAME>_TUPLES` (ALL_CAPS_SNAKE)
 * derived from `cell.sourceFile`; if no source file is attached to the
 * cell (test/standalone paths), it falls back to a generic `TUPLES`.
 *
 * Constructor names resolve in the tuple module's *own* namespace
 * (`cell.tupleSynth.ctorRegistry`) — not the consumer's `cell.registry`.
 * The dep module's qualifier (`FOO_TUPLES::make-int-int`) already
 * isolates the namespace; routing through the consumer registry would
 * let unrelated local declarations turn `make-int-int` into
 * `make-int-int1`. Keep `cell.registry` unchanged so the consumer's
 * binder allocation is not perturbed by tuple-shape registration.
 */
export function cellRegisterTupleConstructor(
  cell: SynthCell,
  shape: TupleShape,
): TupleCtorRef | null {
  // `elementPantTypes` are already-mapped sorts (unwrapped from `mapTsType`'s
  // Result), so a refusal can no longer reach this synthesizer.
  const depModuleName = cell.sourceFile
    ? depModuleNameForFile(cell.sourceFile.fileName)
    : "TUPLES";
  const key = tupleShapeKey(shape);
  const cached = cell.tupleSynth.byShape.get(key);
  if (cached) {
    return { ctorRuleName: cached.ctorRuleName, depModuleName };
  }
  const baseName = tupleCtorBaseName(shape);
  if (!baseName) {
    return null;
  }
  const reg1 = registerName(cell.tupleSynth.ctorRegistry, baseName);
  const ctorRuleName = reg1.name;
  const newByShape = new Map(cell.tupleSynth.byShape);
  newByShape.set(key, {
    shape: { elementPantTypes: [...shape.elementPantTypes] },
    ctorRuleName,
  });
  cell.tupleSynth = { byShape: newByShape, ctorRegistry: reg1.registry };
  return { ctorRuleName, depModuleName };
}

/** Read-through accessor: return all registered tuple shapes (in
 *  registration order) so the pipeline can pass them to
 *  `emitTupleCtorModule` at the document-emit boundary. */
export function cellTupleShapes(
  cell: SynthCell,
): ReadonlyArray<{ shape: TupleShape; ctorRuleName: string }> {
  return [...cell.tupleSynth.byShape.values()];
}

/**
 * Emit the standalone Pantagruel dep module for a source file's tuple
 * constructors. Output structure:
 *
 *   module <FILE_BASE_NAME>_TUPLES.
 *
 *   <ctor> a1: T1, ..., aN: TN => T1 * ... * TN.
 *   ...
 *
 *   ---
 *   all a1: T1, ..., aN: TN | (<ctor> a1 ... aN).1 = a1.
 *   ...
 *
 * One constructor rule head per registered shape, plus N projection
 * axioms (one per element position) per shape. The axioms quantify
 * over the constructor's parameters explicitly — Pantagruel propositions
 * can't carry free variables, so the binders must be reintroduced at
 * the proposition level (parser.mly: proposition is `expr DOT`, where
 * `expr` may be a `quantified` form `all p:T,... | body`).
 *
 * Every shape used by any fixture in the source file aggregates into
 * the single returned module — that's the per-source-file scope
 * decision (one `import <FILE_BASE_NAME>_TUPLES.` per consumer
 * regardless of how many tuple shapes the file uses).
 */
export function emitTupleCtorModule(
  sourceFile: ts.SourceFile,
  shapes: ReadonlyArray<{ shape: TupleShape; ctorRuleName: string }>,
): string {
  const moduleName = depModuleNameForFile(sourceFile.fileName);
  const lines: string[] = [];
  lines.push(`module ${moduleName}.`);
  lines.push("");

  for (const { shape, ctorRuleName } of shapes) {
    const params = shape.elementPantTypes
      .map((t, idx) => `a${idx + 1}: ${t}`)
      .join(", ");
    const tupleType = shape.elementPantTypes.join(" * ");
    lines.push(`${ctorRuleName} ${params} => ${tupleType}.`);
  }

  lines.push("");
  lines.push("---");
  lines.push("");

  for (const { shape, ctorRuleName } of shapes) {
    const params = shape.elementPantTypes
      .map((t, idx) => `a${idx + 1}: ${t}`)
      .join(", ");
    const argList = shape.elementPantTypes
      .map((_, idx) => `a${idx + 1}`)
      .join(" ");
    for (let pos = 0; pos < shape.elementPantTypes.length; pos++) {
      lines.push(
        `all ${params} | (${ctorRuleName} ${argList}).${pos + 1} = a${pos + 1}.`,
      );
    }
  }

  return `${lines.join("\n")}\n`;
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

function canonicalSymbol(sym: ts.Symbol, checker: ts.TypeChecker): ts.Symbol {
  return sym.flags & ts.SymbolFlags.Alias ? checker.getAliasedSymbol(sym) : sym;
}

function isForeignSourceFile(sf: ts.SourceFile): boolean {
  const fileName = sf.fileName.replace(/\\/gu, "/");
  return (
    /\/node_modules\//u.test(fileName) ||
    /\/typescript\/lib\/(?:lib\..*|typescript)\.d\.ts$/u.test(fileName)
  );
}

function foreignSymbolKey(
  rawSymbol: ts.Symbol,
  checker: ts.TypeChecker,
): { key: string; preferredName: string } | null {
  const symbol = canonicalSymbol(rawSymbol, checker);
  const decls = symbol.declarations;
  if (!decls || decls.length === 0) {
    return null;
  }
  if (!decls.every((decl) => isForeignSourceFile(decl.getSourceFile()))) {
    return null;
  }
  const sources = decls
    .map((decl) => `${decl.getSourceFile().fileName}:${decl.pos}:${decl.end}`)
    .sort()
    .join("|");
  return {
    key: `${symbol.getName()}@${sources}`,
    preferredName: symbol.getName(),
  };
}

export function foreignDomainForType(
  type: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell: SynthCell | undefined,
): MapTsTypeResult | null {
  if (synthCell === undefined) {
    return null;
  }
  const symbol = type.aliasSymbol ?? type.symbol;
  if (symbol === undefined) {
    return null;
  }
  const canonical = canonicalSymbol(symbol, checker);
  if (
    canonical.declarations?.every((decl) =>
      /\/typescript\/lib\/lib\..*\.d\.ts$/u.test(
        decl.getSourceFile().fileName.replace(/\\/gu, "/"),
      ),
    )
  ) {
    return null;
  }
  if (foreignSymbolKey(symbol, checker) === null) {
    return null;
  }
  return mapTsType(type, checker, strategy, synthCell);
}

export const RealStrategy: NumericStrategy = {
  mapNumber() {
    return "Real";
  },
};

/**
 * Map a TypeScript type to a Pantagruel type string.
 *
 * When `synthCell` is provided, synthesized shapes in any type position
 * (Map value, array/tuple element, anonymous-record field, discriminated-
 * union variant field, or nested union member) are registered through that
 * shared cell and replaced with the synthesized domain name. Without
 * `synthCell`, Map types fall through to the caller's fallback (typically
 * `checker.typeToString()` which yields unparseable output). The cell-less
 * behavior is preserved for Stage A: interface-field Maps are handled
 * specially in `translateTypes` and must not be synthesized.
 */
export function mapTsType(
  type: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell?: SynthCell,
): MapTsTypeResult {
  const flags = type.flags;

  // `any` / `unknown` are dynamic tops in TypeScript. Pantagruel is
  // monomorphic, so they lower to one shared uninterpreted Opaque sort.
  if (flags & ts.TypeFlags.Any || flags & ts.TypeFlags.Unknown) {
    if (synthCell) {
      cellRegisterOpaqueDomain(synthCell);
    }
    return okSort(OPAQUE_DOMAIN);
  }

  if (flags & ts.TypeFlags.String || flags & ts.TypeFlags.StringLiteral) {
    return okSort("String");
  }
  if (flags & ts.TypeFlags.Number || flags & ts.TypeFlags.NumberLiteral) {
    return okSort(strategy.mapNumber());
  }
  if (flags & ts.TypeFlags.Boolean || flags & ts.TypeFlags.BooleanLiteral) {
    return okSort("Bool");
  }
  // Top-level `null` / `undefined` / `void` have no Pantagruel encoding.
  // Fall through to the generic `checker.typeToString` fallback below so
  // the unsupported string reflects the source rather than an internal
  // sentinel. Null handling for unions happens in the union branch.

  const iteratorElem = getBuiltinIteratorElementType(type, checker);
  if (iteratorElem !== null) {
    const elem = mapTsType(iteratorElem, checker, strategy, synthCell);
    if (!elem.ok) {
      return elem;
    }
    return okSort(`[${elem.sort}]`);
  }

  // Tuple (check before array since tuples are also type references)
  if (checker.isTupleType(type)) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    const elements: string[] = [];
    for (const t of typeArgs) {
      const elem = mapTsType(t, checker, strategy, synthCell);
      if (!elem.ok) {
        return elem;
      }
      elements.push(elem.sort);
    }
    return okSort(elements.join(" * "));
  }

  // Array
  if (checker.isArrayType(type)) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    if (typeArgs.length === 1) {
      const elem = mapTsType(typeArgs[0]!, checker, strategy, synthCell);
      if (!elem.ok) {
        return elem;
      }
      return okSort(`[${elem.sort}]`);
    }
    return okSort(checker.typeToString(type));
  }

  // Set — modeled as a list. Pantagruel lists already encode membership
  // (smt_types.ml: Array elem_sort Bool), so `s.has(x)` can become `x in s`
  // with list semantics. Uniqueness is not tracked as a logical invariant.
  if (isSetType(type)) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    if (typeArgs.length === 1) {
      const elem = mapTsType(typeArgs[0]!, checker, strategy, synthCell);
      if (!elem.ok) {
        return elem;
      }
      return okSort(`[${elem.sort}]`);
    }
    return okSort(checker.typeToString(type));
  }

  // Map — synthesize a domain when a synthesizer cell is provided. If the
  // K or V type is unmangleable (e.g., contains an unsupported TS type),
  // `cellRegisterMap` returns null and we fall through to checker.typeToString
  // — the same unsupported-type fallback used by the array and set branches.
  if (isMapType(type) && synthCell) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    if (typeArgs.length === 2) {
      const kType = mapTsType(typeArgs[0]!, checker, strategy, synthCell);
      // Propagate the refusal rather than registering a Map keyed by or
      // valued in a non-sort — the failure can no longer reach a domain name.
      if (!kType.ok) {
        return kType;
      }
      const vType = mapTsType(typeArgs[1]!, checker, strategy, synthCell);
      if (!vType.ok) {
        return vType;
      }
      const domain = cellRegisterMap(synthCell, kType.sort, vType.sort);
      if (domain !== null) {
        return okSort(domain);
      }
    }
  }

  // Effect Brand<T> intersections refine a value without changing its
  // base representation. Strip Brand.Brand<...> constituents before
  // mapping so precondition recovery does not alter parameter types.
  if (type.isIntersection()) {
    const nonBrandParts = type.types.filter(
      (part) => effectBrandTypeArgument(part) === undefined,
    );
    if (nonBrandParts.length > 0 && nonBrandParts.length < type.types.length) {
      if (nonBrandParts.length === 1) {
        return mapTsType(nonBrandParts[0]!, checker, strategy, synthCell);
      }
      const parts: string[] = [];
      for (const part of nonBrandParts) {
        const mapped = mapTsType(part, checker, strategy, synthCell);
        if (!mapped.ok) {
          return mapped;
        }
        parts.push(mapped.sort);
      }
      return okSort(parts.filter((v, i, a) => a.indexOf(v) === i).join(" + "));
    }
  }

  // Union
  if (type.isUnion()) {
    // Boolean is represented as true | false union
    if (type.types.every((t) => t.flags & ts.TypeFlags.BooleanLiteral)) {
      return okSort("Bool");
    }
    if (synthCell) {
      const detection = detectDiscriminatedUnion(type, checker);
      if (detection) {
        const domain = registerDiscriminatedUnionDomain(
          synthCell,
          type,
          detection,
          checker,
          strategy,
        );
        if (domain !== null) {
          return okSort(domain);
        }
        return unsupportedType(
          UNSUPPORTED_DISCRIMINATED_UNION_REGISTRATION_REASON,
        );
      }
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
      return okSort(checker.typeToString(type));
    }
    const parts: string[] = [];
    for (const t of nonNullTypes) {
      const mapped = mapTsType(t, checker, strategy, synthCell);
      if (!mapped.ok) {
        return mapped;
      }
      parts.push(mapped.sort);
    }
    const unique = parts.filter((v, i, a) => a.indexOf(v) === i);
    if (hasNullish) {
      return okSort(`[${unique.join(" + ")}]`);
    }
    return okSort(unique.join(" + "));
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
    // Same recursion guard as the discriminated-union branch: a field whose
    // type is the record currently being registered would recurse forever.
    if (synthCell && !synthCell.visiting.has(type)) {
      synthCell.visiting.add(type);
      try {
        return registerAnonymousRecord(type, checker, strategy, synthCell);
      } finally {
        synthCell.visiting.delete(type);
      }
    }
    // No synth cell, or a recursive cycle. `UNSUPPORTED_ANONYMOUS_RECORD` is
    // carried as the failure reason (not as a sort) so consumers that need to
    // distinguish this case from `unknown` (e.g. `translateRecordReturn`) can
    // still match on `reason`.
    return unsupportedType(UNSUPPORTED_ANONYMOUS_RECORD);
  }

  // Named type (interface, class, enum, type alias)
  const symbol = type.aliasSymbol ?? type.symbol;
  if (symbol) {
    const foreign = foreignSymbolKey(symbol, checker);
    if (foreign) {
      if (synthCell) {
        const registered = registerForeignDomain(
          synthCell.foreignDomainSynth,
          synthCell.registry,
          foreign.key,
          foreign.preferredName,
        );
        synthCell.foreignDomainSynth = registered.synth;
        synthCell.registry = registered.registry;
        return okSort(registered.domain);
      }
      return unsupportedType(
        `foreign type '${foreign.preferredName}' is not analyzed and requires a synth cell for uninterpreted-domain emission`,
      );
    }
    return okSort(symbol.getName());
  }

  return okSort(checker.typeToString(type));
}

function isDynamicTypeNode(node: ts.TypeNode): boolean {
  return (
    node.kind === ts.SyntaxKind.AnyKeyword ||
    node.kind === ts.SyntaxKind.UnknownKeyword
  );
}

function isNullishTypeNode(node: ts.TypeNode): boolean {
  return (
    node.kind === ts.SyntaxKind.UndefinedKeyword ||
    node.kind === ts.SyntaxKind.VoidKeyword ||
    (ts.isLiteralTypeNode(node) &&
      node.literal.kind === ts.SyntaxKind.NullKeyword)
  );
}

function mapDynamicTypeNode(synthCell: SynthCell | undefined): MapTsTypeResult {
  if (synthCell) {
    cellRegisterOpaqueDomain(synthCell);
  }
  return okSort(OPAQUE_DOMAIN);
}

function mapTsTypeNodePart(
  node: ts.TypeNode,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell?: SynthCell,
): MapTsTypeResult {
  if (isDynamicTypeNode(node)) {
    return mapDynamicTypeNode(synthCell);
  }
  return mapTsType(
    checker.getTypeFromTypeNode(node),
    checker,
    strategy,
    synthCell,
  );
}

/**
 * Map an explicitly declared TS type node when the syntactic union shape
 * carries precision the checker erases. In particular, TypeScript reduces
 * `unknown | null` and `number | unknown` to plain `unknown`; preserve the
 * declared composite positions as `[Opaque]` and `Int + Opaque` rather than
 * collapsing them to `Opaque`.
 */
export function mapTsTypeFromTypeNode(
  node: ts.TypeNode,
  fallbackType: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell?: SynthCell,
): MapTsTypeResult {
  if (!ts.isUnionTypeNode(node) || !node.types.some(isDynamicTypeNode)) {
    return mapTsType(fallbackType, checker, strategy, synthCell);
  }

  const hasNullish = node.types.some(isNullishTypeNode);
  const nonNullTypes = node.types.filter((part) => !isNullishTypeNode(part));
  if (nonNullTypes.length === 0) {
    return okSort(checker.typeToString(fallbackType));
  }

  const parts: string[] = [];
  for (const part of nonNullTypes) {
    const mapped = mapTsTypeNodePart(part, checker, strategy, synthCell);
    if (!mapped.ok) {
      return mapped;
    }
    parts.push(mapped.sort);
  }

  const unique = parts.filter((v, i, a) => a.indexOf(v) === i);
  if (hasNullish) {
    return okSort(`[${unique.join(" + ")}]`);
  }
  return okSort(unique.join(" + "));
}

function getBuiltinIteratorElementType(
  type: ts.Type,
  checker: ts.TypeChecker,
): ts.Type | null {
  const symbolName = type.getSymbol()?.getName();
  if (
    symbolName !== "MapIterator" &&
    symbolName !== "ArrayIterator" &&
    symbolName !== "SetIterator" &&
    symbolName !== "IterableIterator" &&
    symbolName !== "IteratorObject"
  ) {
    return null;
  }
  const args = checker.getTypeArguments(type as ts.TypeReference);
  return args.length === 1 ? args[0]! : null;
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
): MapTsTypeResult {
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
      return unsupportedType(UNSUPPORTED_ANONYMOUS_RECORD);
    }
    const mapped = mapTsType(propType, checker, strategy, synthCell);
    // Propagate the field's refusal reason unchanged (unknown, nested
    // anonymous-record, or recursive discriminated union) so the cause is
    // not masked as a generic anonymous-record failure.
    if (!mapped.ok) {
      return mapped;
    }
    fields.push({ name: prop.getName(), type: mapped.sort });
  }
  // Canonical order: sort alphabetically by field name.
  fields.sort((a, b) => a.name.localeCompare(b.name));
  const domain = cellRegisterRecord(synthCell, fields);
  return domain !== null
    ? okSort(domain)
    : unsupportedType(UNSUPPORTED_ANONYMOUS_RECORD);
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
          if (!kType.ok) {
            decls.push({
              kind: "unsupported",
              reason: `${iface.name}.${prop.name}: ${kType.reason}`,
            });
            continue;
          }
          if (!vType.ok) {
            decls.push({
              kind: "unsupported",
              reason: `${iface.name}.${prop.name}: ${vType.reason}`,
            });
            continue;
          }
          const kName = synthCell ? cellRegisterName(synthCell, "k") : "k";
          const keyPredName = `${ruleName}-key`;
          decls.push({
            kind: "rule",
            name: keyPredName,
            params: [
              { name: pName, type: iface.name },
              { name: kName, type: kType.sort },
            ],
            returnType: "Bool",
          });
          const ast = getAst();
          decls.push({
            kind: "rule",
            name: ruleName,
            params: [
              { name: pName, type: iface.name },
              { name: kName, type: kType.sort },
            ],
            returnType: vType.sort,
            guard: ast.app(ast.var(keyPredName), [
              ast.var(pName),
              ast.var(kName),
            ]),
          });
          continue;
        }
      }
      const fieldType = mapTsType(prop.type, checker, strategy, synthCell);
      if (!fieldType.ok) {
        decls.push({
          kind: "unsupported",
          reason: `${iface.name}.${prop.name}: ${fieldType.reason}`,
        });
        continue;
      }
      decls.push({
        kind: "rule",
        name: ruleName,
        params: [{ name: pName, type: iface.name }],
        returnType: fieldType.sort,
      });
    }
  }

  for (const alias of extracted.aliases) {
    const aliasType = mapTsType(alias.type, checker, strategy, synthCell);
    if (!aliasType.ok) {
      decls.push({
        kind: "unsupported",
        reason: `alias ${alias.name}: ${aliasType.reason}`,
      });
      continue;
    }
    decls.push({
      kind: "alias",
      name: alias.name,
      type: aliasType.sort,
    });
  }

  for (const enumDecl of extracted.enums) {
    decls.push({ kind: "domain", name: enumDecl.name });
  }

  return decls;
}
