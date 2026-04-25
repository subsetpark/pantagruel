import type { SourceFile } from "ts-morph";
import ts from "typescript";
import { type IRExpr, irLet, irWrap } from "./ir.js";
import { buildIR, isBuildUnsupported } from "./ir-build.js";
import { lowerExpr } from "./ir-emit.js";
import { type IR1Expr, ir1FromL2 } from "./ir1.js";
import {
  buildL1Conditional,
  buildL1ConditionalFromArms,
  isL1ConditionalForm,
  isL1Unsupported,
  lowerL1ToOpaque,
} from "./ir1-build.js";
import type {
  OpaqueCombiner,
  OpaqueExpr,
  OpaqueGuard,
  OpaqueParam,
} from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import { isKnownPureCall } from "./purity.js";
import { translateRecordReturn } from "./translate-record.js";
import {
  classifyFunction,
  findFunction,
  isAssertionCall,
  isFollowableGuardCall,
  isPureExpression,
  shortParamName,
  translateExpr,
  translateOperator,
} from "./translate-signature.js";
import {
  cellRegisterMap,
  cellRegisterName,
  fieldRuleName,
  isMapType,
  isSetType,
  lookupMapKV,
  mapTsType,
  type NumericStrategy,
  resolveFieldOwner,
  type SynthCell,
  toPantTermName,
} from "./translate-types.js";
import type { PantDeclaration, PropResult } from "./types.js";

/**
 * Read the `TS2PANT_USE_IR` env var safely from globalThis without
 * pulling in @types/node into the tsconfig. The flag routes the
 * pure-path return-expression translation through the IR pipeline
 * (Stage 1 — see CLAUDE.md §"Intermediate Representation").
 */
function useIRPipeline(): boolean {
  const g = globalThis as { process?: { env?: Record<string, string> } };
  return g.process?.env?.["TS2PANT_USE_IR"] === "1";
}

/**
 * True if the imperative-IR (Layer 1) conditional pipeline is enabled
 * via `TS2PANT_USE_L1=1`. M1 patch 2 uses this flag to gate the L1
 * conditional builder during validation; patch 3 deletes the flag and
 * makes L1 always-on for conditional value forms (workstream
 * `workstreams/ts2pant-imperative-ir.md`, milestone M1).
 */
function useL1Pipeline(): boolean {
  const g = globalThis as { process?: { env?: Record<string, string> } };
  return g.process?.env?.["TS2PANT_USE_L1"] === "1";
}

// --- Const-binding inlining infrastructure (let-elimination) ---

/**
 * Per-body translation context threaded through every `translateBodyExpr`
 * call. Carries the hygienic-binder counter and (optionally) the
 * module-wide `SynthCell` so `.get`/`.has` on non-field Map receivers
 * can resolve to the synthesized rule names.
 *
 * Mutable 2-field record: `n` is reassigned in place by `nextSupply`. This
 * is within ts2pant's self-translation envelope (cell-field reassignment
 * translates to primed rules on the cell), unlike the prior closure over a
 * `let counter = 0`.
 */
export interface UniqueSupply {
  n: number;
  synthCell?: SynthCell | undefined;
}
function makeUniqueSupply(synthCell?: SynthCell): UniqueSupply {
  return { n: 0, synthCell };
}

function nextSupply(supply: UniqueSupply): number {
  const value = supply.n;
  supply.n = value + 1;
  return value;
}

export function freshHygienicBinder(supply: UniqueSupply): string {
  return `$${nextSupply(supply)}`;
}

/**
 * True when a TypeScript type includes `null`, `undefined`, or `void` —
 * i.e., when `mapTsType` will list-lift it to `[T]`. Used at `??` and `?.`
 * sites to decide whether the receiver needs the cardinality-based lowering
 * (nullable) or can pass through as-is (already concrete).
 */
export function isNullableTsType(type: ts.Type): boolean {
  const mask = ts.TypeFlags.Null | ts.TypeFlags.Undefined | ts.TypeFlags.Void;
  if (type.isUnion()) {
    return type.types.some((t) => (t.flags & mask) !== 0);
  }
  return (type.flags & mask) !== 0;
}

/**
 * Recognized prelude binding. The pure-body extractor consumes a sequence of
 * these before the final return statement. Two shapes:
 *
 *   - `const`: ordinary `const x = e;` (let-elimination).
 *   - `muSearch`: the `let counter = init; while (P(counter)) counter++;`
 *     pair, recognized as Kleene μ-minimization and emitted as a synthetic
 *     binding whose value is `min over each $j: Nat, $j >= init, ~P($j) | $j`.
 *     See `recognizeMuSearch` and `emitMuSearch`.
 */
type ConstBinding =
  | { kind: "const"; tsName: string; initializer: ts.Expression }
  | { kind: "muSearch"; tsName: string; mu: MuSearch }
  | {
      kind: "earlyReturn";
      predicateExpr: ts.Expression;
      valueExpr: ts.Expression;
    };

/** Names a binding introduces into scope (none for an early-return arm). */
function bindingNames(b: ConstBinding): readonly string[] {
  return b.kind === "earlyReturn" ? [] : [b.tsName];
}

interface MuSearch {
  counterName: string;
  initTsExpr: ts.Expression;
  predicateTsExpr: ts.Expression;
}

/**
 * Result of translating a body expression. Either an opaque expression
 * (possibly with a deferred list-comprehension structure for chain fusion),
 * or a failure.
 *
 * When `pendingComprehension` is set, `expr` holds the *projection body*
 * (e.g., `name u`) and the field carries the binder, root array, and
 * accumulated filter predicates. Materialization (calling `bodyExpr`) emits
 * the flat `each([], [gIn(binder, arrExpr), ...guards], expr)`.
 *
 * Deforestation (Wadler, TCS 1990) — chained `.filter`/`.map`/`.reduce` fuse
 * into a single traversal by deferring materialization until a consumer
 * outside the chain demands an opaque expression.
 */
interface PendingComprehension {
  binder: string;
  arrExpr: OpaqueExpr;
  guards: OpaqueGuard[];
}

/**
 * A Map mutation effect produced by `.set(k, v)` or `.delete(k)` on a
 * Map-typed receiver. The effect is not an expression — it can only appear
 * as a statement (ExpressionStatement) and is consumed by `symbolicExecute`
 * which installs a MapRuleWriteEntry in the symbolic state. Encountering an
 * effect in expression position is a translation failure.
 */
interface MapMutation {
  op: "set" | "delete";
  ruleName: string;
  keyPredName: string;
  ownerType: string;
  keyType: string;
  objExpr: OpaqueExpr;
  keyExpr: OpaqueExpr;
  valueExpr: OpaqueExpr | null;
}

/**
 * A Set mutation effect produced by `.add(x)`, `.delete(x)`, or `.clear()`
 * on a Set-typed receiver that resolves to a declared interface field
 * (Stage A only; parameter-level Set mutation is rejected upstream). Like
 * MapMutation, it is a statement-position effect consumed by
 * `symbolicExecute` which installs a SetRuleWriteEntry in the symbolic
 * state.
 *
 * Sets-as-lists: encoded as `[T]` list-valued field accessor (one arity-1
 * rule, unlike Map's value+membership rule pair). `elemExpr` is null for
 * `.clear()`.
 */
interface SetMutation {
  op: "add" | "delete" | "clear";
  ruleName: string;
  ownerType: string;
  elemType: string;
  objExpr: OpaqueExpr;
  elemExpr: OpaqueExpr | null;
}

type CollectionMutation = MapMutation | SetMutation;

type BodyResult =
  | { unsupported: string }
  | { expr: OpaqueExpr; pendingComprehension?: PendingComprehension }
  | { effect: CollectionMutation };

/** Type guard for unsupported BodyResult. */
export function isBodyUnsupported(r: BodyResult): r is { unsupported: string } {
  return "unsupported" in r;
}

/** Type guard for effect BodyResult. */
export function isBodyEffect(
  r: BodyResult,
): r is { effect: CollectionMutation } {
  return "effect" in r;
}

/** Discriminator: Map effects carry a `keyPredName`; Set effects don't. */
function isMapEffect(e: CollectionMutation): e is MapMutation {
  return "keyPredName" in e;
}

/**
 * Turn an `effect` result into an `unsupported` marker. Expression-position
 * consumers of `translateBodyExpr` use this after the unsupported check so
 * the remaining value narrows to the `{ expr, pendingComprehension? }` form.
 * Only `symbolicExecute`'s ExpressionStatement handler consumes effects
 * directly.
 */
function rejectEffect(
  r: BodyResult,
):
  | { unsupported: string }
  | { expr: OpaqueExpr; pendingComprehension?: PendingComprehension } {
  if ("effect" in r) {
    return {
      unsupported: "collection mutation outside statement position",
    };
  }
  return r;
}

/** Extract the OpaqueExpr from a successful BodyResult, materializing any
 * deferred comprehension chain into a flat `each` at the boundary. */
export function bodyExpr(r: BodyResult): OpaqueExpr {
  if ("unsupported" in r) {
    throw new Error(`bodyExpr called on unsupported: ${r.unsupported}`);
  }
  if ("effect" in r) {
    throw new Error("bodyExpr called on effect result");
  }
  if (r.pendingComprehension) {
    const ast = getAst();
    const { binder, arrExpr, guards } = r.pendingComprehension;
    return ast.each([], [ast.gIn(binder, arrExpr), ...guards], r.expr);
  }
  return r.expr;
}

// --- Symbolic last-write state (Dijkstra's guarded commands, 1975) ---
//
// Forward symbolic execution with path merging. Each property-assignment
// statement updates `writes[prop::objRepr]`. If statements clone the state
// for each branch and merge via `cond` at the join. Later reads of the same
// property access see the accumulated value. See CLAUDE.md § Guarded Commands.

interface PropertyWriteEntry {
  kind: "property";
  prop: string;
  objExpr: OpaqueExpr;
  value: OpaqueExpr;
}

// Accumulated point-updates to a Map-backed rule pair (value rule + membership
// predicate). One entry per distinct rule name coalesces all `.set`/`.delete`
// writes to the same receiver's underlying rule: a function writing
// `m.set(k1, v1); m.set(k2, v2)` produces one MapRuleWriteEntry with two
// valueOverrides and two membershipOverrides. Emission uses Pantagruel's
// N-ary override `R[(m, k) |-> v]` (samples/06-advanced.pant).

/**
 * One point-update within a rule override. `keyTuple = (objExpr, keyExpr)`
 * is the override LHS the emitter hands to `ast.override`; `objExpr` and
 * `keyExpr` are kept around so the merge fallback can build the pre-state
 * expression `R objExpr keyExpr` (pretty) instead of projecting off the
 * tuple (ugly but equivalent).
 */
interface MapOverride {
  keyTuple: OpaqueExpr;
  objExpr: OpaqueExpr;
  keyExpr: OpaqueExpr;
  value: OpaqueExpr;
}

interface MapRuleWriteEntry {
  kind: "map";
  ruleName: string;
  keyPredName: string;
  ownerType: string;
  keyType: string;
  valueOverrides: MapOverride[];
  membershipOverrides: MapOverride[];
}

/**
 * One point-update within a Set membership override. `elemExpr` is the
 * element being added or removed; `value` is `ast.litBool(true)` for
 * `.add`, `ast.litBool(false)` for `.delete`, or a `cond` after branch
 * merge. Parallel to `MapOverride` but scalar-keyed rather than tuple-
 * keyed — Sets have no separate key-and-value axis.
 */
interface SetOverride {
  elemExpr: OpaqueExpr;
  value: OpaqueExpr;
}

/**
 * Accumulated point-updates to a Set-backed field-accessor rule. One entry
 * per distinct `(ruleName, ownerType, elemType, canonical-receiver)` in
 * `state.writes`. Unlike MapRuleWriteEntry, only one override list (the
 * field-accessor rule IS the membership predicate at the list-semantics
 * level — `s.has(x)` is `x in s`).
 *
 * `cleared` records that a `.clear()` reset the accumulated overrides to
 * the empty list. After a clear, the emission drops the pre-state
 * `y in tags c` fallthrough and uses literal `false` instead — anything
 * that isn't in `memberOverrides` after a clear is gone.
 *
 * `objExpr` is the canonicalized receiver; the emission quantifier runs
 * only over the element (type `elemType`), while the LHS applies the
 * primed rule to this receiver as a free term.
 */
interface SetRuleWriteEntry {
  kind: "set";
  ruleName: string;
  ownerType: string;
  elemType: string;
  objExpr: OpaqueExpr;
  memberOverrides: SetOverride[];
  cleared: boolean;
}

type WriteEntry = PropertyWriteEntry | MapRuleWriteEntry | SetRuleWriteEntry;

/**
 * Per-body symbolic-execution accumulator. Held as a cell of immutable
 * records: `writes`, `writtenKeys`, `modifiedProps` are typed as read-only
 * views, and updates replace the whole map/set via `putWrite`,
 * `addWrittenKey`, `addModifiedProp`. Cell-field reassignment is within
 * ts2pant's self-translation envelope (translatable as primed rules on the
 * cell), whereas the prior `.set`/`.add` in-place mutation was not.
 *
 * `modifiedProps` is *shared* across cloned cells — it tracks which rules
 * have been modified anywhere in this execution, including Shape A loop
 * writes whose per-element equation is emitted directly (bypassing
 * `writes`). Consumed by the frame-condition generator so loop-modified
 * rules don't get a spurious identity frame.
 *
 * `canonicalize` applies the ambient const-binding substitution to an
 * expression before it is used as a state key. Writes store keys under the
 * post-substitution form so `const x = a; x.balance = 1` and a later
 * `x.balance` read resolve to the same key. Updated by `symbolicExecute`
 * whenever a new const binding is inlined so the in-flight `applyConst`
 * stays in sync with the state.
 */
export interface SymbolicState {
  writes: ReadonlyMap<string, WriteEntry>;
  // Keys *written during the current branch* (reset on clone). Used by the
  // if-merge algorithm to determine which locations are "touched."
  writtenKeys: ReadonlySet<string>;
  modifiedProps: Set<string>;
  canonicalize: (e: OpaqueExpr) => OpaqueExpr;
}

function makeSymbolicState(
  canonicalize: (e: OpaqueExpr) => OpaqueExpr = (e) => e,
): SymbolicState {
  return {
    writes: new Map(),
    writtenKeys: new Set(),
    modifiedProps: new Set(),
    canonicalize,
  };
}

function cloneSymbolicState(s: SymbolicState): SymbolicState {
  return {
    writes: new Map(s.writes),
    writtenKeys: new Set(),
    modifiedProps: s.modifiedProps,
    canonicalize: s.canonicalize,
  };
}

function putWrite(state: SymbolicState, key: string, entry: WriteEntry): void {
  const next = new Map(state.writes);
  next.set(key, entry);
  state.writes = next;
}

function addWrittenKey(state: SymbolicState, key: string): void {
  const next = new Set(state.writtenKeys);
  next.add(key);
  state.writtenKeys = next;
}

function addModifiedProp(state: SymbolicState, prop: string): void {
  state.modifiedProps.add(prop);
}

function setCanonicalize(
  state: SymbolicState,
  fn: (e: OpaqueExpr) => OpaqueExpr,
): void {
  state.canonicalize = fn;
}

function symbolicKey(prop: string, objExpr: OpaqueExpr): string {
  return `${prop}::${getAst().strExpr(objExpr)}`;
}

/**
 * State-map key for Map writes. Writes coalesce into one MapRuleWriteEntry
 * so multiple `.set`/`.delete` calls to the same rule accumulate as distinct
 * `(tuple |-> value)` override pairs (collapsed into one override expression
 * at emission). Keying by just `ruleName` is too coarse for Stage A, where
 * two different interfaces can share a field name (e.g., `A.cache` and
 * `B.cache`) — emission quantifies one `ownerType`/`keyType` per entry, so
 * crossed-field writes would be emitted against the first writer's types.
 * Include the full rule identity (rule, key predicate, owner, key) so only
 * writes that share a target rule coalesce.
 */
function mapWriteKey(
  ruleName: string,
  keyPredName: string,
  ownerType: string,
  keyType: string,
): string {
  return `map::${ruleName}::${keyPredName}::${ownerType}::${keyType}`;
}

/**
 * Collapse two arrays of overrides (from the two branches of an if, or
 * a branch vs. identity fallback) into a single array, keyed by canonical
 * position form. Per canonical key, the per-branch values are combined via
 * `combine`. Overrides that only exist in one side use the caller-supplied
 * `fallback` expression (pre-state `R m k` for value; pre-state `Rkey m k`
 * for membership; pre-state `y in s` for set membership) for the missing
 * side.
 *
 * Order is preserved from `aSide` first, then any `bSide` keys not already
 * seen. The canonical form comes from `ast.strExpr(keyOf(o))`. Generic over
 * the override record so Map (tuple-keyed) and Set (element-keyed) writes
 * share the same merge plumbing.
 */
function mergeOverrides<O extends { value: OpaqueExpr }>(
  aSide: ReadonlyArray<O>,
  bSide: ReadonlyArray<O>,
  keyOf: (o: O) => OpaqueExpr,
  fallback: (o: O) => OpaqueExpr,
  combine: (vA: OpaqueExpr, vB: OpaqueExpr) => OpaqueExpr,
): O[] {
  const ast = getAst();
  const canonical = (t: OpaqueExpr) => ast.strExpr(t);
  const bByKey = new Map<string, OpaqueExpr>();
  for (const o of bSide) {
    bByKey.set(canonical(keyOf(o)), o.value);
  }
  const seen = new Set<string>();
  const out: O[] = [];
  for (const o of aSide) {
    const k = canonical(keyOf(o));
    seen.add(k);
    const vB = bByKey.get(k) ?? fallback(o);
    out.push({ ...o, value: combine(o.value, vB) });
  }
  for (const o of bSide) {
    const k = canonical(keyOf(o));
    if (seen.has(k)) {
      continue;
    }
    const vA = fallback(o);
    out.push({ ...o, value: combine(vA, o.value) });
  }
  return out;
}

/**
 * Install a `.set`/`.delete` effect into the symbolic state. Creates a new
 * MapRuleWriteEntry on first use per rule; subsequent effects on the same
 * rule append their (tuple |-> value) overrides. The key tuple is `(m, k)`
 * — `m` is the receiver (so distinct receivers at the same key don't
 * collide), `k` is the key argument.
 */
function installMapWrite(
  state: SymbolicState,
  effect: MapMutation,
  applyConst: (e: OpaqueExpr) => OpaqueExpr,
): void {
  const ast = getAst();
  const key = mapWriteKey(
    effect.ruleName,
    effect.keyPredName,
    effect.ownerType,
    effect.keyType,
  );
  const objExpr = applyConst(effect.objExpr);
  const keyExpr = applyConst(effect.keyExpr);
  const keyTuple = ast.tuple([objExpr, keyExpr]);
  const tupleKeyText = ast.strExpr(keyTuple);
  const isSameTuple = (o: MapOverride) =>
    ast.strExpr(o.keyTuple) === tupleKeyText;

  // cloneSymbolicState only shallow-copies `writes`, so the MapRuleWriteEntry
  // object is shared with sibling branches and the outer state. Copy the
  // entry (including its arrays) before mutating so branch-local writes
  // don't leak.
  const prev = state.writes.get(key);
  const entry: MapRuleWriteEntry =
    prev !== undefined && prev.kind === "map"
      ? {
          ...prev,
          valueOverrides: [...prev.valueOverrides],
          membershipOverrides: [...prev.membershipOverrides],
        }
      : {
          kind: "map",
          ruleName: effect.ruleName,
          keyPredName: effect.keyPredName,
          ownerType: effect.ownerType,
          keyType: effect.keyType,
          valueOverrides: [],
          membershipOverrides: [],
        };

  // Pantagruel override semantics are first-pair-wins, but for a sequence
  // `m.set(k, 1); m.set(k, 2)` inside one path the later write must win.
  // Drop any prior override for the same (obj, key) tuple on both sides
  // before appending the new one.
  entry.valueOverrides = entry.valueOverrides.filter((o) => !isSameTuple(o));
  entry.membershipOverrides = entry.membershipOverrides.filter(
    (o) => !isSameTuple(o),
  );

  if (effect.op === "set") {
    const valueExpr = applyConst(effect.valueExpr!);
    entry.valueOverrides = [
      ...entry.valueOverrides,
      { keyTuple, objExpr, keyExpr, value: valueExpr },
    ];
    entry.membershipOverrides = [
      ...entry.membershipOverrides,
      { keyTuple, objExpr, keyExpr, value: ast.litBool(true) },
    ];
  } else {
    // delete: membership goes false; value rule need not be restated because
    // the declaration guard makes the value-rule body vacuous under false
    // membership.
    entry.membershipOverrides = [
      ...entry.membershipOverrides,
      { keyTuple, objExpr, keyExpr, value: ast.litBool(false) },
    ];
  }
  putWrite(state, key, entry);
  addWrittenKey(state, key);
  state.modifiedProps.add(effect.ruleName);
  state.modifiedProps.add(effect.keyPredName);
}

/**
 * Build a `.get`/`.has` read expression that threads staged writes. When a
 * prior `.set`/`.delete` on the same rule has been installed in the symbolic
 * state, the returned read applies Pantagruel's N-ary override inline —
 * `R[(m, k) |-> v](obj, key)` — so a `.set(k, v)` followed by `.get(k)` or
 * `.has(k)` in the same body observes the just-written value. The ite
 * expansion matches McCarthy's select/store (Kroening & Strichman Ch. 7):
 * at the override key the override fires, everywhere else it falls through
 * to the pre-state rule application. Without a staged entry, returns the
 * plain pre-state rule application.
 *
 * `.get` respects staged `.delete`s: a value override whose tuple has a
 * literal `false` as its latest membership claim is dropped from the read,
 * so `m.set(k, v); m.delete(k); m.get(k)` falls through to the pre-state
 * rule rather than returning `v`. Pantagruel's declaration guard handles
 * this at emission time (vacuous value under false membership — Dafny-style
 * partial function), but an inline override application has no such guard,
 * so the filter is explicit at the read site.
 */
function readMapThroughWrites(
  state: SymbolicState | undefined,
  methodName: "get" | "has",
  ruleName: string,
  keyPredName: string,
  ownerType: string,
  keyType: string,
  objExpr: OpaqueExpr,
  keyExpr: OpaqueExpr,
): OpaqueExpr {
  const ast = getAst();
  const appliedRule = methodName === "has" ? keyPredName : ruleName;
  if (state === undefined) {
    return ast.app(ast.var(appliedRule), [objExpr, keyExpr]);
  }
  // Apply the ambient const-binding substitution so the read's receiver/key
  // normalize to the same form the write site stored (installMapWrite passes
  // its tuple components through applyConst). Otherwise a `const x = a;
  // x.cache.set(k, v); x.cache.get(k)` would have `(a, k) |-> v` on the
  // write and `(x, k)` on the read, and the ite in the override expansion
  // wouldn't fire because the const alias has been inlined away and the SMT
  // engine has no remaining equality to recover.
  const canonObj = state.canonicalize(objExpr);
  const canonKey = state.canonicalize(keyExpr);
  const entry = state.writes.get(
    mapWriteKey(ruleName, keyPredName, ownerType, keyType),
  );
  const baseRead = ast.app(ast.var(appliedRule), [canonObj, canonKey]);
  if (entry === undefined || entry.kind !== "map") {
    return baseRead;
  }
  const overrides = readOverridesFor(entry, methodName);
  if (overrides.length === 0) {
    return baseRead;
  }
  return ast.app(
    ast.override(
      appliedRule,
      overrides.map((o) => [o.keyTuple, o.value] as [OpaqueExpr, OpaqueExpr]),
    ),
    [canonObj, canonKey],
  );
}

/**
 * Pick the override list to use for an inline `.get`/`.has` read. `.has`
 * uses the raw membership list. `.get` drops value overrides whose tuple
 * has a literal `false` latest membership — a staged `.delete` supersedes
 * any earlier staged `.set` at that tuple. Conditional membership (e.g.
 * `cond g => false, true => true` after a branch-merged `.delete`) is not
 * filtered; only literal `false` is detected here.
 */
function readOverridesFor(
  entry: MapRuleWriteEntry,
  methodName: "get" | "has",
): MapOverride[] {
  if (methodName === "has") {
    return [...entry.membershipOverrides];
  }
  const ast = getAst();
  const canonical = (t: OpaqueExpr) => ast.strExpr(t);
  const falseText = ast.strExpr(ast.litBool(false));
  const latestMembership = new Map<string, string>();
  for (const m of entry.membershipOverrides) {
    latestMembership.set(canonical(m.keyTuple), ast.strExpr(m.value));
  }
  return entry.valueOverrides.filter(
    (o) => latestMembership.get(canonical(o.keyTuple)) !== falseText,
  );
}

/**
 * State-map key for Set writes. Includes ownerType and elemType to
 * disambiguate two interfaces that happen to share a field name but with
 * different element types (analogous to `mapWriteKey`'s guard against
 * cross-interface collisions). Parallel to `mapWriteKey` at :310.
 */
function setWriteKey(
  ruleName: string,
  ownerType: string,
  elemType: string,
): string {
  return `set::${ruleName}::${ownerType}::${elemType}`;
}

/**
 * Install a `.add` / `.delete` / `.clear` effect into the symbolic state.
 * Creates a new SetRuleWriteEntry on first use per rule; subsequent
 * effects accumulate overrides on the same entry.
 *
 * Later-wins per element: a later `.delete(x)` drops any prior `.add(x)`
 * override at the same canonical element form before appending its own
 * `false` override. Mirrors the tuple-keyed later-wins in
 * `installMapWrite`.
 *
 * `.clear()` resets `memberOverrides` to the empty list and sets
 * `cleared = true`. Subsequent `.add`/`.delete` calls on the same
 * receiver in the same path accumulate normally on top of the cleared
 * baseline; at emission the pre-state `y in tags c` fallthrough is
 * replaced with literal `false`.
 */
function installSetWrite(
  state: SymbolicState,
  effect: SetMutation,
  applyConst: (e: OpaqueExpr) => OpaqueExpr,
): void {
  const ast = getAst();
  const key = setWriteKey(effect.ruleName, effect.ownerType, effect.elemType);
  const objExpr = applyConst(effect.objExpr);

  const prev = state.writes.get(key);
  const entry: SetRuleWriteEntry =
    prev !== undefined && prev.kind === "set"
      ? { ...prev, memberOverrides: [...prev.memberOverrides] }
      : {
          kind: "set",
          ruleName: effect.ruleName,
          ownerType: effect.ownerType,
          elemType: effect.elemType,
          objExpr,
          memberOverrides: [],
          cleared: false,
        };

  if (effect.op === "clear") {
    entry.memberOverrides = [];
    entry.cleared = true;
    putWrite(state, key, entry);
    addWrittenKey(state, key);
    state.modifiedProps.add(effect.ruleName);
    return;
  }

  const elemExpr = applyConst(effect.elemExpr!);
  const elemText = ast.strExpr(elemExpr);
  const isSameElem = (o: SetOverride) => ast.strExpr(o.elemExpr) === elemText;
  entry.memberOverrides = entry.memberOverrides.filter((o) => !isSameElem(o));
  entry.memberOverrides.push({
    elemExpr,
    value: effect.op === "add" ? ast.litBool(true) : ast.litBool(false),
  });
  putWrite(state, key, entry);
  addWrittenKey(state, key);
  state.modifiedProps.add(effect.ruleName);
}

/**
 * Read a Set `.has(x)` through staged writes. Parallel to
 * `readMapThroughWrites`. When a prior `.add`/`.delete`/`.clear` has been
 * installed, the returned expression is a `cond` over per-element equality
 * arms with the pre-state `x in s` lookup as the fallthrough (or literal
 * `false` when `cleared = true`). Without a staged entry, returns the
 * plain pre-state membership `x in s`.
 *
 * Unlike the Map read (which emits `ast.override` — an N-ary rule override
 * applied to the receiver+key), Set membership goes through `ast.binop`
 * with `ast.opIn` and a cond over equality arms. `ast.override` would be
 * invalid on a list-valued field accessor.
 */
function readSetThroughWrites(
  state: SymbolicState | undefined,
  ruleName: string,
  ownerType: string,
  elemType: string,
  objExpr: OpaqueExpr,
  queryExpr: OpaqueExpr,
): OpaqueExpr {
  const ast = getAst();
  const baseIn = (obj: OpaqueExpr, q: OpaqueExpr) =>
    ast.binop(ast.opIn(), q, ast.app(ast.var(ruleName), [obj]));
  if (state === undefined) {
    return baseIn(objExpr, queryExpr);
  }
  const canonObj = state.canonicalize(objExpr);
  const canonQuery = state.canonicalize(queryExpr);
  const entry = state.writes.get(setWriteKey(ruleName, ownerType, elemType));
  if (entry === undefined || entry.kind !== "set") {
    return baseIn(canonObj, canonQuery);
  }
  if (entry.memberOverrides.length === 0) {
    return entry.cleared ? ast.litBool(false) : baseIn(canonObj, canonQuery);
  }
  const fallback: [OpaqueExpr, OpaqueExpr] = entry.cleared
    ? [ast.litBool(true), ast.litBool(false)]
    : [ast.litBool(true), baseIn(canonObj, canonQuery)];
  return ast.cond([
    ...entry.memberOverrides.map(
      (o) =>
        [ast.binop(ast.opEq(), canonQuery, o.elemExpr), o.value] as [
          OpaqueExpr,
          OpaqueExpr,
        ],
    ),
    fallback,
  ]);
}

/**
 * Emit quantified equations for a Map rule pair (value rule +
 * membership predicate). Extracted from the previous inline emission so
 * the outer loop can dispatch uniformly across Property / Map / Set
 * entries. `valueOverrides` empty (pure `.delete`) — skip the value
 * rule; the membership going false makes the rule-guard vacuous.
 */
function emitMapEquations(
  entry: MapRuleWriteEntry,
  propositions: PropResult[],
  allocBinder: (hint: string) => string,
  state: SymbolicState,
): void {
  const ast = getAst();
  if (entry.valueOverrides.length > 0) {
    const m1 = allocBinder("m");
    const k1 = allocBinder("k");
    propositions.push({
      kind: "equation",
      quantifiers: [
        ast.param(m1, ast.tName(entry.ownerType)),
        ast.param(k1, ast.tName(entry.keyType)),
      ] as OpaqueParam[],
      lhs: ast.app(ast.primed(entry.ruleName), [ast.var(m1), ast.var(k1)]),
      rhs: ast.app(
        ast.override(
          entry.ruleName,
          entry.valueOverrides.map(
            (o) => [o.keyTuple, o.value] as [OpaqueExpr, OpaqueExpr],
          ),
        ),
        [ast.var(m1), ast.var(k1)],
      ),
    });
    addModifiedProp(state, entry.ruleName);
  }
  if (entry.membershipOverrides.length > 0) {
    const m1 = allocBinder("m");
    const k1 = allocBinder("k");
    propositions.push({
      kind: "equation",
      quantifiers: [
        ast.param(m1, ast.tName(entry.ownerType)),
        ast.param(k1, ast.tName(entry.keyType)),
      ] as OpaqueParam[],
      lhs: ast.app(ast.primed(entry.keyPredName), [ast.var(m1), ast.var(k1)]),
      rhs: ast.app(
        ast.override(
          entry.keyPredName,
          entry.membershipOverrides.map(
            (o) => [o.keyTuple, o.value] as [OpaqueExpr, OpaqueExpr],
          ),
        ),
        [ast.var(m1), ast.var(k1)],
      ),
    });
    addModifiedProp(state, entry.keyPredName);
  }
}

/**
 * Emit the one universally-quantified Bool equation for a Set
 * membership write:
 *   all y: T | y in tags' c <=> cond <per-elem arms>, true => <tail>.
 *
 * The tail is `y in tags c` (pre-state membership) for normal
 * accumulated writes, or literal `false` when a `.clear()` reset the
 * overrides. An empty override list with `cleared = true` collapses the
 * cond to `all y | ~(y in tags' c)` because every arm degenerates.
 *
 * Parallel in role to `emitMapEquations`, but emits one equation (Sets
 * have no separate value-rule / membership-predicate split) and uses
 * `<=>` over `in` rather than `=` over `ast.override`. The receiver is
 * carried on the entry itself; the quantifier runs only over the
 * element.
 */
function emitSetMembershipEquation(
  entry: SetRuleWriteEntry,
  propositions: PropResult[],
  allocBinder: (hint: string) => string,
  state: SymbolicState,
): void {
  const ast = getAst();
  if (entry.memberOverrides.length === 0 && !entry.cleared) {
    return;
  }
  const y = allocBinder("y");
  const yVar = ast.var(y);
  const memberIn = (rule: OpaqueExpr) => ast.binop(ast.opIn(), yVar, rule);
  const primedApp = ast.app(ast.primed(entry.ruleName), [entry.objExpr]);
  const preApp = ast.app(ast.var(entry.ruleName), [entry.objExpr]);
  const fallback: [OpaqueExpr, OpaqueExpr] = entry.cleared
    ? [ast.litBool(true), ast.litBool(false)]
    : [ast.litBool(true), memberIn(preApp)];
  const condArms: [OpaqueExpr, OpaqueExpr][] = entry.memberOverrides.map(
    (o) =>
      [ast.binop(ast.opEq(), yVar, o.elemExpr), o.value] as [
        OpaqueExpr,
        OpaqueExpr,
      ],
  );
  const rhs =
    condArms.length === 0 ? fallback[1] : ast.cond([...condArms, fallback]);
  // Emitted as an assertion (a quantified Bool formula), not an equation:
  // Pantagruel's equation kind is LHS = RHS (`=`), but Set membership
  // writes require `<=>` over `in`, which is structurally a Bool
  // biconditional. Mirrors how the empty-Set initializer in
  // translate-record.ts emits its `~(y in f_i (f <args>))` assertion
  // rather than an equation.
  propositions.push({
    kind: "assertion",
    quantifiers: [ast.param(y, ast.tName(entry.elemType))] as OpaqueParam[],
    body: ast.binop(ast.opIff(), memberIn(primedApp), rhs),
  });
  addModifiedProp(state, entry.ruleName);
}

/**
 * Return a fresh Map equal to `m` plus the binding `k -> v`. Used instead of
 * `m.set(k, v)` so the immutable-record discipline holds: callers either
 * thread the returned map or assign it into a cell field.
 */
function withParam<K, V>(m: ReadonlyMap<K, V>, k: K, v: V): ReadonlyMap<K, V> {
  const next = new Map(m);
  next.set(k, v);
  return next;
}

function isBareReturn(stmt: ts.Statement): boolean {
  if (ts.isReturnStatement(stmt) && !stmt.expression) {
    return true;
  }
  if (ts.isBlock(stmt) && stmt.statements.length === 1) {
    const s = stmt.statements[0]!;
    return ts.isReturnStatement(s) && !s.expression;
  }
  return false;
}

function flattenStmt(stmt: ts.Statement): ts.Statement[] {
  return ts.isBlock(stmt) ? Array.from(stmt.statements) : [stmt];
}

/**
 * Early-exit if-conversion (Allen et al., POPL 1983, extended to early
 * exits). Recognizes three patterns with a bare `return;` on one side:
 *
 *   if (c) { return; }              → early-exit when c; continuation = post-if
 *   if (c) { return; } else { X }   → early-exit when c; continuation = X ++ post-if
 *   if (c) { X } else { return; }   → early-exit when !c; continuation = X ++ post-if
 *
 * The non-returning branch's statements are lifted into the continuation
 * (to be executed together with the statements following the `if`). The
 * flag `earlyExitWhenTrue` indicates whether the if-condition directly
 * represents the early-exit path or needs to be negated at the merge.
 */
interface EarlyExitDetection {
  condition: ts.Expression;
  /** If false, early exit is taken when !condition. */
  earlyExitWhenTrue: boolean;
  continuationPrefix: ts.Statement[];
}

function detectEarlyExit(stmt: ts.Statement): EarlyExitDetection | null {
  if (!ts.isIfStatement(stmt)) {
    return null;
  }
  const thenExits = isBareReturn(stmt.thenStatement);
  const elseExits =
    stmt.elseStatement !== undefined && isBareReturn(stmt.elseStatement);

  if (thenExits && !stmt.elseStatement) {
    return {
      condition: stmt.expression,
      earlyExitWhenTrue: true,
      continuationPrefix: [],
    };
  }
  if (thenExits && stmt.elseStatement && !elseExits) {
    return {
      condition: stmt.expression,
      earlyExitWhenTrue: true,
      continuationPrefix: flattenStmt(stmt.elseStatement),
    };
  }
  if (!thenExits && elseExits) {
    return {
      condition: stmt.expression,
      earlyExitWhenTrue: false,
      continuationPrefix: flattenStmt(stmt.thenStatement),
    };
  }
  return null;
}

/**
 * Map TypeScript compound-assignment tokens (`+=`, `-=`, ...) to their
 * binary operator counterparts. Used to desugar `a.prop += v` into
 * `a.prop = a.prop + v` during translation of mutating bodies.
 *
 * Restricted to the four arithmetic operators Pantagruel's AST supports
 * (`+`, `-`, `*`, `/`). `%=` and `**=` are intentionally excluded because
 * the underlying `%` and `**` operators have no Pantagruel counterpart —
 * desugaring them would produce an unsupported binary expression anyway.
 */
const COMPOUND_ASSIGN_TO_BINOP: Map<ts.SyntaxKind, ts.BinaryOperator> = new Map(
  [
    [ts.SyntaxKind.PlusEqualsToken, ts.SyntaxKind.PlusToken],
    [ts.SyntaxKind.MinusEqualsToken, ts.SyntaxKind.MinusToken],
    [ts.SyntaxKind.AsteriskEqualsToken, ts.SyntaxKind.AsteriskToken],
    [ts.SyntaxKind.SlashEqualsToken, ts.SyntaxKind.SlashToken],
  ],
);

/**
 * Map each compound assignment operator to the pair it induces for
 * loop-fold translation: the *inside* combiner (for the comprehension)
 * and the *outside* binary operator (joining prior state to the aggregate).
 *
 *   a.p += f(x)  iterated  =>  p' a = p a + (+ over each x in arr | f x)
 *   a.p -= f(x)  iterated  =>  p' a = p a - (+ over each x in arr | f x)
 *   a.p *= f(x)  iterated  =>  p' a = p a * (* over each x in arr | f x)
 *   a.p /= f(x)  iterated  =>  p' a = p a / (* over each x in arr | f x)
 *
 * Non-commutative outer ops (`-`, `/`) pair with the commutative combiner
 * of their identity group (`+`/`*`), since e.g. `p - f(x1) - f(x2)`
 * equals `p - (f(x1) + f(x2))`.
 */
type CombinerKind = "add" | "mul" | "and" | "or";

interface FoldOps {
  combiner: CombinerKind;
  outer: ts.BinaryOperator;
}
const COMPOUND_ASSIGN_TO_FOLD: Map<ts.SyntaxKind, FoldOps> = new Map([
  [
    ts.SyntaxKind.PlusEqualsToken,
    { combiner: "add", outer: ts.SyntaxKind.PlusToken },
  ],
  [
    ts.SyntaxKind.MinusEqualsToken,
    { combiner: "add", outer: ts.SyntaxKind.MinusToken },
  ],
  [
    ts.SyntaxKind.AsteriskEqualsToken,
    { combiner: "mul", outer: ts.SyntaxKind.AsteriskToken },
  ],
  [
    ts.SyntaxKind.SlashEqualsToken,
    { combiner: "mul", outer: ts.SyntaxKind.SlashToken },
  ],
]);

interface ReduceOpInfo {
  combiner: CombinerKind;
  outer: ts.BinaryOperator;
  /** Source text of the init value that permits eliding init (combiner identity). */
  identityText: string | null;
  /** Whether `acc` can appear on either side — true iff the outer op is commutative. */
  commutative: boolean;
}

/** Map a TypeScript binary operator (as used in `.reduce` callback body) to fold info. */
function binopToReduceInfo(kind: ts.SyntaxKind): ReduceOpInfo | null {
  switch (kind) {
    case ts.SyntaxKind.PlusToken:
      return {
        combiner: "add",
        outer: kind,
        identityText: "0",
        commutative: true,
      };
    case ts.SyntaxKind.MinusToken:
      return {
        combiner: "add",
        outer: kind,
        identityText: null,
        commutative: false,
      };
    case ts.SyntaxKind.AsteriskToken:
      return {
        combiner: "mul",
        outer: kind,
        identityText: "1",
        commutative: true,
      };
    case ts.SyntaxKind.SlashToken:
      return {
        combiner: "mul",
        outer: kind,
        identityText: null,
        commutative: false,
      };
    case ts.SyntaxKind.AmpersandAmpersandToken:
      return {
        combiner: "and",
        outer: kind,
        identityText: "true",
        commutative: true,
      };
    case ts.SyntaxKind.BarBarToken:
      return {
        combiner: "or",
        outer: kind,
        identityText: "false",
        commutative: true,
      };
    default:
      return null;
  }
}

/**
 * Decide whether an init expression evaluates to the combiner's identity element.
 * Normalizes parenthesized/cast wrappers and numeric-literal variants so that
 * `0`, `(0)`, `0.0`, `+0`, `-0` all match identity 0 for `+`, etc. Avoids
 * relying on raw source text, which fails on whitespace or syntactically
 * distinct but semantically equivalent forms.
 */
function isIdentityInit(node: ts.Expression, identityText: string): boolean {
  const inner = unwrapExpression(node);
  if (identityText === "true") {
    return inner.kind === ts.SyntaxKind.TrueKeyword;
  }
  if (identityText === "false") {
    return inner.kind === ts.SyntaxKind.FalseKeyword;
  }
  const n = evaluateNumericLiteral(inner);
  if (n === null) {
    return false;
  }
  const target = Number(identityText);
  return Number.isFinite(target) && n === target;
}

function evaluateNumericLiteral(node: ts.Expression): number | null {
  if (ts.isNumericLiteral(node)) {
    const n = Number(node.text);
    return Number.isFinite(n) ? n : null;
  }
  if (
    ts.isPrefixUnaryExpression(node) &&
    (node.operator === ts.SyntaxKind.PlusToken ||
      node.operator === ts.SyntaxKind.MinusToken) &&
    ts.isNumericLiteral(node.operand)
  ) {
    const n = Number(node.operand.text);
    if (!Number.isFinite(n)) {
      return null;
    }
    return node.operator === ts.SyntaxKind.MinusToken ? -n : n;
  }
  return null;
}

function makeCombiner(kind: CombinerKind): OpaqueCombiner {
  const ast = getAst();
  switch (kind) {
    case "add":
      return ast.combAdd();
    case "mul":
      return ast.combMul();
    case "and":
      return ast.combAnd();
    case "or":
      return ast.combOr();
    default: {
      const _exhaustive: never = kind;
      throw new Error(`unknown combiner kind: ${_exhaustive as string}`);
    }
  }
}

export interface TranslateBodyOptions {
  sourceFile: SourceFile;
  functionName: string;
  strategy: NumericStrategy;
  /** Declarations in scope — used for frame condition generation. */
  declarations?: PantDeclaration[];
  /**
   * Synthesizer cell populated during signature and type translation. Used
   * by the body translator to (a) resolve Map-parameter types to their
   * synthesized domain names when reconstructing the param list, and
   * (b) dispatch `.get`/`.has` on non-interface-field Map receivers.
   */
  synthCell?: SynthCell | undefined;
  /**
   * TS-name → Pant-name mapping produced by `translateSignature`. When
   * provided, the body translator uses these exact names rather than
   * recomputing from `toPantTermName(param.name)`. Required so that
   * collision-resolved names (e.g., `r1` when `r` was already claimed by
   * a synth-record binder) stay in lockstep between the declared
   * signature and body-emitted applications.
   */
  paramNameMap?: ReadonlyMap<string, string> | undefined;
}

/**
 * Translate a TypeScript function body to Pantagruel propositions.
 *
 * Pure functions: return expression becomes `all params | f args = <expr>`.
 * Mutating functions: property assignments become primed propositions,
 * plus frame conditions for unmodified rules.
 */
export function translateBody(opts: TranslateBodyOptions): PropResult[] {
  const {
    sourceFile,
    functionName,
    strategy,
    declarations,
    synthCell,
    paramNameMap,
  } = opts;
  const checker = sourceFile.getProject().getTypeChecker().compilerObject;
  const { node, className } = findFunction(sourceFile, functionName);
  // Strip class qualifier for use in Pantagruel identifiers
  const baseName = toPantTermName(
    functionName.includes(".") ? functionName.split(".", 2)[1]! : functionName,
  );
  const classification = classifyFunction(node, checker);

  // Build param name map (same logic as translateSignature). When the
  // caller passes `paramNameMap` from `translateSignature`, prefer those
  // allocated names — recomputing via `toPantTermName` drops the
  // registry-based collision resolution that signature translation
  // performed (e.g., a param whose kebab-cased name clashed with a
  // synth-record binder was suffixed to `r1` at declaration time, and
  // the body must reference that same suffixed name).
  //
  // Invariant: if a shared `synthCell` is supplied, the signature pass
  // has already advanced its registry, so re-claiming here would
  // silently suffix (`r` → `r1`) and the body would reference
  // different binders than the declared head. Require `paramNameMap`
  // so allocations stay in lockstep between sig and body.
  if (synthCell && !paramNameMap) {
    throw new Error(
      "translateBody: paramNameMap is required when a shared synthCell is supplied — " +
        "the signature pass has already claimed names from the registry, so the body " +
        "must reuse them rather than re-register (which would silently suffix).",
    );
  }
  const paramNames = new Map<string, string>();
  const paramList: Array<{ name: string; type: string }> = [];

  const sig = checker.getSignatureFromDeclaration(node);

  if (className) {
    const thisName = paramNameMap?.get("this");
    const pName =
      thisName ??
      shortParamName(
        className,
        new Set(sig ? sig.getParameters().map((p) => p.name) : []),
      );
    paramNames.set("this", pName);
    paramList.push({ name: pName, type: className });
  }

  if (sig) {
    for (const param of sig.getParameters()) {
      const paramType = checker.getTypeOfSymbol(param);
      // Pass the synthesizer so Map parameters resolve to their synthesized
      // domain names (idempotent; the signature pass already registered
      // them, so this is a lookup rather than a fresh registration).
      const typeName = mapTsType(paramType, checker, strategy, synthCell);
      // With a `paramNameMap` we reuse the signature pass's allocations
      // exactly. Without (standalone / test callers with no synthCell),
      // we fall back to the pure kebab-case — no registry to collide
      // against, so no suffixing is needed.
      const pantName =
        paramNameMap?.get(param.name) ?? toPantTermName(param.name);
      paramNames.set(param.name, pantName);
      paramList.push({ name: pantName, type: typeName });
    }
  }

  if (!node.body) {
    return [];
  }

  if (classification === "pure") {
    return translatePureBody(
      node,
      baseName,
      paramList,
      checker,
      strategy,
      paramNames,
      synthCell,
    );
  } else {
    return translateMutatingBody(
      node,
      checker,
      strategy,
      paramNames,
      declarations ?? [],
      synthCell,
    );
  }
}

function translatePureBody(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  functionName: string,
  params: Array<{ name: string; type: string }>,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  synthCell?: SynthCell,
): PropResult[] {
  const ast = getAst();

  if (!node.body) {
    return [];
  }

  const extracted = extractReturnExpression(node.body, checker);
  if (!extracted) {
    const reason = describeRejectedBody(node.body, checker);
    return [{ kind: "unsupported", reason: `${functionName} — ${reason}` }];
  }

  const supply = makeUniqueSupply(synthCell);
  const inlined = inlineConstBindings(
    extracted.bindings,
    checker,
    strategy,
    paramNames,
    supply,
  );
  if ("error" in inlined) {
    return [
      { kind: "unsupported", reason: `${functionName} — ${inlined.error}` },
    ];
  }

  // Record returns: when the function returns an object literal, decompose
  // into one equation per field of the return type. Observational
  // axiomatization (Kroening & Strichman Ch. 8; Dafny-style postcondition
  // decomposition): `f(a) = { p: e }` becomes `p (f a) = e` for each field.
  // Pantagruel has no record-constructor syntax — its interfaces are opaque
  // domains exposed only through per-field accessor rules — so this is the
  // natural shape.
  //
  // Per-field decomposition only handles the canonical "object-literal
  // terminal with no arms" shape. Object literals in other positions (early-
  // return arm values, branches of an if/else terminal) would otherwise fall
  // through to translateBodyExpr's `ast.var(getText())` fallback and emit
  // garbage Pantagruel — reject those uniformly.
  const armHasObjLit = extracted.bindings.some(
    (b) =>
      b.kind === "earlyReturn" && ts.isObjectLiteralExpression(b.valueExpr),
  );
  const terminalIsObjLit =
    ts.isExpression(extracted.returnExpr) &&
    ts.isObjectLiteralExpression(extracted.returnExpr);
  const terminalIfHasObjLitBranch =
    ts.isIfStatement(extracted.returnExpr) &&
    ifTerminalHasObjLitBranch(extracted.returnExpr, checker);

  if (terminalIsObjLit && inlined.arms.length === 0) {
    return translateRecordReturn(
      extracted.returnExpr as ts.ObjectLiteralExpression,
      functionName,
      params,
      node,
      checker,
      strategy,
      inlined.scopedParams,
      supply,
      synthCell,
      inlined.applyTo,
    );
  }

  if (terminalIsObjLit || armHasObjLit || terminalIfHasObjLitBranch) {
    return [
      {
        kind: "unsupported",
        reason:
          `${functionName} — record return combined with early-return ` +
          `arms or if/else branches is not yet supported`,
      },
    ];
  }

  // IR pipeline: when `TS2PANT_USE_IR=1` is set in the environment,
  // route the return-expression translation through the IR (ir-build →
  // ir-emit). See CLAUDE.md §"Intermediate Representation" for the
  // migration roadmap.
  let rhs: OpaqueExpr;

  // Layer 1 imperative-IR conditional pipeline (workstream M1, gated on
  // `TS2PANT_USE_L1=1` during patch 2; always-on after patch 3 cutover).
  // Routes when the body has prelude arms (early-return if-conversion)
  // or a conditional terminal (if/switch/ternary/Bool-typed &&/||) —
  // exactly the cases where L1 normalization unifies recognition.
  // Plain non-conditional returns fall through to the existing IR or
  // legacy path.
  const l1IsConditionalReturn =
    ts.isIfStatement(extracted.returnExpr) ||
    ts.isSwitchStatement(extracted.returnExpr) ||
    (ts.isExpression(extracted.returnExpr) &&
      isL1ConditionalForm(extracted.returnExpr, checker));
  if (useL1Pipeline() && (inlined.arms.length > 0 || l1IsConditionalReturn)) {
    const l1Ctx = {
      checker,
      strategy,
      paramNames: inlined.scopedParams,
      state: undefined as SymbolicState | undefined,
      supply,
    };
    // Wrap pre-translated arm OpaqueExprs as L1 from-l2 expressions so
    // the L1 cond-builder can compose them with the L1-translated
    // terminal. Each arm's guard and value are independent
    // sub-expressions whose internal normalization isn't M1's concern;
    // M2/M3 will progressively replace these wraps with native L1
    // construction.
    const preludeL1: Array<readonly [IR1Expr, IR1Expr]> = inlined.arms.map(
      ([g, v]) => [ir1FromL2(irWrap(g)), ir1FromL2(irWrap(v))] as const,
    );
    const built = buildL1ConditionalFromArms(
      preludeL1,
      extracted.returnExpr,
      l1Ctx,
    );
    if (!isL1Unsupported(built)) {
      // Wrap the lowered cond in IRWrap so the const-binding Let chain
      // (Stage 6) can substitute hygienic-name references inside it via
      // Pant's `substituteBinder`. This mirrors the IR-pipeline branch
      // below — a uniform IRWrap-wrapped body lets the same Let chain
      // work whether the body came from the IR builder or the L1 cond
      // builder.
      let bodyIR: IRExpr = irWrap(lowerL1ToOpaque(built));
      for (let i = inlined.translatedBindings.length - 1; i >= 0; i--) {
        const tb = inlined.translatedBindings[i]!;
        bodyIR = irLet(tb.hygienicName, irWrap(tb.initExpr), bodyIR);
      }
      rhs = lowerExpr(bodyIR);
    } else {
      // L1 rejected — fall through to legacy. Patch 2 keeps both paths
      // available so dogfood can measure rejection rate. Patch 3 deletes
      // the legacy paths and the rejection becomes terminal.
      const body = translateBodyExpr(
        extracted.returnExpr,
        checker,
        strategy,
        inlined.scopedParams,
        undefined,
        supply,
      );
      if (isBodyUnsupported(body)) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — ${built.unsupported} (L1) / ${body.unsupported} (legacy)`,
          },
        ];
      }
      const terminal = bodyExpr(body);
      const merged =
        inlined.arms.length === 0
          ? terminal
          : ast.cond([
              ...inlined.arms.map(
                ([g, v]) => [g, v] as [OpaqueExpr, OpaqueExpr],
              ),
              [ast.litBool(true), terminal] as [OpaqueExpr, OpaqueExpr],
            ]);
      rhs = inlined.applyTo(merged);
    }
  } else if (useIRPipeline() && ts.isExpression(extracted.returnExpr)) {
    const ir = buildIR(
      extracted.returnExpr,
      checker,
      strategy,
      inlined.scopedParams,
      supply,
    );
    if (isBuildUnsupported(ir)) {
      return [{ kind: "unsupported", reason: ir.unsupported }];
    }
    // Stage 6: build IR `Let` nodes around the body for each const-
    // binding instead of applying the legacy `applyTo` substitution
    // closure. lowerExpr's Let case calls Pant's `substituteBinder`,
    // which is the same primitive `applyTo` used — so the result is
    // semantically identical to the legacy path. Right-fold semantics
    // are preserved: outermost Let is the first binding, innermost is
    // the body.
    //
    // Early-return arms (PR #129 if-conversion) are already OpaqueExprs
    // containing hygienic-name references. Lower the IR terminal, merge
    // it with the arms into a `cond`, then wrap the result as IRWrap
    // inside the Let chain — substituteBinder traverses opaque
    // expressions, so hygienic names inside arms get substituted by the
    // Let chain just like names inside the IR body.
    let bodyIR = ir;
    if (inlined.arms.length > 0) {
      const terminalExpr = lowerExpr(bodyIR);
      const mergedExpr = ast.cond([
        ...inlined.arms.map(([g, v]) => [g, v] as [OpaqueExpr, OpaqueExpr]),
        [ast.litBool(true), terminalExpr] as [OpaqueExpr, OpaqueExpr],
      ]);
      bodyIR = { kind: "ir-wrap", expr: mergedExpr };
    }
    for (let i = inlined.translatedBindings.length - 1; i >= 0; i--) {
      const tb = inlined.translatedBindings[i]!;
      bodyIR = irLet(tb.hygienicName, irWrap(tb.initExpr), bodyIR);
    }
    rhs = lowerExpr(bodyIR);
  } else {
    const body = translateBodyExpr(
      extracted.returnExpr,
      checker,
      strategy,
      inlined.scopedParams,
      undefined,
      supply,
    );

    if (isBodyUnsupported(body)) {
      return [{ kind: "unsupported", reason: body.unsupported }];
    }

    const terminal = bodyExpr(body);
    const merged =
      inlined.arms.length === 0
        ? terminal
        : ast.cond([
            ...inlined.arms.map(([g, v]) => [g, v] as [OpaqueExpr, OpaqueExpr]),
            [ast.litBool(true), terminal] as [OpaqueExpr, OpaqueExpr],
          ]);
    rhs = inlined.applyTo(merged);
  }

  const argExprs = params.map((p) => ast.var(p.name));
  const lhs = ast.app(ast.var(functionName), argExprs);
  return [
    {
      kind: "equation",
      quantifiers: [] as OpaqueParam[],
      lhs,
      rhs,
    },
  ];
}

interface ExtractedBody {
  bindings: ConstBinding[];
  returnExpr: ts.Expression | ts.IfStatement | ts.SwitchStatement;
}

/**
 * True when the property access names a field declared on a user-defined
 * interface or class (e.g., `cache.entries` where `entries` is declared in
 * `interface Cache`, or `this.entries` inside a class method). Disambiguates
 * Stage A (interface/class-field Map encoding) from Stage B (synthesized-
 * domain Map encoding) for `.get`/`.has` on a Map-typed receiver. Class
 * methods reuse the surrounding module's field declarations rather than
 * synthesized handles.
 */
/** Resolve a receiver type to the owner name that declares [fieldName],
 *  then qualify via [fieldRuleName]. Delegates to `resolveFieldOwner`,
 *  which walks the property-declaration chain (handles inheritance) and
 *  tracks distinct owners across union/intersection members so it can
 *  signal ambiguity rather than pick an arbitrary branch. Returns `null`
 *  when the field is ambiguous across distinct owners — property-access
 *  reads/writes must resolve to a single stable owner to stay in
 *  lockstep with declaration emission, so ambiguous sites must surface
 *  as unsupported. A `null`-result from here is always a caller-visible
 *  signal to emit `{ unsupported }`. Non-ambiguous unresolvable cases
 *  (built-in types, type parameters, anonymous shapes with no synth
 *  context) still fall back to the bare kebab'd field name — those
 *  don't collide with qualified rule names. */
export function qualifyFieldAccess(
  receiverType: ts.Type,
  fieldName: string,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell: SynthCell | undefined,
): string | null {
  const r = resolveFieldOwner(
    receiverType,
    fieldName,
    checker,
    strategy,
    synthCell,
  );
  if (r.kind === "resolved") {
    return fieldRuleName(r.owner, fieldName);
  }
  if (r.kind === "ambiguous") {
    return null;
  }
  return toPantTermName(fieldName);
}

export function ambiguousFieldMsg(fieldName: string): string {
  return (
    `property access .${fieldName}: ambiguous owner — ` +
    `union/intersection members declare this field on multiple distinct ` +
    `types, so no single qualified accessor rule applies`
  );
}

function isInterfaceFieldAccess(
  node: ts.PropertyAccessExpression,
  checker: ts.TypeChecker,
): boolean {
  const symbol = checker.getSymbolAtLocation(node.name);
  if (!symbol) {
    return false;
  }
  for (const decl of symbol.getDeclarations() ?? []) {
    if (
      ts.isPropertySignature(decl) &&
      decl.parent &&
      ts.isInterfaceDeclaration(decl.parent)
    ) {
      return true;
    }
    if (
      ts.isPropertyDeclaration(decl) &&
      decl.parent &&
      ts.isClassDeclaration(decl.parent)
    ) {
      return true;
    }
  }
  return false;
}

/**
 * Extract the return expression from a function body, collecting any leading
 * const bindings with pure initializers for inline substitution.
 * Handles:
 *   - Single return statement
 *   - Leading const bindings + return statement
 *   - if/else with returns in both branches (produces a synthetic conditional)
 * Returns null if the body contains let/var bindings or effectful const initializers.
 */
/**
 * Recognize the Kleene μ-minimization pattern as a `let counter = init;`
 * statement followed by `while (P(counter)) counter++;` (with or without
 * braces around the while body).
 *
 * Returns `{ counterName, initTsExpr, predicateTsExpr }` if the pair at
 * `stmts[idx]` and `stmts[idx + 1]` matches; null otherwise. Shape-only
 * match: counter must be a single identifier with any initializer, and the
 * loop body must be exactly `counter++` or `++counter` on the same
 * identifier (no compound bodies, no other writes). Purity of the
 * initializer and predicate is screened in `inlineConstBindings`'s TDZ
 * phase (alongside the sibling forward-reference check) rather than here —
 * `translateBodyExpr` has no handler for bare `++`/`--` expressions, so a
 * side-effectful init or predicate would otherwise lower silently.
 *
 * Standard name: Kleene μ-operator / bounded minimization.
 * Reference: Kleene, *General Recursive Functions of Natural Numbers*,
 * Math. Ann. 112 (1936); Kroening & Strichman, *Decision Procedures* Ch. 4.
 */
function recognizeMuSearch(
  stmts: readonly ts.Statement[],
  idx: number,
  _checker: ts.TypeChecker,
): MuSearch | null {
  if (idx + 1 >= stmts.length) {
    return null;
  }
  const letStmt = stmts[idx]!;
  const whileStmt = stmts[idx + 1]!;

  if (!ts.isVariableStatement(letStmt)) {
    return null;
  }
  if (!(letStmt.declarationList.flags & ts.NodeFlags.Let)) {
    return null;
  }
  if (letStmt.declarationList.declarations.length !== 1) {
    return null;
  }
  const decl = letStmt.declarationList.declarations[0]!;
  if (!ts.isIdentifier(decl.name) || !decl.initializer) {
    return null;
  }
  const counterName = decl.name.text;

  if (!ts.isWhileStatement(whileStmt)) {
    return null;
  }

  // Body must be exactly one `counter++` or `++counter`, either as the
  // direct child of the while (unbraced) or as the sole statement of a
  // single-statement block. Side-effect purity of init and predicate is
  // not pre-screened here: the recognizer identifies the syntactic shape;
  // the downstream translation surfaces any unsupported expressions with
  // their natural error message.
  const body = whileStmt.statement;
  const bodyStmt: ts.Statement | null = ts.isBlock(body)
    ? body.statements.length === 1
      ? body.statements[0]!
      : null
    : body;
  if (!bodyStmt || !ts.isExpressionStatement(bodyStmt)) {
    return null;
  }
  const update = bodyStmt.expression;
  if (
    !ts.isPostfixUnaryExpression(update) &&
    !ts.isPrefixUnaryExpression(update)
  ) {
    return null;
  }
  if (update.operator !== ts.SyntaxKind.PlusPlusToken) {
    return null;
  }
  if (!ts.isIdentifier(update.operand) || update.operand.text !== counterName) {
    return null;
  }
  // Predicate must reference the counter; otherwise the loop is either a
  // no-op or a divergence, neither of which is a μ-search. Translating
  // such a shape as `min over each j: Int, j >= INIT, ~Q | j` (with Q
  // free of j) changes behavior at the non-terminating case.
  if (
    !expressionReferencesNames(whileStmt.expression, new Set([counterName]))
  ) {
    return null;
  }

  return {
    counterName,
    initTsExpr: decl.initializer,
    predicateTsExpr: whileStmt.expression,
  };
}

/**
 * Recognize a prelude *early-return arm*: an `IfStatement` whose body is
 * exactly one return-with-expression and which has no `else` clause. Such a
 * statement contributes one arm `(predicate, returnValue)` to a synthetic
 * `cond` that wraps the rest of the function body — the standard
 * if-conversion of an early-exit guard (Allen et al. POPL 1983).
 *
 * Mirrors the body-shape constraint of `extractReturnFromBranch` so that
 * prelude-position and terminal-position if-conversion accept the same
 * branch shapes. An if with an `else` falls through to the existing
 * terminal-position handling unchanged.
 */
function recognizeEarlyReturnArm(
  stmt: ts.Statement,
): { predicateExpr: ts.Expression; valueExpr: ts.Expression } | null {
  if (!ts.isIfStatement(stmt)) {
    return null;
  }
  if (stmt.elseStatement) {
    return null;
  }
  const body = stmt.thenStatement;
  let returnStmt: ts.Statement | null = null;
  if (ts.isReturnStatement(body)) {
    returnStmt = body;
  } else if (ts.isBlock(body) && body.statements.length === 1) {
    returnStmt = body.statements[0]!;
  }
  if (
    !returnStmt ||
    !ts.isReturnStatement(returnStmt) ||
    !returnStmt.expression
  ) {
    return null;
  }
  return { predicateExpr: stmt.expression, valueExpr: returnStmt.expression };
}

function extractReturnExpression(
  body: ts.Block,
  checker: ts.TypeChecker,
): ExtractedBody | null {
  // Skip guard statements (if-throw patterns and assertion calls)
  const stmts = body.statements.filter((s) => !isGuardStatement(s, checker));

  if (stmts.length === 0) {
    return null;
  }

  const bindings: ConstBinding[] = [];

  // Every statement before the last must be either a const binding or a
  // recognized μ-search pair (`let counter = init; while (P) counter++`).
  // Any other shape rejects the whole body. The last statement is the
  // return / if-else-return.
  const lastIdx = stmts.length - 1;
  let i = 0;
  while (i < lastIdx) {
    const mu = recognizeMuSearch(stmts, i, checker);
    if (mu) {
      bindings.push({ kind: "muSearch", tsName: mu.counterName, mu });
      i += 2;
      continue;
    }

    const arm = recognizeEarlyReturnArm(stmts[i]!);
    if (arm) {
      bindings.push({ kind: "earlyReturn", ...arm });
      i += 1;
      continue;
    }

    const stmt = stmts[i]!;
    if (!ts.isVariableStatement(stmt)) {
      return null;
    }
    const declList = stmt.declarationList;
    if (!(declList.flags & ts.NodeFlags.Const)) {
      return null;
    }

    for (const decl of declList.declarations) {
      if (!ts.isIdentifier(decl.name) || !decl.initializer) {
        return null;
      }
      if (expressionHasSideEffects(decl.initializer, checker)) {
        return null;
      }
      bindings.push({
        kind: "const",
        tsName: decl.name.text,
        initializer: decl.initializer,
      });
    }
    i += 1;
  }

  const last = stmts[lastIdx]!;
  if (ts.isReturnStatement(last) && last.expression) {
    return { bindings, returnExpr: last.expression };
  }
  if (ts.isIfStatement(last) && last.elseStatement) {
    return { bindings, returnExpr: last };
  }
  // Switch as a terminal — only when L1 is enabled. Under USE_L1=0
  // switches still fall through to the normal rejection path because
  // there is no legacy switch translator. Under USE_L1=1, the L1
  // conditional builder accepts switches in this position.
  if (ts.isSwitchStatement(last) && useL1Pipeline()) {
    return { bindings, returnExpr: last };
  }

  return null;
}

function describeRejectedBody(body: ts.Block, checker: ts.TypeChecker): string {
  const stmts = body.statements.filter((s) => !isGuardStatement(s, checker));
  if (stmts.length === 0) {
    return "empty body";
  }
  if (stmts.length > 1) {
    const lastIdx = stmts.length - 1;
    // Walk the prelude with the same recognizers `extractReturnExpression`
    // uses, so the reported reason matches the first pattern that actually
    // failed rather than a heuristic guess.
    let i = 0;
    while (i < lastIdx) {
      if (recognizeMuSearch(stmts, i, checker)) {
        i += 2;
        continue;
      }
      const stmt = stmts[i]!;
      if (recognizeEarlyReturnArm(stmt)) {
        i += 1;
        continue;
      }
      // An if-shaped statement that didn't match recognizeEarlyReturnArm:
      // diagnose precisely.
      if (ts.isIfStatement(stmt)) {
        if (stmt.elseStatement) {
          return "if-with-else only supported as the final statement";
        }
        return "if-with-return body must be a single return statement";
      }
      // A let; while pair where recognizeMuSearch failed — probably a
      // compound while body or non-`i++` body.
      if (
        ts.isVariableStatement(stmt) &&
        stmt.declarationList.flags & ts.NodeFlags.Let &&
        i + 1 < stmts.length &&
        ts.isWhileStatement(stmts[i + 1]!)
      ) {
        return "unsupported while-loop shape (not a recognized μ-search)";
      }
      if (ts.isVariableStatement(stmt)) {
        const declList = stmt.declarationList;
        if (!(declList.flags & ts.NodeFlags.Const)) {
          return "let/var bindings not supported";
        }
        for (const decl of declList.declarations) {
          if (
            decl.initializer &&
            expressionHasSideEffects(decl.initializer, checker)
          ) {
            return "const binding with side-effectful initializer";
          }
        }
      }
      if (ts.isExpressionStatement(stmt)) {
        return "expression statement before return (only const / μ-search / if-early-return allowed)";
      }
      return "local bindings or multiple statements before return";
    }
    // Prelude scanned cleanly — the rejection must be at the terminal
    // statement (e.g., trailing if without else, or non-return).
    const last = stmts[lastIdx]!;
    if (ts.isIfStatement(last) && !last.elseStatement) {
      return "if-without-else as final statement (use `if (P) return E` for early-return arms or add an else branch)";
    }
    if (!ts.isReturnStatement(last)) {
      return "final statement must be a return";
    }
    if (!last.expression) {
      return "return without expression";
    }
    return "local bindings or multiple statements before return";
  }
  const stmt = stmts[0]!;
  if (ts.isReturnStatement(stmt) && !stmt.expression) {
    return "return without expression";
  }
  if (
    ts.isIfStatement(stmt) &&
    !stmt.elseStatement &&
    recognizeEarlyReturnArm(stmt) !== null
  ) {
    return "if-without-else as final statement (use `if (P) return E` for early-return arms or add an else branch)";
  }
  return "non-translatable control flow";
}

/**
 * Shared const-binding inlining (let-elimination) for both pure and mutating paths.
 *
 * Three phases:
 *   1. TDZ validation on TS AST (rejects forward/self references)
 *   2. Translate initializers forward, building scopedParams incrementally
 *   3. Return a right-fold substitution closure
 *
 * The right-fold means substitutions are applied inside-out: the last binding
 * is substituted first, so each step naturally resolves references to earlier
 * bindings that are already embedded in the result.
 */
/**
 * One translated const-binding: a hygienic name (`$N`) and the
 * already-translated initializer as an OpaqueExpr. Exposed so the IR
 * pipeline (Stage 6+) can wrap these in `IRWrap`-valued IR `Let` nodes
 * and let `lowerExpr`'s `substituteBinder` do the substitution at
 * lowering time, replacing the legacy `applyTo` closure.
 */
export interface TranslatedBinding {
  hygienicName: string;
  initExpr: OpaqueExpr;
}

export function inlineConstBindings(
  bindings: ConstBinding[],
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  baseParams: ReadonlyMap<string, string>,
  supply: UniqueSupply,
  state?: SymbolicState,
):
  | {
      applyTo: (expr: OpaqueExpr) => OpaqueExpr;
      translatedBindings: ReadonlyArray<TranslatedBinding>;
      scopedParams: ReadonlyMap<string, string>;
      arms: ReadonlyArray<readonly [OpaqueExpr, OpaqueExpr]>;
    }
  | { error: string } {
  const ast = getAst();

  // Phase 1: TDZ validation — reject forward/self references on TS AST.
  // For μ-search bindings, validate both the init and the predicate; the
  // predicate may reference its own counter (that's the loop), so the
  // counter is removed from the blocked set when checking the predicate.
  // Side-effectful init or predicate (assignments, ++/--, unknown-pure
  // calls) are also rejected here: translateBodyExpr has no explicit
  // handler for ++/-- and silently falls through to `ast.var(getText())`,
  // so without this screen a loop like `while (used.has(i++)) i++;`
  // would lower to a Pant expression containing a bogus var `"i++"`.
  for (const [idx, binding] of bindings.entries()) {
    const blockedNames = new Set(
      bindings.slice(idx).flatMap((b) => bindingNames(b) as string[]),
    );
    if (binding.kind === "const") {
      if (expressionReferencesNames(binding.initializer, blockedNames)) {
        return { error: "const initializer references a later binding" };
      }
    } else if (binding.kind === "muSearch") {
      if (expressionHasSideEffects(binding.mu.initTsExpr, checker)) {
        return { error: "while-loop init has side effects" };
      }
      if (expressionHasSideEffects(binding.mu.predicateTsExpr, checker)) {
        return { error: "while-loop predicate has side effects" };
      }
      if (expressionReferencesNames(binding.mu.initTsExpr, blockedNames)) {
        return { error: "while-loop init references a later binding" };
      }
      const predBlocked = new Set(blockedNames);
      predBlocked.delete(binding.mu.counterName);
      if (expressionReferencesNames(binding.mu.predicateTsExpr, predBlocked)) {
        return { error: "while-loop predicate references a later binding" };
      }
    } else {
      // earlyReturn arm — predicate and value must be pure and may not refer
      // to bindings declared after this point. The arm itself binds nothing,
      // so `blockedNames` here just contains the names of later const /
      // μ-search bindings (earlyReturn entries contribute nothing).
      if (expressionHasSideEffects(binding.predicateExpr, checker)) {
        return { error: "early-return predicate has side effects" };
      }
      if (expressionHasSideEffects(binding.valueExpr, checker)) {
        return { error: "early-return value has side effects" };
      }
      if (expressionReferencesNames(binding.predicateExpr, blockedNames)) {
        return { error: "early-return predicate references a later binding" };
      }
      if (expressionReferencesNames(binding.valueExpr, blockedNames)) {
        return { error: "early-return value references a later binding" };
      }
    }
  }

  // Phase 2: translate initializers as a left fold, threading scopedParams
  // and translatedBindings through the accumulator. Early-return arms are
  // accumulated alongside (they don't introduce a name, but their predicate
  // and value are translated under the scope visible at their position so
  // references to earlier bindings resolve to the correct hygienic `$N`
  // names). Errors short-circuit subsequent work via the `tag: "error"`
  // discriminant; successful steps return a fresh map via `withParam` so no
  // `.set`-style mutation remains.
  type Acc =
    | {
        tag: "ok";
        scopedParams: ReadonlyMap<string, string>;
        translatedBindings: ReadonlyArray<{
          hygienicName: string;
          initExpr: OpaqueExpr;
        }>;
        arms: ReadonlyArray<readonly [OpaqueExpr, OpaqueExpr]>;
      }
    | { tag: "error"; error: string };

  const folded = bindings.reduce<Acc>(
    (acc, binding) => {
      if (acc.tag === "error") {
        return acc;
      }
      if (binding.kind === "earlyReturn") {
        const predRes = translateBindingInit(
          binding.predicateExpr,
          checker,
          strategy,
          acc.scopedParams,
          state,
          supply,
        );
        if ("error" in predRes) {
          return { tag: "error", error: predRes.error };
        }
        const valRes = translateBindingInit(
          binding.valueExpr,
          checker,
          strategy,
          acc.scopedParams,
          state,
          supply,
        );
        if ("error" in valRes) {
          return { tag: "error", error: valRes.error };
        }
        return {
          tag: "ok",
          scopedParams: acc.scopedParams,
          translatedBindings: acc.translatedBindings,
          arms: [...acc.arms, [predRes.value, valRes.value] as const],
        };
      }
      const hygienicName = `$${nextSupply(supply)}`;
      const initExpr =
        binding.kind === "const"
          ? translateBindingInit(
              binding.initializer,
              checker,
              strategy,
              acc.scopedParams,
              state,
              supply,
            )
          : translateMuSearchInit(
              binding.mu,
              checker,
              strategy,
              acc.scopedParams,
              state,
              supply,
            );
      if ("error" in initExpr) {
        return { tag: "error", error: initExpr.error };
      }
      return {
        tag: "ok",
        scopedParams: withParam(acc.scopedParams, binding.tsName, hygienicName),
        translatedBindings: [
          ...acc.translatedBindings,
          { hygienicName, initExpr: initExpr.value },
        ],
        arms: acc.arms,
      };
    },
    { tag: "ok", scopedParams: baseParams, translatedBindings: [], arms: [] },
  );

  if (folded.tag === "error") {
    return { error: folded.error };
  }

  // Phase 3: right-fold substitution closure. `reduceRight` applies the last
  // binding first so references to earlier bindings inside its init remain
  // unresolved — they get substituted in subsequent iterations.
  const { scopedParams, translatedBindings, arms } = folded;
  const applyTo = (expr: OpaqueExpr): OpaqueExpr =>
    translatedBindings.reduceRight(
      (acc, { hygienicName, initExpr }) =>
        ast.substituteBinder(acc, hygienicName, initExpr),
      expr,
    );

  return { applyTo, translatedBindings, scopedParams, arms };
}

type BindingInitResult = { value: OpaqueExpr } | { error: string };

function translateBindingInit(
  initializer: ts.Expression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  scopedParams: ReadonlyMap<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BindingInitResult {
  const result = translateBodyExpr(
    initializer,
    checker,
    strategy,
    scopedParams,
    state,
    supply,
  );
  if (isBodyUnsupported(result)) {
    return { error: result.unsupported };
  }
  return { value: bodyExpr(result) };
}

/**
 * Emit a `min over each j: Int, j >= init, ~P(j) | j` expression for a
 * recognized μ-search binding (binder type follows `strategy.mapNumber()`).
 * A fresh comprehension binder takes the place of the loop counter inside
 * the predicate. The binder is allocated through the document-wide name
 * registry (when present) so it never collides with the function's own
 * params or with type-derived rule binders; the `$N` `freshHygienicBinder`
 * form would not round-trip through Pantagruel's parser since `$` isn't a
 * legal identifier character.
 */
function translateMuSearchInit(
  mu: MuSearch,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  scopedParams: ReadonlyMap<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BindingInitResult {
  const ast = getAst();

  const initResult = translateBodyExpr(
    mu.initTsExpr,
    checker,
    strategy,
    scopedParams,
    state,
    supply,
  );
  if (isBodyUnsupported(initResult)) {
    return { error: initResult.unsupported };
  }
  const initExpr = bodyExpr(initResult);

  // `synthCell` path uses the document-wide NameRegistry (kebab-cased,
  // numeric-suffixed) and is inherently collision-safe against other
  // registered names. The standalone fallback has to guard itself against
  // the current frame's Pant names — `scopedParams.values()` is the set
  // the comprehension binder must avoid so predicate translation cannot
  // alias a param to the fresh binder.
  let jName: string;
  if (supply.synthCell) {
    jName = cellRegisterName(supply.synthCell, "j");
  } else {
    const usedNames = new Set(scopedParams.values());
    do {
      jName = `j${nextSupply(supply)}`;
    } while (usedNames.has(jName));
  }
  const predicateScope = withParam(scopedParams, mu.counterName, jName);
  const predResult = translateBodyExpr(
    mu.predicateTsExpr,
    checker,
    strategy,
    predicateScope,
    state,
    supply,
  );
  if (isBodyUnsupported(predResult)) {
    return { error: predResult.unsupported };
  }
  const predExpr = bodyExpr(predResult);

  // Binder type follows the active NumericStrategy so the comprehension
  // stays consistent with how `number` is emitted elsewhere. A hardcoded
  // `Nat` would exclude negative INITs from the search domain and
  // diverge from the containing body's numeric type. Real is rejected:
  // μ-search enumerates the discrete sequence `INIT, INIT+1, …` from
  // `counter++`, whereas `min over each j: Real, j >= INIT, ~P(j) | j`
  // ranges over a dense domain — e.g. `while (i * i < 2) i++` would
  // return √2 instead of 2.
  const counterType = strategy.mapNumber();
  if (counterType === "Real") {
    return {
      error:
        "μ-search is only supported for discrete numeric strategies (got Real)",
    };
  }
  return {
    value: ast.eachComb(
      [ast.param(jName, ast.tName(counterType))],
      [
        ast.gExpr(ast.binop(ast.opGe(), ast.var(jName), initExpr)),
        ast.gExpr(ast.unop(ast.opNot(), predExpr)),
      ],
      ast.combMin(),
      ast.var(jName),
    ),
  };
}

function isGuardStatement(
  stmt: ts.Statement,
  checker: ts.TypeChecker,
): boolean {
  // Assertion call or followable guard call — must match the same purity
  // checks used by scanBodyForGuards in translate-signature.ts
  if (ts.isExpressionStatement(stmt) && ts.isCallExpression(stmt.expression)) {
    const call = stmt.expression;
    if (!isPureExpression(call.expression)) {
      return false;
    }
    if (!call.arguments.every(isPureExpression)) {
      return false;
    }
    if (isAssertionCall(checker, call) !== null) {
      return true;
    }
    if (isFollowableGuardCall(call, checker)) {
      return true;
    }
  }

  if (!ts.isIfStatement(stmt)) {
    return false;
  }
  // Condition must be pure (aligned with classifyGuardIf's isPureExpression check)
  if (!isPureExpression(stmt.expression)) {
    return false;
  }
  // if (...) { throw } without else
  if (!stmt.elseStatement && blockThrows(stmt.thenStatement, checker)) {
    return true;
  }
  // if (...) { ... } else { throw }
  if (stmt.elseStatement && blockThrows(stmt.elseStatement, checker)) {
    // Only a guard if the then-block has no side effects and doesn't return.
    // A mutating then-branch like `if (ok) { a.balance = 1; } else { throw e; }`
    // must NOT be classified as a guard — collectAssignments() needs to see it.
    return (
      blockHasNoSideEffects(stmt.thenStatement, checker) &&
      !blockReturns(stmt.thenStatement)
    );
  }
  return false;
}

function variableStatementHasNoSideEffects(
  stmt: ts.VariableStatement,
  checker: ts.TypeChecker,
): boolean {
  return stmt.declarationList.declarations.every(
    (decl) =>
      !decl.initializer || !expressionHasSideEffects(decl.initializer, checker),
  );
}

function blockThrows(node: ts.Statement, checker: ts.TypeChecker): boolean {
  if (ts.isThrowStatement(node)) {
    return true;
  }
  if (ts.isBlock(node)) {
    const stmts = node.statements;
    if (stmts.length === 0) {
      return false;
    }
    // Last statement must be a throw; all preceding must be side-effect-free
    // (variable declarations for building the error message, etc.)
    if (!ts.isThrowStatement(stmts[stmts.length - 1]!)) {
      return false;
    }
    return stmts
      .slice(0, -1)
      .every(
        (s) =>
          ts.isVariableStatement(s) &&
          variableStatementHasNoSideEffects(s, checker),
      );
  }
  return false;
}

function blockReturns(node: ts.Statement): boolean {
  if (ts.isBlock(node)) {
    return node.statements.some((s) => ts.isReturnStatement(s));
  }
  return ts.isReturnStatement(node);
}

/** Check that a statement/block contains no assignments or property writes. */
function blockHasNoSideEffects(
  node: ts.Statement,
  checker: ts.TypeChecker,
): boolean {
  if (ts.isBlock(node)) {
    return node.statements.every((s) => blockHasNoSideEffects(s, checker));
  }
  if (ts.isExpressionStatement(node)) {
    return !expressionHasSideEffects(node.expression, checker);
  }
  // Variable declarations are fine only if initializers have no side effects
  if (ts.isVariableStatement(node)) {
    return variableStatementHasNoSideEffects(node, checker);
  }
  // Return statements, throw statements are fine
  if (ts.isReturnStatement(node) || ts.isThrowStatement(node)) {
    return true;
  }
  // if/for/while/switch may contain mutations — treat as side-effectful
  return false;
}

/** Check whether a TS expression references any variable name from the given
 *  set as a free variable. Scope-aware: nested function/arrow parameters
 *  shadow outer bindings, so `xs.some(i => i > 0)` does not report dependence
 *  on an outer `i`. Default-value expressions and property-access `.name`
 *  tokens are handled specially. */
function expressionReferencesNames(
  expr: ts.Expression,
  names: Set<string>,
): boolean {
  return nodeReferencesNames(unwrapExpression(expr), names);
}

/** Walk a binding pattern (identifier, object, array) and collect every
 *  identifier it binds, including nested patterns and rest elements. */
function collectBindingNames(name: ts.BindingName, out: Set<string>): void {
  if (ts.isIdentifier(name)) {
    out.add(name.text);
    return;
  }
  for (const element of name.elements) {
    if (ts.isBindingElement(element)) {
      collectBindingNames(element.name, out);
    }
  }
}

/** Collect sub-expressions of a binding pattern that are evaluated in the
 *  enclosing scope before the bound names take effect: computed property
 *  keys (`{ [expr]: x }`) and default-value expressions (`{ x = expr }` or
 *  `[x = expr]`). */
function collectBindingOuterScopeExprs(
  name: ts.BindingName,
  out: ts.Expression[],
): void {
  if (ts.isIdentifier(name)) {
    return;
  }
  for (const element of name.elements) {
    if (ts.isBindingElement(element)) {
      if (
        element.propertyName &&
        ts.isComputedPropertyName(element.propertyName)
      ) {
        out.push(element.propertyName.expression);
      }
      if (element.initializer) {
        out.push(element.initializer);
      }
      collectBindingOuterScopeExprs(element.name, out);
    }
  }
}

function nodeReferencesNames(node: ts.Node, names: Set<string>): boolean {
  if (names.size === 0) {
    return false;
  }
  if (ts.isIdentifier(node)) {
    return names.has(node.text);
  }
  // Property access: the `.name` token is a syntactic position, not a
  // variable reference.
  if (ts.isPropertyAccessExpression(node)) {
    return nodeReferencesNames(node.expression, names);
  }
  // Object-literal property key: `{ foo: expr }` — `foo` is a syntactic
  // key, not a variable reference. Only the initializer references
  // free vars, and computed-property-name expressions are evaluated in
  // the outer scope.
  if (ts.isPropertyAssignment(node)) {
    if (
      ts.isComputedPropertyName(node.name) &&
      nodeReferencesNames(node.name.expression, names)
    ) {
      return true;
    }
    return nodeReferencesNames(node.initializer, names);
  }
  // Shorthand `{ foo }` *is* sugar for `{ foo: foo }` — the identifier
  // at that position is a variable reference. Default value (`{ foo = e }`
  // in destructuring-assignment form) evaluates in the outer scope.
  if (ts.isShorthandPropertyAssignment(node)) {
    if (names.has(node.name.text)) {
      return true;
    }
    if (node.objectAssignmentInitializer) {
      return nodeReferencesNames(node.objectAssignmentInitializer, names);
    }
    return false;
  }
  // Nested function-like scopes. Parameter bindings (including nested
  // identifiers inside destructuring patterns and a named function
  // expression's own name) shadow outer bindings inside the body.
  // Expressions evaluated before bindings take effect — top-level
  // default values, nested default values, computed destructuring
  // keys, and a method/accessor's own computed property-name
  // expression — are still walked in the outer scope. A method's
  // non-computed name is a syntactic key and is not a reference.
  if (
    ts.isArrowFunction(node) ||
    ts.isFunctionExpression(node) ||
    ts.isMethodDeclaration(node) ||
    ts.isGetAccessorDeclaration(node) ||
    ts.isSetAccessorDeclaration(node) ||
    ts.isConstructorDeclaration(node)
  ) {
    for (const p of node.parameters) {
      if (p.initializer && nodeReferencesNames(p.initializer, names)) {
        return true;
      }
      const outerScopeExprs: ts.Expression[] = [];
      collectBindingOuterScopeExprs(p.name, outerScopeExprs);
      for (const e of outerScopeExprs) {
        if (nodeReferencesNames(e, names)) {
          return true;
        }
      }
    }
    if (
      (ts.isMethodDeclaration(node) ||
        ts.isGetAccessorDeclaration(node) ||
        ts.isSetAccessorDeclaration(node)) &&
      ts.isComputedPropertyName(node.name) &&
      nodeReferencesNames(node.name.expression, names)
    ) {
      return true;
    }
    const shadowed = new Set<string>();
    for (const p of node.parameters) {
      collectBindingNames(p.name, shadowed);
    }
    if (ts.isFunctionExpression(node) && node.name) {
      shadowed.add(node.name.text);
    }
    const innerNames = new Set<string>();
    for (const n of names) {
      if (!shadowed.has(n)) {
        innerNames.add(n);
      }
    }
    // Overload signatures and ambient methods have no body.
    if (!node.body) {
      return false;
    }
    return nodeReferencesNames(node.body, innerNames);
  }
  return (
    ts.forEachChild(node, (child) => nodeReferencesNames(child, names)) ?? false
  );
}

/** Unwrap parentheses, type assertions, and non-null assertions to get the inner expression. */
function unwrapExpression(expr: ts.Expression): ts.Expression {
  while (
    ts.isParenthesizedExpression(expr) ||
    ts.isAsExpression(expr) ||
    ts.isSatisfiesExpression(expr) ||
    ts.isNonNullExpression(expr)
  ) {
    expr = expr.expression;
  }
  return expr;
}

export function expressionHasSideEffects(
  expr: ts.Expression,
  checker: ts.TypeChecker,
): boolean {
  expr = unwrapExpression(expr);

  if (ts.isDeleteExpression(expr)) {
    return true;
  }

  if (ts.isBinaryExpression(expr)) {
    // Any assignment operator
    return (
      (expr.operatorToken.kind >= ts.SyntaxKind.EqualsToken &&
        expr.operatorToken.kind <= ts.SyntaxKind.CaretEqualsToken) ||
      expressionHasSideEffects(expr.left, checker) ||
      expressionHasSideEffects(expr.right, checker)
    );
  }
  if (ts.isCallExpression(expr)) {
    if (isKnownPureCall(expr, checker)) {
      // Known-pure callees still require a pure callee expression —
      // `makeString().trim()` passes the name-based builtin allowlist
      // but the receiver `makeString()` is itself a user call with
      // unknown effects. The Tier 1a checks in `isKnownPureCall`
      // already enforce this for Map/Set/HO-array, but not for
      // Math/String/Number/PURE_ARRAY entries, so we double-check here.
      return (
        expressionHasSideEffects(expr.expression, checker) ||
        expr.arguments.some((a) => expressionHasSideEffects(a, checker))
      );
    }
    return true;
  }
  if (ts.isNewExpression(expr) || ts.isAwaitExpression(expr)) {
    return true;
  }
  if (ts.isPrefixUnaryExpression(expr) || ts.isPostfixUnaryExpression(expr)) {
    const op = expr.operator;
    return (
      op === ts.SyntaxKind.PlusPlusToken ||
      op === ts.SyntaxKind.MinusMinusToken ||
      expressionHasSideEffects(expr.operand, checker)
    );
  }
  return (
    ts.forEachChild(expr, (child) =>
      ts.isExpression(child) ? expressionHasSideEffects(child, checker) : false,
    ) ?? false
  );
}

/**
 * Translate a TS expression to an opaque Pantagruel AST node, extending the
 * base translateExpr with support for ternary, array ops, and if/else as cond.
 *
 * When a `state` is provided (mutating-body context), property-access reads
 * first consult the symbolic state so that `a.balance` after an assignment
 * `a.balance = v` evaluates to `v`.
 */
export function translateBodyExpr(
  expr: ts.Expression | ts.Statement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BodyResult {
  const ast = getAst();

  if (ts.isExpression(expr)) {
    expr = unwrapExpression(expr);
  }

  // Layer 1 imperative-IR conditional pipeline (workstream M1, gated on
  // `TS2PANT_USE_L1=1` during patch 2). Routes if-statement, ternary,
  // switch, and Bool-typed `&&`/`||` through the L1 builder. Other forms
  // and L1 rejections fall through to the legacy handlers below.
  if (useL1Pipeline()) {
    if (
      ts.isIfStatement(expr) ||
      ts.isSwitchStatement(expr) ||
      isL1ConditionalForm(expr, checker)
    ) {
      const l1 = buildL1Conditional(
        expr as ts.Expression | ts.IfStatement | ts.SwitchStatement,
        { checker, strategy, paramNames, state, supply },
      );
      if (!isL1Unsupported(l1)) {
        return { expr: lowerL1ToOpaque(l1) };
      }
      // L1 rejected — under USE_L1=1 with patch 2, fall through to the
      // legacy handlers so dogfood / existing fixtures still translate.
      // Patch 3 deletes the legacy conditional paths and the rejection
      // becomes terminal.
    }
  }

  // if/else statement -> cond
  if (ts.isIfStatement(expr)) {
    return translateIfStatement(
      expr,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
  }

  // Ternary: a ? b : c -> cond([[a, b], [true, c]])
  if (ts.isConditionalExpression(expr)) {
    const cond = translateBodyExpr(
      expr.condition,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(cond)) {
      return cond;
    }
    const whenTrue = translateBodyExpr(
      expr.whenTrue,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(whenTrue)) {
      return whenTrue;
    }
    const whenFalse = translateBodyExpr(
      expr.whenFalse,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(whenFalse)) {
      return whenFalse;
    }
    return {
      expr: ast.cond([
        [bodyExpr(cond), bodyExpr(whenTrue)],
        [ast.litBool(true), bodyExpr(whenFalse)],
      ]),
    };
  }

  // Property access with special array operations
  if (ts.isPropertyAccessExpression(expr)) {
    const prop = expr.name.text;
    const obj = translateBodyExpr(
      expr.expression,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(obj)) {
      return obj;
    }
    // Optional chain `x?.prop` under list-lift. With `x: [T]` on the Pant
    // side, the functor-lift encoding is `each $n in x | prop $n`, giving
    // `[U]` — empty when x is null/empty, singleton when x is present.
    // Chains compose as nested comprehensions: TS parses `x?.a.b` as outer
    // `.b` (no questionDotToken) wrapping inner `x?.a` (questionDotToken),
    // with the outer tail marked by `NodeFlags.OptionalChain`. Lift at the
    // tail too, so `x?.a.b` becomes `each $n' in (each $n in x | a $n) | b $n'`
    // rather than falling through to a plain property access on the list-
    // lifted receiver. When the receiver is not nullable in TS, `?.`
    // degenerates to `.` and falls through.
    const inOptionalChain = (expr.flags & ts.NodeFlags.OptionalChain) !== 0;
    if (inOptionalChain) {
      let shouldLift = false;
      if (expr.questionDotToken !== undefined) {
        const receiverTsType = checker.getTypeAtLocation(expr.expression);
        shouldLift = isNullableTsType(receiverTsType);
      } else if (ts.isOptionalChain(expr.expression)) {
        shouldLift = true;
      }
      if (shouldLift) {
        // Qualify the accessor by the receiver's element type so the
        // comprehension body matches the declaration-site rule emitted by
        // translateTypes. `resolveFieldOwner` walks union members, so a
        // nullable receiver like `Account | null` resolves to `Account`.
        const receiverTsType = checker.getTypeAtLocation(expr.expression);
        const ruleName = qualifyFieldAccess(
          receiverTsType,
          prop,
          checker,
          strategy,
          supply.synthCell,
        );
        if (ruleName === null) {
          return { unsupported: ambiguousFieldMsg(prop) };
        }
        const binderName = freshHygienicBinder(supply);
        return {
          expr: ast.each(
            [],
            [ast.gIn(binderName, bodyExpr(obj))],
            ast.app(ast.var(ruleName), [ast.var(binderName)]),
          ),
        };
      }
    }
    // .length (array) / .size (Set) -> #obj
    if (prop === "length" || prop === "size") {
      const receiverType = checker.getTypeAtLocation(expr.expression);
      const isArray = prop === "length" && checker.isArrayType(receiverType);
      const isSet = prop === "size" && isSetType(receiverType);
      if (isArray || isSet) {
        return { expr: ast.unop(ast.opCard(), bodyExpr(obj)) };
      }
    }
    // Qualify the rule symbol by the receiver's interface, matching the
    // declaration-site emission in translateTypes. Symbolic-state keys use
    // the qualified form too so writes and reads hash under the same key.
    const receiverType = checker.getTypeAtLocation(expr.expression);
    const ruleName = qualifyFieldAccess(
      receiverType,
      prop,
      checker,
      strategy,
      supply.synthCell,
    );
    if (ruleName === null) {
      return { unsupported: ambiguousFieldMsg(prop) };
    }
    // Consult symbolic state for a prior write at this location. Apply the
    // same canonicalization as the write site (see SymbolicState) so that
    // reads through const aliases hit the prior write — e.g.,
    // `const x = a; x.balance = 1; x.balance += 2` must see the `= 1` write
    // under a key that matches both the read and the write.
    if (state !== undefined) {
      const key = symbolicKey(ruleName, state.canonicalize(bodyExpr(obj)));
      const entry = state.writes.get(key);
      if (entry !== undefined && entry.kind === "property") {
        return { expr: entry.value };
      }
    }
    return { expr: ast.app(ast.var(ruleName), [bodyExpr(obj)]) };
  }

  // Call expression: handle .includes(), .filter().map(), etc.
  // A Map.set/delete call returns a `{ effect }` BodyResult which only
  // `symbolicExecute` consumes as a statement; in any expression context
  // (ternary arm, binary operand, arg, etc.) turn it into `{ unsupported }`
  // so downstream `bodyExpr()` calls see a narrow result and never throw.
  if (ts.isCallExpression(expr)) {
    return rejectEffect(
      translateCallExpr(expr, checker, strategy, paramNames, state, supply),
    );
  }

  // Prefix unary: !x -> unop(opNot(), x), -x -> unop(opNeg(), x)
  if (ts.isPrefixUnaryExpression(expr)) {
    const operand = translateBodyExpr(
      expr.operand,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(operand)) {
      return operand;
    }
    if (expr.operator === ts.SyntaxKind.ExclamationToken) {
      return { expr: ast.unop(ast.opNot(), bodyExpr(operand)) };
    }
    if (expr.operator === ts.SyntaxKind.MinusToken) {
      return { expr: ast.unop(ast.opNeg(), bodyExpr(operand)) };
    }
  }

  // Binary expression
  if (ts.isBinaryExpression(expr)) {
    // Nullish-coalescing lowering under the list-lift encoding.
    // `x ?? y` where `x: T | null` translates to `x: [T]` on the Pant side,
    // with `#x = 0` as the null test. The result type depends on `y`:
    //   - `y: T`      (default, non-nullable)  → result `T`;
    //                 emit `cond #x = 0 => y, true => (x 1)`.
    //   - `y: [T]`    (nested nullable)         → result `[T]`;
    //                 emit `cond #x = 0 => y, true => x`.
    // When `x` is not nullable in TS, `??` is semantically a no-op — emit
    // just the LHS. This matches Dafny's nullable-typed fromMaybe pattern
    // (Dafny Reference Manual §"Nullable Types"). See CLAUDE.md "Option-
    // Type Elimination" for the broader encoding.
    if (expr.operatorToken.kind === ts.SyntaxKind.QuestionQuestionToken) {
      const leftTsType = checker.getTypeAtLocation(expr.left);
      const leftResult = translateBodyExpr(
        expr.left,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(leftResult)) {
        return leftResult;
      }
      if (!isNullableTsType(leftTsType)) {
        // LHS can never be nullish — `??` degenerates to just the LHS.
        return leftResult;
      }
      const rightResult = translateBodyExpr(
        expr.right,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(rightResult)) {
        return rightResult;
      }
      const xExpr = bodyExpr(leftResult);
      const yExpr = bodyExpr(rightResult);
      const rightTsType = checker.getTypeAtLocation(expr.right);
      const cardZero = ast.binop(
        ast.opEq(),
        ast.unop(ast.opCard(), xExpr),
        ast.litNat(0),
      );
      const presentBranch = isNullableTsType(rightTsType)
        ? xExpr
        : ast.app(xExpr, [ast.litNat(1)]);
      return {
        expr: ast.cond([
          [cardZero, yExpr],
          [ast.litBool(true), presentBranch],
        ]),
      };
    }
    const op = translateOperator(expr.operatorToken.kind);
    if (op === null) {
      return {
        unsupported: `operator ${ts.SyntaxKind[expr.operatorToken.kind]}`,
      };
    }
    const left = translateBodyExpr(
      expr.left,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(left)) {
      return left;
    }
    const right = translateBodyExpr(
      expr.right,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(right)) {
      return right;
    }
    return { expr: ast.binop(op, bodyExpr(left), bodyExpr(right)) };
  }

  // Fall through to base translateExpr for identifiers, literals, this, etc.
  if (ts.isExpression(expr)) {
    return {
      expr: translateExpr(
        expr,
        checker,
        strategy,
        paramNames,
        supply.synthCell,
      ),
    };
  }

  return { unsupported: "non-expression statement" };
}

function translateIfStatement(
  stmt: ts.IfStatement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BodyResult {
  const ast = getAst();

  const cond = translateBodyExpr(
    stmt.expression,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  if (isBodyUnsupported(cond)) {
    return cond;
  }
  const thenExpr = extractReturnFromBranch(stmt.thenStatement, checker);
  const elseExpr = stmt.elseStatement
    ? extractReturnFromBranch(stmt.elseStatement, checker)
    : null;

  if (thenExpr && elseExpr) {
    const thenVal = translateBodyExpr(
      thenExpr,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(thenVal)) {
      return thenVal;
    }
    const elseVal = translateBodyExpr(
      elseExpr,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(elseVal)) {
      return elseVal;
    }
    return {
      expr: ast.cond([
        [bodyExpr(cond), bodyExpr(thenVal)],
        [ast.litBool(true), bodyExpr(elseVal)],
      ]),
    };
  }

  return { unsupported: "if statement without return in both branches" };
}

function extractReturnFromBranch(
  stmt: ts.Statement,
  checker: ts.TypeChecker,
): ts.Expression | null {
  if (ts.isReturnStatement(stmt) && stmt.expression) {
    return stmt.expression;
  }
  if (ts.isBlock(stmt)) {
    // Apply the same rule as extractReturnExpression: only allow a single
    // return (after filtering guards). Blocks with local declarations or
    // multiple non-guard statements are rejected so we don't leak
    // branch-scoped bindings into the generated proposition.
    const nonGuard = stmt.statements.filter(
      (s) => !isGuardStatement(s, checker),
    );
    if (nonGuard.length === 1) {
      const s = nonGuard[0]!;
      if (ts.isReturnStatement(s) && s.expression) {
        return s.expression;
      }
    }
  }
  return null;
}

function ifTerminalHasObjLitBranch(
  stmt: ts.IfStatement,
  checker: ts.TypeChecker,
): boolean {
  const branchHasObjLit = (branch: ts.Statement): boolean => {
    const ret = extractReturnFromBranch(branch, checker);
    return ret !== null && ts.isObjectLiteralExpression(ret);
  };
  if (branchHasObjLit(stmt.thenStatement)) {
    return true;
  }
  if (!stmt.elseStatement) {
    return false;
  }
  if (ts.isIfStatement(stmt.elseStatement)) {
    return ifTerminalHasObjLitBranch(stmt.elseStatement, checker);
  }
  return branchHasObjLit(stmt.elseStatement);
}

/** Get element type name for an array-typed TS expression, or null. */
function getArrayElementType(
  tsExpr: ts.Expression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
): string | null {
  const sourceType = checker.getTypeAtLocation(tsExpr);
  if (!checker.isArrayType(sourceType)) {
    return null;
  }
  const typeArgs = checker.getTypeArguments(sourceType as ts.TypeReference);
  return typeArgs.length === 1
    ? mapTsType(typeArgs[0]!, checker, strategy)
    : "?";
}

/**
 * Translate `.filter()` / `.map()` on an array. Returns a BodyResult whose
 * `pendingComprehension` encodes the deferred chain (Wadler, TCS 1990).
 * Materialization into `each([], [gIn(binder, arrExpr), ...guards], body)`
 * happens at the chain boundary via `bodyExpr`.
 *
 * `.filter(p)` extends the guard list; `.map(f)` rewrites the projection.
 * Composition preserves the original root array in `arrExpr`.
 */
function translateArrayMethod(
  methodName: "filter" | "map",
  tsReceiver: ts.Expression,
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BodyResult | null {
  const ast = getAst();

  if (!getArrayElementType(tsReceiver, checker, strategy)) {
    return null;
  }

  const receiverRaw = translateBodyExpr(
    tsReceiver,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  const receiver = rejectEffect(receiverRaw);
  if (isBodyUnsupported(receiver)) {
    return receiver;
  }

  const pending = receiver.pendingComprehension;
  const isComposing = pending !== undefined;
  // Hygienic `$N` binders (Barendregt convention): they cannot clash with
  // user-visible identifiers or with other fresh binders from the same
  // translation session.
  const sourceBinder = isComposing
    ? pending.binder
    : freshHygienicBinder(supply);
  const callbackBinder = isComposing
    ? freshHygienicBinder(supply)
    : sourceBinder;
  const extendedParams = new Map(paramNames);
  extendedParams.set(callbackBinder, callbackBinder);

  const rawBody = extractArrowBody(
    expr.arguments[0]!,
    callbackBinder,
    extendedParams,
    checker,
    strategy,
    supply,
  );
  if (!rawBody) {
    return { unsupported: expr.getText() };
  }
  if (isBodyUnsupported(rawBody)) {
    return rawBody;
  }

  // Substitute the callback binder with the prior chain's *projection* (not
  // `var(sourceBinder)` — for a prior `.map(u => score u)`, the element in
  // scope is `score u`, not `u`). Initial step has no prior projection, so
  // `callbackBinder === sourceBinder` and this is a no-op.
  const bodyE = isComposing
    ? ast.substituteBinder(bodyExpr(rawBody), callbackBinder, receiver.expr)
    : bodyExpr(rawBody);

  if (methodName === "filter") {
    if (isComposing) {
      return {
        expr: receiver.expr,
        pendingComprehension: {
          binder: pending.binder,
          arrExpr: pending.arrExpr,
          guards: [...pending.guards, ast.gExpr(bodyE)],
        },
      };
    }
    return {
      expr: ast.var(sourceBinder),
      pendingComprehension: {
        binder: sourceBinder,
        arrExpr: receiver.expr,
        guards: [ast.gExpr(bodyE)],
      },
    };
  }
  // map
  if (isComposing) {
    return {
      expr: bodyE,
      pendingComprehension: pending,
    };
  }
  return {
    expr: bodyE,
    pendingComprehension: {
      binder: sourceBinder,
      arrExpr: receiver.expr,
      guards: [],
    },
  };
}

/**
 * Translate `arr.reduce((acc, x) => acc OP f(x), init)` to a comprehension fold.
 * Emits `init OP (combOP over each x in arr | f(x))`, eliding `init` when it
 * equals the combiner's identity element.
 *
 * Fuses with an upstream `.filter`/`.map` pending comprehension (Wadler,
 * TCS 1990) into a single `eachComb` with accumulated guards and the
 * composed projection.
 */
function translateReduceCall(
  methodName: "reduce" | "reduceRight",
  tsReceiver: ts.Expression,
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BodyResult | null {
  const ast = getAst();

  if (expr.arguments.length !== 2) {
    return { unsupported: `.${methodName} requires an explicit initial value` };
  }

  if (!getArrayElementType(tsReceiver, checker, strategy)) {
    return null;
  }

  const receiverRaw = translateBodyExpr(
    tsReceiver,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  const receiver = rejectEffect(receiverRaw);
  if (isBodyUnsupported(receiver)) {
    return receiver;
  }

  const cb = expr.arguments[0]!;
  if (!ts.isArrowFunction(cb)) {
    return { unsupported: `.${methodName} callback must be an arrow function` };
  }
  if (cb.parameters.length !== 2) {
    return {
      unsupported: `.${methodName} callback must take exactly (acc, x)`,
    };
  }
  const accParamNode = cb.parameters[0]!;
  const xParamNode = cb.parameters[1]!;
  if (
    !ts.isIdentifier(accParamNode.name) ||
    !ts.isIdentifier(xParamNode.name)
  ) {
    return {
      unsupported: `.${methodName} callback parameters must be identifiers`,
    };
  }
  const accName = (accParamNode.name as ts.Identifier).text;
  const xName = (xParamNode.name as ts.Identifier).text;

  let body: ts.Expression;
  if (ts.isBlock(cb.body)) {
    const stmts = cb.body.statements;
    if (
      stmts.length !== 1 ||
      !ts.isReturnStatement(stmts[0]!) ||
      !stmts[0]!.expression
    ) {
      return {
        unsupported: `.${methodName} callback block body must be a single return`,
      };
    }
    body = stmts[0]!.expression;
  } else {
    body = cb.body;
  }
  body = unwrapExpression(body);

  if (!ts.isBinaryExpression(body)) {
    return {
      unsupported: `.${methodName} callback body must be 'acc OP f(x)'`,
    };
  }
  const info = binopToReduceInfo(body.operatorToken.kind);
  if (info === null) {
    return {
      unsupported: `.${methodName} operator ${ts.SyntaxKind[body.operatorToken.kind]} has no combiner`,
    };
  }

  // `reduceRight` visits elements right-to-left; safe only for commutative combiners.
  if (methodName === "reduceRight" && !info.commutative) {
    return {
      unsupported: `.reduceRight with non-commutative operator`,
    };
  }

  const leftRoot = getRootIdentifier(body.left);
  const rightRoot = getRootIdentifier(body.right);
  let innerExpr: ts.Expression;
  if (leftRoot === accName && rightRoot !== accName) {
    innerExpr = body.right;
  } else if (rightRoot === accName && leftRoot !== accName) {
    if (!info.commutative) {
      return {
        unsupported: `.${methodName} with acc on the right of a non-commutative operator`,
      };
    }
    innerExpr = body.left;
  } else {
    return {
      unsupported: `.${methodName} callback must reference acc exactly once`,
    };
  }

  if (expressionReferencesNames(innerExpr, new Set([accName]))) {
    return {
      unsupported: `.${methodName} inner expression must not reference acc`,
    };
  }

  const pending = receiver.pendingComprehension;
  // Hygienic `$N` binder for the callback's `x`; in the composing case we'll
  // substitute it away with the prior projection so the outer guard binds
  // `pending.binder`.
  const xBinder = freshHygienicBinder(supply);
  const extendedParams = new Map(paramNames);
  extendedParams.set(xName, xBinder);

  const innerResult = translateBodyExpr(
    innerExpr,
    checker,
    strategy,
    extendedParams,
    state,
    supply,
  );
  if (isBodyUnsupported(innerResult)) {
    return innerResult;
  }

  const comb = makeCombiner(info.combiner);
  let folded: OpaqueExpr;
  if (pending) {
    const projectedInner = ast.substituteBinder(
      bodyExpr(innerResult),
      xBinder,
      receiver.expr,
    );
    folded = ast.eachComb(
      [],
      [ast.gIn(pending.binder, pending.arrExpr), ...pending.guards],
      comb,
      projectedInner,
    );
  } else {
    folded = ast.eachComb(
      [],
      [ast.gIn(xBinder, receiver.expr)],
      comb,
      bodyExpr(innerResult),
    );
  }

  const initNode = expr.arguments[1]!;
  if (
    info.identityText !== null &&
    isIdentityInit(initNode, info.identityText)
  ) {
    return { expr: folded };
  }

  const initResult = translateBodyExpr(
    initNode,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  if (isBodyUnsupported(initResult)) {
    return initResult;
  }

  const outerOp = translateOperator(info.outer);
  if (outerOp === null) {
    return { unsupported: `.${methodName} outer operator translation` };
  }
  return {
    expr: ast.binop(outerOp, bodyExpr(initResult), folded),
  };
}

// --- Structured iteration (for-of / forEach / reduce) ---
//
// Catamorphisms over arrays (Meijer et al., "Functional Programming with
// Bananas, Lenses, Envelopes and Barbed Wire", FPCA 1991). Three shapes:
//
//   A. `for (const x of arr) { x.p = e }`   → `all x in arr | p' x = e`
//   B. `for (const x of arr) { a.p OP= f }` → `p' a = p a OP (combOP over each x in arr | f)`
//   C. `arr.reduce((acc, x) => acc OP f, i)`→ `i OP (combOP over each x in arr | f)`
//
// See CLAUDE.md § Structured Iteration.

interface ShapeBLeaf {
  /** `a` in `a.total += x.v` — the accumulator base expression (must not depend on iterator). */
  target: ts.Expression;
  /** `total` in `a.total += x.v`. */
  prop: string;
  /** Fold op pair (inner combiner + outer binary operator). */
  ops: FoldOps;
  /** RHS expression `f(x)` — may reference the iterator. */
  rhs: ts.Expression;
  /** Optional guard from a wrapping `if (g(x)) { a.p OP= f(x) }`. */
  guard?: ts.Expression;
}

type LoopStmtClass =
  | { kind: "shapeA" }
  | { kind: "shapeB"; leaves: ShapeBLeaf[] }
  | { unsupported: string };

/** Extract the root identifier of a chained property access (`a.b.c` → `a`). */
function getRootIdentifier(expr: ts.Expression): string | null {
  expr = unwrapExpression(expr);
  if (ts.isIdentifier(expr)) {
    return expr.text;
  }
  if (ts.isPropertyAccessExpression(expr)) {
    return getRootIdentifier(expr.expression);
  }
  return null;
}

/**
 * Classify one loop-body statement as Shape A (iterator-targeted write),
 * Shape B (accumulator fold), or unsupported. Handles nested if-stmts:
 *   - if all branches are Shape A, the whole is Shape A
 *   - if the stmt is `if (g(x)) { a.p OP= f }` with no else, it becomes a
 *     single Shape B leaf with `guard = g(x)`.
 */
function classifyLoopStmt(
  stmt: ts.Statement,
  iterName: string,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell: SynthCell | undefined,
  parentGuard?: ts.Expression,
): LoopStmtClass {
  if (
    ts.isExpressionStatement(stmt) &&
    ts.isBinaryExpression(unwrapExpression(stmt.expression))
  ) {
    const bin = unwrapExpression(stmt.expression) as ts.BinaryExpression;
    if (!ts.isPropertyAccessExpression(bin.left)) {
      return {
        unsupported: "loop body assignment target must be a property access",
      };
    }
    const rootName = getRootIdentifier(bin.left.expression);
    const isSimpleAssign = bin.operatorToken.kind === ts.SyntaxKind.EqualsToken;
    const compoundFold = COMPOUND_ASSIGN_TO_FOLD.get(bin.operatorToken.kind);

    if (rootName === iterName) {
      if (!isSimpleAssign) {
        return {
          unsupported: "compound assignment on loop iterator property",
        };
      }
      return { kind: "shapeA" };
    }
    if (compoundFold === undefined) {
      return {
        unsupported:
          "loop accumulator write must use a compound assignment (+=, -=, *=, /=)",
      };
    }
    if (expressionReferencesNames(bin.left.expression, new Set([iterName]))) {
      return { unsupported: "loop accumulator target depends on iterator" };
    }
    // Qualify the field name at classification time so downstream use
    // sites (write emission, prior-read identity, modifiedProps tracking)
    // all see the Pantagruel rule symbol, not the raw TS field name.
    // Pass synth context so anonymous-record accumulators (including
    // alias-backed ones) resolve to the synthesized owner rather than
    // falling back to the bare field name.
    const receiverType = checker.getTypeAtLocation(bin.left.expression);
    const rawProp = bin.left.name.text;
    const propName = qualifyFieldAccess(
      receiverType,
      rawProp,
      checker,
      strategy,
      synthCell,
    );
    if (propName === null) {
      return { unsupported: ambiguousFieldMsg(rawProp) };
    }
    const leaf: ShapeBLeaf = {
      target: bin.left.expression,
      prop: propName,
      ops: compoundFold,
      rhs: bin.right,
    };
    if (parentGuard) {
      leaf.guard = parentGuard;
    }
    return { kind: "shapeB", leaves: [leaf] };
  }

  if (ts.isIfStatement(stmt)) {
    if (expressionHasSideEffects(stmt.expression, checker)) {
      return { unsupported: "impure if-condition in loop body" };
    }
    const thenStmts = flattenStmt(stmt.thenStatement);
    const elseStmts = stmt.elseStatement ? flattenStmt(stmt.elseStatement) : [];

    // Shape A: every branch-leaf is an iterator-write
    const allA = [...thenStmts, ...elseStmts].every((s) => {
      const c = classifyLoopStmt(s, iterName, checker, strategy, synthCell);
      return "kind" in c && c.kind === "shapeA";
    });
    if (allA) {
      return { kind: "shapeA" };
    }

    // Shape B: `if (g) { a.p OP= f }` — single leaf, no else, no parent guard
    if (
      elseStmts.length === 0 &&
      thenStmts.length === 1 &&
      parentGuard === undefined
    ) {
      const inner = classifyLoopStmt(
        thenStmts[0]!,
        iterName,
        checker,
        strategy,
        synthCell,
        stmt.expression,
      );
      if ("kind" in inner && inner.kind === "shapeB") {
        return inner;
      }
    }
    return {
      unsupported:
        "loop body if-statement mixes shapes or has an unsupported structure",
    };
  }

  if (ts.isBlock(stmt)) {
    const stmts = Array.from(stmt.statements);
    const classes = stmts.map((s) =>
      classifyLoopStmt(s, iterName, checker, strategy, synthCell, parentGuard),
    );
    const firstBad = classes.find((c) => "unsupported" in c);
    if (firstBad) {
      return firstBad;
    }
    const allA = classes.every((c) => "kind" in c && c.kind === "shapeA");
    if (allA) {
      return { kind: "shapeA" };
    }
    const allB = classes.every((c) => "kind" in c && c.kind === "shapeB");
    if (allB) {
      const leaves = classes.flatMap(
        (c) => (c as { kind: "shapeB"; leaves: ShapeBLeaf[] }).leaves,
      );
      return { kind: "shapeB", leaves };
    }
    return {
      unsupported: "loop body block mixes iterator and accumulator writes",
    };
  }

  return { unsupported: `loop body statement: ${ts.SyntaxKind[stmt.kind]}` };
}

/**
 * Emit Shape A / Shape B propositions for a loop body.
 *
 * Shape A uses a sub-`symbolicExecute` to reuse the full symbolic-state
 * machinery (path merging via `cond` for conditional writes). Each emitted
 * write is wrapped in `all x in arr | p' x = v`.
 *
 * Shape B writes are merged into the caller's `state`: the outer state's
 * prior value (or the pre-state identity) is combined with the comprehension
 * `(combOP over each x in arr[, g(x)] | f(x))` via the outer binary operator.
 */
function translateForOfLoopBody(
  iterName: string,
  arrExpr: OpaqueExpr,
  bodyStmts: ts.Statement[],
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  state: SymbolicState,
  propositions: PropResult[],
  applyConst: (e: OpaqueExpr) => OpaqueExpr,
  supply: UniqueSupply,
): boolean {
  const ast = getAst();

  const shapeAStmts: ts.Statement[] = [];
  const shapeBLeaves: ShapeBLeaf[] = [];
  for (const stmt of bodyStmts) {
    const cls = classifyLoopStmt(
      stmt,
      iterName,
      checker,
      strategy,
      supply.synthCell,
    );
    if ("unsupported" in cls) {
      propositions.push({ kind: "unsupported", reason: cls.unsupported });
      return false;
    }
    if (cls.kind === "shapeA") {
      shapeAStmts.push(stmt);
    } else {
      shapeBLeaves.push(...cls.leaves);
    }
  }

  // `subState` captures Shape A's per-iteration writes so that Shape B reads
  // of the iterator's properties resolve to the updated expression. E.g.
  // `x.value = x.value + 1; a.total += x.value` must fold as
  // `total a + (+ over each x in xs | value x + 1)`, not `value x`. Property
  // reads against non-iterator objects pass through unchanged (they don't
  // appear as keys in `subState.writes`).
  const subState = makeSymbolicState(applyConst);
  const subParams = new Map(paramNames);
  subParams.set(iterName, iterName);

  if (shapeAStmts.length > 0) {
    const subProps: PropResult[] = [];
    const subBlock = ts.factory.createBlock(shapeAStmts, true);
    const okA = symbolicExecute(
      subBlock,
      checker,
      strategy,
      subParams,
      subState,
      subProps,
      applyConst,
      supply,
      false,
    );
    if (!okA) {
      for (const p of subProps) {
        propositions.push(p);
      }
      return false;
    }
    for (const [, entry] of subState.writes) {
      if (entry.kind !== "property") {
        propositions.push({
          kind: "unsupported",
          reason: "Map mutation inside Shape A loop",
        });
        return false;
      }
      propositions.push({
        kind: "equation",
        quantifiers: [] as OpaqueParam[],
        guards: [ast.gIn(iterName, arrExpr)],
        lhs: ast.app(ast.primed(entry.prop), [entry.objExpr]),
        rhs: entry.value,
      });
      addModifiedProp(state, entry.prop);
    }
  }

  for (const leaf of shapeBLeaves) {
    const accResult = translateBodyExpr(
      leaf.target,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(accResult)) {
      propositions.push({ kind: "unsupported", reason: accResult.unsupported });
      return false;
    }
    const accExpr = applyConst(bodyExpr(accResult));

    const rhsResult = translateBodyExpr(
      leaf.rhs,
      checker,
      strategy,
      subParams,
      subState,
      supply,
    );
    if (isBodyUnsupported(rhsResult)) {
      propositions.push({ kind: "unsupported", reason: rhsResult.unsupported });
      return false;
    }
    const rhsExpr = applyConst(bodyExpr(rhsResult));

    const guards: OpaqueGuard[] = [ast.gIn(iterName, arrExpr)];
    if (leaf.guard) {
      const gResult = translateBodyExpr(
        leaf.guard,
        checker,
        strategy,
        subParams,
        subState,
        supply,
      );
      if (isBodyUnsupported(gResult)) {
        propositions.push({ kind: "unsupported", reason: gResult.unsupported });
        return false;
      }
      guards.push(ast.gExpr(applyConst(bodyExpr(gResult))));
    }

    const comb = makeCombiner(leaf.ops.combiner);
    const folded = ast.eachComb([], guards, comb, rhsExpr);

    const outerOp = translateOperator(leaf.ops.outer);
    if (outerOp === null) {
      propositions.push({
        kind: "unsupported",
        reason: "loop-fold outer operator",
      });
      return false;
    }
    const key = symbolicKey(leaf.prop, accExpr);
    const priorEntry = state.writes.get(key);
    if (priorEntry !== undefined && priorEntry.kind !== "property") {
      propositions.push({
        kind: "unsupported",
        reason: "loop-fold accumulator over a Map receiver",
      });
      return false;
    }
    const priorVal =
      priorEntry?.value ?? ast.app(ast.var(leaf.prop), [accExpr]);
    const newVal = ast.binop(outerOp, priorVal, folded);

    putWrite(state, key, {
      kind: "property",
      prop: leaf.prop,
      objExpr: accExpr,
      value: newVal,
    });
    addWrittenKey(state, key);
  }

  return true;
}

/** Translate a `for (const x of arr) { ... }` statement. */
function translateForOfLoop(
  stmt: ts.ForOfStatement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  state: SymbolicState,
  propositions: PropResult[],
  applyConst: (e: OpaqueExpr) => OpaqueExpr,
  supply: UniqueSupply,
): boolean {
  const initList = stmt.initializer;
  if (
    !ts.isVariableDeclarationList(initList) ||
    !(initList.flags & ts.NodeFlags.Const) ||
    initList.declarations.length !== 1
  ) {
    propositions.push({
      kind: "unsupported",
      reason: "for-of initializer must be a single const binding",
    });
    return false;
  }
  const decl = initList.declarations[0]!;
  if (!ts.isIdentifier(decl.name)) {
    propositions.push({
      kind: "unsupported",
      reason: "for-of destructuring pattern is not supported",
    });
    return false;
  }
  const iterName = decl.name.text;

  const arrResult = translateBodyExpr(
    stmt.expression,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  if (isBodyUnsupported(arrResult)) {
    propositions.push({ kind: "unsupported", reason: arrResult.unsupported });
    return false;
  }
  const arrExpr = applyConst(bodyExpr(arrResult));
  const bodyStmts = flattenStmt(stmt.statement);

  return translateForOfLoopBody(
    iterName,
    arrExpr,
    bodyStmts,
    checker,
    strategy,
    paramNames,
    state,
    propositions,
    applyConst,
    supply,
  );
}

/**
 * Translate a `arr.forEach(x => { ... })` call-statement. Returns null if
 * the call is not a forEach (caller falls back to normal handling).
 */
function translateForEachStmt(
  call: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  state: SymbolicState,
  propositions: PropResult[],
  applyConst: (e: OpaqueExpr) => OpaqueExpr,
  supply: UniqueSupply,
): boolean | null {
  if (
    !ts.isPropertyAccessExpression(call.expression) ||
    call.expression.name.text !== "forEach" ||
    call.arguments.length !== 1
  ) {
    return null;
  }
  const receiver = call.expression.expression;
  const arg = call.arguments[0]!;
  if (!ts.isArrowFunction(arg)) {
    propositions.push({
      kind: "unsupported",
      reason: "forEach callback must be an arrow function",
    });
    return false;
  }
  if (
    arg.parameters.length !== 1 ||
    !ts.isIdentifier(arg.parameters[0]!.name)
  ) {
    propositions.push({
      kind: "unsupported",
      reason: "forEach callback must take a single identifier parameter",
    });
    return false;
  }
  const iterName = (arg.parameters[0]!.name as ts.Identifier).text;

  const arrResult = translateBodyExpr(
    receiver,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  if (isBodyUnsupported(arrResult)) {
    propositions.push({ kind: "unsupported", reason: arrResult.unsupported });
    return false;
  }
  const arrExpr = applyConst(bodyExpr(arrResult));

  const bodyStmts = ts.isBlock(arg.body)
    ? Array.from(arg.body.statements)
    : [ts.factory.createExpressionStatement(arg.body)];

  return translateForOfLoopBody(
    iterName,
    arrExpr,
    bodyStmts,
    checker,
    strategy,
    paramNames,
    state,
    propositions,
    applyConst,
    supply,
  );
}

function translateCallExpr(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BodyResult {
  const ast = getAst();

  // Method calls: obj.method(args)
  if (ts.isPropertyAccessExpression(expr.expression)) {
    const methodName = expr.expression.name.text;
    const tsReceiver = expr.expression.expression;

    // .add(x) / .delete(x) / .clear() on a Set<T> receiver -> emit a
    // `{ effect: SetMutation }` that `symbolicExecute` consumes via
    // `installSetWrite`. Stage A only: the receiver must be a
    // property-access to a declared interface field. Parameter-level Set
    // mutation is rejected explicitly — there is no wrapper domain to
    // prime against.
    //
    // `.delete` dispatches to this branch before the Map branch below
    // because `isMapType` and `isSetType` are mutually exclusive, so the
    // Map check would short-circuit cleanly even if the order were
    // reversed; placing Set first keeps the translation path compact.
    if (
      ((methodName === "add" && expr.arguments.length === 1) ||
        (methodName === "delete" && expr.arguments.length === 1) ||
        (methodName === "clear" && expr.arguments.length === 0)) &&
      isSetType(checker.getTypeAtLocation(tsReceiver))
    ) {
      const normalizedReceiver = unwrapExpression(tsReceiver);
      if (
        !ts.isPropertyAccessExpression(normalizedReceiver) ||
        !isInterfaceFieldAccess(normalizedReceiver, checker)
      ) {
        return { unsupported: "parameter-level Set mutation" };
      }
      const innerObj = normalizedReceiver.expression;
      const rawFieldName = normalizedReceiver.name.text;
      const fieldName = qualifyFieldAccess(
        checker.getTypeAtLocation(innerObj),
        rawFieldName,
        checker,
        strategy,
        supply.synthCell,
      );
      if (fieldName === null) {
        return { unsupported: ambiguousFieldMsg(rawFieldName) };
      }
      const ownerType = mapTsType(
        checker.getTypeAtLocation(innerObj),
        checker,
        strategy,
        supply.synthCell,
      );
      const setTypeArgs = checker.getTypeArguments(
        checker.getTypeAtLocation(normalizedReceiver) as ts.TypeReference,
      );
      if (setTypeArgs.length !== 1) {
        return { unsupported: "Set with unexpected arity" };
      }
      const elemType = mapTsType(
        setTypeArgs[0]!,
        checker,
        strategy,
        supply.synthCell,
      );
      const objRaw = translateBodyExpr(
        innerObj,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      const objExpr = rejectEffect(objRaw);
      if (isBodyUnsupported(objExpr)) {
        return objExpr;
      }
      let elemOpaque: OpaqueExpr | null = null;
      if (methodName !== "clear") {
        const eRaw = translateBodyExpr(
          expr.arguments[0]!,
          checker,
          strategy,
          paramNames,
          state,
          supply,
        );
        const eExpr = rejectEffect(eRaw);
        if (isBodyUnsupported(eExpr)) {
          return eExpr;
        }
        elemOpaque = bodyExpr(eExpr);
      }
      return {
        effect: {
          op: methodName as "add" | "delete" | "clear",
          ruleName: fieldName,
          ownerType,
          elemType,
          objExpr: bodyExpr(objExpr),
          elemExpr: elemOpaque,
        },
      };
    }

    // .set(k, v) / .delete(k) on a Map<K,V> receiver -> emit a `{ effect: ... }`
    // BodyResult that `symbolicExecute` consumes to install a MapRuleWriteEntry.
    // Stage A / Stage B disambiguation mirrors the .get/.has branch below; the
    // owner and key types are captured on the effect so `translateMutatingBody`
    // can emit the quantifier binders when expanding the override equation.
    if (
      ((methodName === "set" && expr.arguments.length === 2) ||
        (methodName === "delete" && expr.arguments.length === 1)) &&
      isMapType(checker.getTypeAtLocation(tsReceiver))
    ) {
      // Unwrap parens, non-null assertions, and type-only wrappers so
      // `(cache.entries).set(...)` and `cache.entries!.set(...)` reach the
      // same Stage A detection as the bare `cache.entries.set(...)` form.
      const normalizedReceiver = unwrapExpression(tsReceiver);
      const stageA =
        ts.isPropertyAccessExpression(normalizedReceiver) &&
        isInterfaceFieldAccess(normalizedReceiver, checker);

      const keyArg = expr.arguments[0]!;
      const kExprRaw = translateBodyExpr(
        keyArg,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      const kExpr = rejectEffect(kExprRaw);
      if (isBodyUnsupported(kExpr)) {
        return kExpr;
      }

      let valueExpr: OpaqueExpr | null = null;
      if (methodName === "set") {
        const vRaw = translateBodyExpr(
          expr.arguments[1]!,
          checker,
          strategy,
          paramNames,
          state,
          supply,
        );
        const vExpr = rejectEffect(vRaw);
        if (isBodyUnsupported(vExpr)) {
          return vExpr;
        }
        valueExpr = bodyExpr(vExpr);
      }

      if (stageA && ts.isPropertyAccessExpression(normalizedReceiver)) {
        const rawFieldName = normalizedReceiver.name.text;
        const innerObj = normalizedReceiver.expression;
        const objRaw = translateBodyExpr(
          innerObj,
          checker,
          strategy,
          paramNames,
          state,
          supply,
        );
        const objExpr = rejectEffect(objRaw);
        if (isBodyUnsupported(objExpr)) {
          return objExpr;
        }
        const ownerType = mapTsType(
          checker.getTypeAtLocation(innerObj),
          checker,
          strategy,
          supply.synthCell,
        );
        // Qualify the field-derived rule + keyPred names, matching the
        // Stage A emission in translateTypes.
        const fieldName = qualifyFieldAccess(
          checker.getTypeAtLocation(innerObj),
          rawFieldName,
          checker,
          strategy,
          supply.synthCell,
        );
        if (fieldName === null) {
          return { unsupported: ambiguousFieldMsg(rawFieldName) };
        }
        const typeArgs = checker.getTypeArguments(
          checker.getTypeAtLocation(normalizedReceiver) as ts.TypeReference,
        );
        if (typeArgs.length !== 2) {
          return { unsupported: "Map with unexpected arity" };
        }
        const keyType = mapTsType(
          typeArgs[0]!,
          checker,
          strategy,
          supply.synthCell,
        );
        return {
          effect: {
            op: methodName as "set" | "delete",
            ruleName: fieldName,
            keyPredName: `${fieldName}-key`,
            ownerType,
            keyType,
            objExpr: bodyExpr(objExpr),
            keyExpr: bodyExpr(kExpr),
            valueExpr,
          },
        };
      }

      // Stage B — synthesized rule lookup (same on-demand registration as
      // the .get/.has branch).
      const receiverType = checker.getTypeAtLocation(tsReceiver);
      const typeArgs = checker.getTypeArguments(
        receiverType as ts.TypeReference,
      );
      if (typeArgs.length !== 2) {
        return { unsupported: "Map with unexpected arity" };
      }
      const kType = mapTsType(
        typeArgs[0]!,
        checker,
        strategy,
        supply.synthCell,
      );
      const vType = mapTsType(
        typeArgs[1]!,
        checker,
        strategy,
        supply.synthCell,
      );
      let info = supply.synthCell
        ? lookupMapKV(supply.synthCell.synth, kType, vType)
        : undefined;
      if (!info && supply.synthCell) {
        cellRegisterMap(supply.synthCell, kType, vType);
        info = lookupMapKV(supply.synthCell.synth, kType, vType);
      }
      if (!info) {
        return {
          unsupported: `Map<${kType}, ${vType}>: key or value type cannot be mangled into a synthesized domain name`,
        };
      }
      const objRaw = translateBodyExpr(
        tsReceiver,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      const objExpr = rejectEffect(objRaw);
      if (isBodyUnsupported(objExpr)) {
        return objExpr;
      }
      return {
        effect: {
          op: methodName as "set" | "delete",
          ruleName: info.names.rule,
          keyPredName: info.names.keyPred,
          ownerType: info.names.domain,
          keyType: kType,
          objExpr: bodyExpr(objExpr),
          keyExpr: bodyExpr(kExpr),
          valueExpr,
        },
      };
    }

    // .get(k) / .has(k) on a Map<K,V> receiver -> 2-arity rule application.
    // Two paths, same encoding shape:
    //   Stage A — receiver is a property access to a declared interface
    //     field. The field's own name is the rule; the owner is the user's
    //     interface (translate-types.ts, interface-field branch).
    //   Stage B — receiver is anything else (parameter, call result, etc.).
    //     The rule was synthesized at signature/type translation time and
    //     is looked up by (K, V) via the MapSynth cell.
    if (
      (methodName === "get" || methodName === "has") &&
      expr.arguments.length === 1 &&
      isMapType(checker.getTypeAtLocation(tsReceiver))
    ) {
      // Match the .set/.delete branch: unwrap so wrapped field receivers
      // (`(cache.entries).get(k)`, `cache.entries!.has(k)`) stay on Stage A.
      const normalizedReceiver = unwrapExpression(tsReceiver);
      const stageA =
        ts.isPropertyAccessExpression(normalizedReceiver) &&
        isInterfaceFieldAccess(normalizedReceiver, checker);

      if (stageA && ts.isPropertyAccessExpression(normalizedReceiver)) {
        const rawFieldName = normalizedReceiver.name.text;
        const innerObj = normalizedReceiver.expression;
        const fieldName = qualifyFieldAccess(
          checker.getTypeAtLocation(innerObj),
          rawFieldName,
          checker,
          strategy,
          supply.synthCell,
        );
        if (fieldName === null) {
          return { unsupported: ambiguousFieldMsg(rawFieldName) };
        }
        const kExpr = translateBodyExpr(
          expr.arguments[0]!,
          checker,
          strategy,
          paramNames,
          state,
          supply,
        );
        if (isBodyUnsupported(kExpr)) {
          return kExpr;
        }
        const objExpr = translateBodyExpr(
          innerObj,
          checker,
          strategy,
          paramNames,
          state,
          supply,
        );
        if (isBodyUnsupported(objExpr)) {
          return objExpr;
        }
        const typeArgs = checker.getTypeArguments(
          checker.getTypeAtLocation(normalizedReceiver) as ts.TypeReference,
        );
        if (typeArgs.length !== 2) {
          return { unsupported: "Map with unexpected arity" };
        }
        const ownerType = mapTsType(
          checker.getTypeAtLocation(innerObj),
          checker,
          strategy,
          supply.synthCell,
        );
        const keyType = mapTsType(
          typeArgs[0]!,
          checker,
          strategy,
          supply.synthCell,
        );
        return {
          expr: readMapThroughWrites(
            state,
            methodName as "get" | "has",
            fieldName,
            `${fieldName}-key`,
            ownerType,
            keyType,
            bodyExpr(objExpr),
            bodyExpr(kExpr),
          ),
        };
      }

      // Stage B: synthesized rule lookup. Register on demand — a body-only
      // receiver (e.g., `build().get(k)!` where `build`'s return type wasn't
      // surfaced through the current function's signature or referenced
      // types) wouldn't be pre-registered by the signature/type passes.
      // The pipeline drains any new registrations with a second emit() call
      // after body translation completes.
      const receiverType = checker.getTypeAtLocation(tsReceiver);
      const typeArgs = checker.getTypeArguments(
        receiverType as ts.TypeReference,
      );
      if (typeArgs.length !== 2) {
        return { unsupported: "Map with unexpected arity" };
      }
      const kType = mapTsType(
        typeArgs[0]!,
        checker,
        strategy,
        supply.synthCell,
      );
      const vType = mapTsType(
        typeArgs[1]!,
        checker,
        strategy,
        supply.synthCell,
      );
      let info = supply.synthCell
        ? lookupMapKV(supply.synthCell.synth, kType, vType)
        : undefined;
      if (!info && supply.synthCell) {
        cellRegisterMap(supply.synthCell, kType, vType);
        info = lookupMapKV(supply.synthCell.synth, kType, vType);
      }
      if (!info) {
        // register returns null only when K or V mangles to something that
        // isn't a valid Pantagruel identifier — typically because mapTsType
        // fell back to checker.typeToString for an unsupported TS construct.
        return {
          unsupported: `Map<${kType}, ${vType}>: key or value type cannot be mangled into a synthesized domain name`,
        };
      }
      const objExpr = translateBodyExpr(
        tsReceiver,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(objExpr)) {
        return objExpr;
      }
      const kExpr = translateBodyExpr(
        expr.arguments[0]!,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(kExpr)) {
        return kExpr;
      }
      return {
        expr: readMapThroughWrites(
          state,
          methodName as "get" | "has",
          info.names.rule,
          info.names.keyPred,
          info.names.domain,
          kType,
          bodyExpr(objExpr),
          bodyExpr(kExpr),
        ),
      };
    }

    // .includes(x) on Array / .has(x) on Set -> x in obj
    if (
      (methodName === "includes" || methodName === "has") &&
      expr.arguments.length === 1
    ) {
      const receiverType = checker.getTypeAtLocation(tsReceiver);
      const isArray =
        methodName === "includes" && checker.isArrayType(receiverType);
      const isSet = methodName === "has" && isSetType(receiverType);
      if (!isArray && !isSet) {
        return {
          unsupported:
            methodName === "includes"
              ? "non-array .includes()"
              : "non-Set .has()",
        };
      }
      const arg = translateBodyExpr(
        expr.arguments[0]!,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(arg)) {
        return arg;
      }
      const objRaw = translateBodyExpr(
        tsReceiver,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      const objExpr = rejectEffect(objRaw);
      if (isBodyUnsupported(objExpr)) {
        return objExpr;
      }
      // Stage A Set: receiver is a declared interface field. Route through
      // `readSetThroughWrites` so prior `.add`/`.delete`/`.clear` in the
      // same body are observed inline as `cond` over equality arms with
      // the pre-state membership as fallthrough. Degenerates cleanly to
      // `arg in receiver` when no staged entry exists.
      if (isSet) {
        const normalizedReceiver = unwrapExpression(tsReceiver);
        if (
          ts.isPropertyAccessExpression(normalizedReceiver) &&
          isInterfaceFieldAccess(normalizedReceiver, checker)
        ) {
          const innerObj = normalizedReceiver.expression;
          const rawFieldName = normalizedReceiver.name.text;
          const fieldName = qualifyFieldAccess(
            checker.getTypeAtLocation(innerObj),
            rawFieldName,
            checker,
            strategy,
            supply.synthCell,
          );
          if (fieldName !== null) {
            const ownerType = mapTsType(
              checker.getTypeAtLocation(innerObj),
              checker,
              strategy,
              supply.synthCell,
            );
            const typeArgs = checker.getTypeArguments(
              receiverType as ts.TypeReference,
            );
            const elemType =
              typeArgs.length === 1
                ? mapTsType(typeArgs[0]!, checker, strategy, supply.synthCell)
                : null;
            if (elemType !== null) {
              const innerObjRaw = translateBodyExpr(
                innerObj,
                checker,
                strategy,
                paramNames,
                state,
                supply,
              );
              const innerObjExpr = rejectEffect(innerObjRaw);
              if (isBodyUnsupported(innerObjExpr)) {
                return innerObjExpr;
              }
              return {
                expr: readSetThroughWrites(
                  state,
                  fieldName,
                  ownerType,
                  elemType,
                  bodyExpr(innerObjExpr),
                  bodyExpr(arg),
                ),
              };
            }
          }
        }
      }
      return { expr: ast.binop(ast.opIn(), bodyExpr(arg), bodyExpr(objExpr)) };
    }

    // .filter(pred) / .map(fn) — each independently produces or refines a comprehension
    if (
      (methodName === "filter" || methodName === "map") &&
      expr.arguments.length === 1
    ) {
      const result = translateArrayMethod(
        methodName,
        tsReceiver,
        expr,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (result) {
        return result;
      }
    }

    // .reduce(cb, init) / .reduceRight(cb, init) — fold into `over each` aggregate
    if (methodName === "reduce" || methodName === "reduceRight") {
      const result = translateReduceCall(
        methodName,
        tsReceiver,
        expr,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (result) {
        return result;
      }
    }

    // General method call: obj.method(args) → method obj args (EUF encoding)
    // Ref: Kroening & Strichman, Decision Procedures, Ch. 4
    //
    // Normalize the method name with the same kebab-casing applied to
    // declarations (mirrors the free-function branch below). A property
    // token is not a variable reference, so the callee is never resolved
    // through `paramNames`.
    if (expr.arguments.some(ts.isSpreadElement)) {
      return { unsupported: expr.getText() };
    }
    const receiver = translateBodyExpr(
      tsReceiver,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(receiver)) {
      return receiver;
    }
    const methodArgs: OpaqueExpr[] = [bodyExpr(receiver)];
    for (const arg of expr.arguments) {
      const a = translateBodyExpr(
        arg,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(a)) {
        return a;
      }
      methodArgs.push(bodyExpr(a));
    }
    return { expr: ast.app(ast.var(toPantTermName(methodName)), methodArgs) };
  }

  // Free function calls: fn(args) → fn args (EUF encoding)
  if (ts.isIdentifier(expr.expression)) {
    const fnName = expr.expression.text;

    if (expr.arguments.some(ts.isSpreadElement)) {
      return { unsupported: expr.getText() };
    }

    // Normalize the callee name with the same kebab-casing applied to
    // function declarations (see `baseName` at the top of
    // `translateBody`). Without this, a recursive or helper call like
    // `camelCase()` would emit `camelCase` while the declaration is
    // `camel-case`, producing an undeclared-rule reference.
    const calleeName = paramNames.get(fnName) ?? toPantTermName(fnName);

    // Zero-arity call → variable reference (EUF constant)
    if (expr.arguments.length === 0) {
      return { expr: ast.var(calleeName) };
    }

    const fnArgs: OpaqueExpr[] = [];
    for (const arg of expr.arguments) {
      const a = translateBodyExpr(
        arg,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(a)) {
        return a;
      }
      fnArgs.push(bodyExpr(a));
    }
    return { expr: ast.app(ast.var(calleeName), fnArgs) };
  }

  // Unsupported call (computed calls, tagged templates, optional calls, etc.)
  return { unsupported: expr.getText() };
}

function extractArrowBody(
  expr: ts.Expression,
  binderName: string,
  paramNames: ReadonlyMap<string, string>,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  supply: UniqueSupply,
): BodyResult | null {
  if (!ts.isArrowFunction(expr)) {
    return null;
  }
  if (
    expr.parameters.length !== 1 ||
    !ts.isIdentifier(expr.parameters[0]!.name)
  ) {
    return {
      unsupported:
        "filter/map callback must have exactly one identifier parameter",
    };
  }

  // Map arrow param to the fresh binder
  const param = expr.parameters[0]!;
  const arrowParams = new Map(paramNames);
  arrowParams.set((param.name as ts.Identifier).text, binderName);

  if (ts.isBlock(expr.body)) {
    // Only allow a single return (after filtering guards), same rule as
    // extractReturnExpression — blocks with locals or multiple statements
    // would introduce free variables in the generated comprehension.
    const nonGuard = expr.body.statements.filter(
      (s) => !isGuardStatement(s, checker),
    );
    if (nonGuard.length === 1) {
      const s = nonGuard[0]!;
      if (ts.isReturnStatement(s) && s.expression) {
        return translateBodyExpr(
          s.expression,
          checker,
          strategy,
          arrowParams,
          undefined,
          supply,
        );
      }
    }
    return null;
  }

  // Expression body
  return translateBodyExpr(
    expr.body,
    checker,
    strategy,
    arrowParams,
    undefined,
    supply,
  );
}

// --- Mutating function body translation ---

function translateMutatingBody(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  declarations: PantDeclaration[],
  synthCell?: SynthCell,
): PropResult[] {
  if (!node.body) {
    return [];
  }

  const ast = getAst();
  const propositions: PropResult[] = [];
  const state = makeSymbolicState();
  const supply = makeUniqueSupply(synthCell);

  const ok = symbolicExecute(
    node.body,
    checker,
    strategy,
    paramNames,
    state,
    propositions,
    (e) => e,
    supply,
  );

  // Only emit state equations + frames when the whole body was translatable;
  // partial emission would be unsound (frames would mask unhandled writes).
  if (!ok) {
    return propositions;
  }

  // Quantifier binders allocated through the document-wide name registry
  // (inside synthCell) so they don't collide with the function's own
  // params or with type-derived accessor rule binders — unlike the
  // internal `freshHygienicBinder` which emits `$N` names that don't
  // round-trip through the Pantagruel parser.
  const allocBinder = (hint: string): string =>
    synthCell ? cellRegisterName(synthCell, hint) : freshHygienicBinder(supply);
  for (const [, entry] of state.writes) {
    switch (entry.kind) {
      case "property":
        propositions.push({
          kind: "equation",
          quantifiers: [] as OpaqueParam[],
          lhs: ast.app(ast.primed(entry.prop), [entry.objExpr]),
          rhs: entry.value,
        });
        addModifiedProp(state, entry.prop);
        break;
      case "map":
        emitMapEquations(entry, propositions, allocBinder, state);
        break;
      case "set":
        emitSetMembershipEquation(entry, propositions, allocBinder, state);
        break;
      default: {
        // Exhaustiveness guard: `entry.kind` is discriminated across
        // property / map / set. If a new kind is added without updating
        // this switch, the `_exhaustive: never` assignment will fail to
        // compile — TS's standard exhaustive-switch pattern.
        const _exhaustive: never = entry;
        void _exhaustive;
      }
    }
  }

  const frames = generateFrameConditions(state.modifiedProps, declarations);
  propositions.push(...frames);

  return propositions;
}

/**
 * Forward symbolic execution with path merging (Dijkstra CACM 1975;
 * Allen POPL 1983 if-conversion). Updates `state.writes` for each property
 * assignment; merges `if`/`else` via `cond` at the join point. Returns
 * `false` when an unsupported construct was encountered; unsupported
 * markers are pushed into `propositions` for the caller to inspect.
 */
function symbolicExecute(
  body: ts.Block | ts.Statement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  state: SymbolicState,
  propositions: PropResult[],
  outerApply: (e: OpaqueExpr) => OpaqueExpr = (e) => e,
  supply: UniqueSupply = makeUniqueSupply(),
  insideBranch: boolean = false,
): boolean {
  const ast = getAst();
  let ok = true;
  let applyConst = outerApply;
  // Keep the state's canonicalize in sync with the frame's applyConst so
  // symbolic-state reads see the same normalization the write site uses.
  setCanonicalize(state, applyConst);
  const stmts = ts.isBlock(body) ? Array.from(body.statements) : [body];

  for (const [i, stmt] of stmts.entries()) {
    // Skip guard statements (if-throw patterns and assertion calls)
    if (isGuardStatement(stmt, checker)) {
      continue;
    }

    // Early-exit if-conversion (Allen et al., POPL 1983, extended to
    // early exits). Any `if` with a bare-return branch lifts the remaining
    // statements — plus the other branch's statements when present — into
    // a single continuation conditioned on the non-early-exit path.
    const exit = !insideBranch ? detectEarlyExit(stmt) : null;
    if (exit !== null) {
      if (expressionHasSideEffects(exit.condition, checker)) {
        ok = false;
        propositions.push({
          kind: "unsupported",
          reason: "impure if-condition in mutating body",
        });
        break;
      }
      const gResult = translateBodyExpr(
        exit.condition,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(gResult)) {
        ok = false;
        propositions.push({
          kind: "unsupported",
          reason: gResult.unsupported,
        });
        break;
      }
      const gExpr = applyConst(bodyExpr(gResult));

      // Continuation = (other-branch's stmts if any) ++ post-if stmts.
      // The continuation is the fall-through path at the same logical scope
      // as the current statement list, so preserve the caller's `insideBranch`
      // flag rather than forcing it. This lets a chain of top-level guards
      // like `if (g) return; if (h) return; a.balance = 1` flatten into
      // nested conds via successive if-conversion passes (Allen et al.,
      // POPL 1983); forcing `true` would instead reject the second guard as
      // `return in mutating branch`.
      const continuation = [...exit.continuationPrefix, ...stmts.slice(i + 1)];
      const sR = cloneSymbolicState(state);
      const remainingProps: PropResult[] = [];
      const continuationBlock = ts.factory.createBlock(continuation, true);
      const okR = symbolicExecute(
        continuationBlock,
        checker,
        strategy,
        new Map(paramNames),
        sR,
        remainingProps,
        applyConst,
        supply,
        insideBranch,
      );
      if (!okR) {
        ok = false;
        propositions.push(...remainingProps);
        break;
      }

      // Shape A loop equations emit directly into `propositions` rather than
      // flowing through `state.writes`, so they'd be silently dropped by the
      // merge-only path below. Reject when the continuation produced such
      // equations — we'd need to thread `gExpr` into each rhs (and reconcile
      // `state.modifiedProps`) to preserve the early-exit semantics, which
      // isn't yet implemented.
      const directEquations = remainingProps.filter(
        (p) => p.kind === "equation",
      );
      if (directEquations.length > 0) {
        ok = false;
        propositions.push({
          kind: "unsupported",
          reason:
            "loop with per-iteration writes cannot appear after an early-exit guard",
        });
        break;
      }

      // Merge: for each key touched by the continuation, emit a cond
      // selecting the pre-state value when we take the early exit and
      // the continuation's value otherwise. `earlyExitWhenTrue` picks
      // which arm of the cond the condition guards.
      for (const key of sR.writtenKeys) {
        const entryR = sR.writes.get(key)!;
        const prior = state.writes.get(key);
        if (prior !== undefined && prior.kind !== entryR.kind) {
          ok = false;
          propositions.push({
            kind: "unsupported",
            reason:
              "branches wrote the same key with different kinds (property / map / set mismatch)",
          });
          break;
        }
        if (entryR.kind === "property") {
          const priorP = prior as PropertyWriteEntry | undefined;
          const identity = ast.app(ast.var(entryR.prop), [entryR.objExpr]);
          const vEarlyReturn = priorP?.value ?? identity;
          const vContinuation = entryR.value;
          const merged = exit.earlyExitWhenTrue
            ? ast.cond([
                [gExpr, vEarlyReturn],
                [ast.litBool(true), vContinuation],
              ])
            : ast.cond([
                [gExpr, vContinuation],
                [ast.litBool(true), vEarlyReturn],
              ]);
          putWrite(state, key, {
            kind: "property",
            prop: entryR.prop,
            objExpr: entryR.objExpr,
            value: merged,
          });
          addWrittenKey(state, key);
          continue;
        }
        // Map / Set: continuation-side overrides are taken only when NOT
        // on the early-exit arm. The early-exit fallback for each
        // override is the accumulated outer-state value if the outer
        // state had a prior write there, else the pre-state lookup — so
        // outer writes persist across the exit arm.
        const combineContCond = (
          vCont: OpaqueExpr,
          vExit: OpaqueExpr,
        ): OpaqueExpr =>
          exit.earlyExitWhenTrue
            ? ast.cond([
                [gExpr, vExit],
                [ast.litBool(true), vCont],
              ])
            : ast.cond([
                [gExpr, vCont],
                [ast.litBool(true), vExit],
              ]);
        if (entryR.kind === "map") {
          const priorMap =
            prior !== undefined && prior.kind === "map" ? prior : undefined;
          const ruleVar = ast.var(entryR.ruleName);
          const keyVar = ast.var(entryR.keyPredName);
          const valueFallback = (o: MapOverride): OpaqueExpr =>
            ast.app(ruleVar, [o.objExpr, o.keyExpr]);
          const memberFallback = (o: MapOverride): OpaqueExpr =>
            ast.app(keyVar, [o.objExpr, o.keyExpr]);
          const mergedValue = mergeOverrides(
            entryR.valueOverrides,
            priorMap?.valueOverrides ?? [],
            (o) => o.keyTuple,
            valueFallback,
            combineContCond,
          );
          const mergedMember = mergeOverrides(
            entryR.membershipOverrides,
            priorMap?.membershipOverrides ?? [],
            (o) => o.keyTuple,
            memberFallback,
            combineContCond,
          );
          putWrite(state, key, {
            kind: "map",
            ruleName: entryR.ruleName,
            keyPredName: entryR.keyPredName,
            ownerType: entryR.ownerType,
            keyType: entryR.keyType,
            valueOverrides: mergedValue,
            membershipOverrides: mergedMember,
          });
          addWrittenKey(state, key);
          continue;
        }
        // Set: fallback is the pre-state membership `y in s` at the
        // canonical receiver + override element. `cleared` is inherited
        // from the continuation — an early-exit path with an outer-state
        // clear can't re-establish pre-state membership, so we leave the
        // flag as continuation's.
        const priorSet =
          prior !== undefined && prior.kind === "set" ? prior : undefined;
        const setRuleVar = ast.var(entryR.ruleName);
        const setFallback = (o: SetOverride): OpaqueExpr =>
          ast.binop(
            ast.opIn(),
            o.elemExpr,
            ast.app(setRuleVar, [entryR.objExpr]),
          );
        const mergedSet = mergeOverrides(
          entryR.memberOverrides,
          priorSet?.memberOverrides ?? [],
          (o) => o.elemExpr,
          setFallback,
          combineContCond,
        );
        putWrite(state, key, {
          kind: "set",
          ruleName: entryR.ruleName,
          ownerType: entryR.ownerType,
          elemType: entryR.elemType,
          objExpr: entryR.objExpr,
          memberOverrides: mergedSet,
          cleared: entryR.cleared || (priorSet?.cleared ?? false),
        });
        addWrittenKey(state, key);
      }
      // Remaining stmts have been consumed by the continuation.
      break;
    }

    // Handle const bindings via shared inlineConstBindings
    if (ts.isVariableStatement(stmt)) {
      const declList = stmt.declarationList;
      if (declList.flags & ts.NodeFlags.Const) {
        const bindings: ConstBinding[] = [];
        let allPure = true;
        for (const decl of declList.declarations) {
          if (
            !ts.isIdentifier(decl.name) ||
            !decl.initializer ||
            expressionHasSideEffects(decl.initializer, checker)
          ) {
            allPure = false;
            break;
          }
          bindings.push({
            kind: "const",
            tsName: decl.name.text,
            initializer: decl.initializer,
          });
        }
        if (allPure) {
          const inlined = inlineConstBindings(
            bindings,
            checker,
            strategy,
            paramNames,
            supply,
            state,
          );
          if ("error" in inlined) {
            ok = false;
            propositions.push({
              kind: "unsupported",
              reason: inlined.error,
            });
          } else {
            for (const [key, value] of inlined.scopedParams) {
              paramNames.set(key, value);
            }
            const prevApply = applyConst;
            applyConst = (e) => prevApply(inlined.applyTo(e));
            setCanonicalize(state, applyConst);
          }
          continue;
        }
      }
      ok = false;
      propositions.push({
        kind: "unsupported",
        reason: "local variable declaration (let/var or effectful const)",
      });
      continue;
    }

    // Map mutation: `m.set(k, v)` or `m.delete(k)` as a statement.
    // translateCallExpr returns a `{ effect }` result for these calls; the
    // write is installed via installMapWrite, which coalesces multiple
    // writes to the same rule into one MapRuleWriteEntry.
    if (
      ts.isExpressionStatement(stmt) &&
      ts.isCallExpression(unwrapExpression(stmt.expression))
    ) {
      const call = unwrapExpression(stmt.expression) as ts.CallExpression;
      const callResult = translateCallExpr(
        call,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyEffect(callResult)) {
        if (isMapEffect(callResult.effect)) {
          installMapWrite(state, callResult.effect, applyConst);
        } else {
          installSetWrite(state, callResult.effect, applyConst);
        }
        continue;
      }
      // If the call *looks like* a Map.set/Map.delete or a
      // Set.add/Set.delete/Set.clear (right method name, arity, and
      // receiver type) but translateCallExpr returned an unsupported
      // marker, propagate the specific reason rather than falling through
      // to the generic "side-effectful expression" error. Other
      // unsupported results (non-Map/Set calls the EUF encoder rejected
      // because of an arrow-function argument, etc.) still fall through
      // so the forEach handler below gets its chance.
      if (
        isBodyUnsupported(callResult) &&
        ts.isPropertyAccessExpression(call.expression)
      ) {
        const mName = call.expression.name.text;
        const argc = call.arguments.length;
        const recvType = checker.getTypeAtLocation(call.expression.expression);
        const looksLikeMapMutation =
          ((mName === "set" && argc === 2) ||
            (mName === "delete" && argc === 1)) &&
          isMapType(recvType);
        const looksLikeSetMutation =
          ((mName === "add" && argc === 1) ||
            (mName === "delete" && argc === 1) ||
            (mName === "clear" && argc === 0)) &&
          isSetType(recvType);
        if (looksLikeMapMutation || looksLikeSetMutation) {
          ok = false;
          propositions.push({
            kind: "unsupported",
            reason: callResult.unsupported,
          });
          continue;
        }
      }
      // Otherwise fall through to forEach / side-effect handling below.
    }

    // Property assignment: obj.prop = rhs
    if (
      ts.isExpressionStatement(stmt) &&
      ts.isBinaryExpression(unwrapExpression(stmt.expression))
    ) {
      const bin = unwrapExpression(stmt.expression) as ts.BinaryExpression;
      const compoundOp = COMPOUND_ASSIGN_TO_BINOP.get(bin.operatorToken.kind);
      const isSimpleAssign =
        bin.operatorToken.kind === ts.SyntaxKind.EqualsToken;
      if (
        (isSimpleAssign || compoundOp !== undefined) &&
        ts.isPropertyAccessExpression(bin.left)
      ) {
        const rawProp = bin.left.name.text;
        const receiverType = checker.getTypeAtLocation(bin.left.expression);
        // Qualify once at the write site; the state keys, primed-lhs
        // emission, modifiedProps tracking, and frame conditions all see
        // the same already-qualified rule name.
        const prop = qualifyFieldAccess(
          receiverType,
          rawProp,
          checker,
          strategy,
          supply.synthCell,
        );
        if (prop === null) {
          ok = false;
          propositions.push({
            kind: "unsupported",
            reason: ambiguousFieldMsg(rawProp),
          });
          continue;
        }
        const obj = translateBodyExpr(
          bin.left.expression,
          checker,
          strategy,
          paramNames,
          state,
          supply,
        );
        if (isBodyUnsupported(obj)) {
          ok = false;
          propositions.push({ kind: "unsupported", reason: obj.unsupported });
          continue;
        }
        // For compound assignment `a.p OP= v`, desugar rhs to `a.p OP v`.
        // The rhs's `a.p` read goes through translateBodyExpr, which
        // consults the symbolic state and returns the prior-write value
        // or the pre-state identity.
        const rhsNode =
          compoundOp !== undefined
            ? ts.factory.createBinaryExpression(bin.left, compoundOp, bin.right)
            : bin.right;
        const val = translateBodyExpr(
          rhsNode,
          checker,
          strategy,
          paramNames,
          state,
          supply,
        );
        if (isBodyUnsupported(val)) {
          ok = false;
          propositions.push({ kind: "unsupported", reason: val.unsupported });
          continue;
        }
        const objExpr = applyConst(bodyExpr(obj));
        const valExpr = applyConst(bodyExpr(val));
        const key = symbolicKey(prop, objExpr);
        putWrite(state, key, {
          kind: "property",
          prop,
          objExpr,
          value: valExpr,
        });
        addWrittenKey(state, key);
        continue;
      }
    }

    // `arr.forEach(x => { ... })` — structurally equivalent to `for-of` when
    // used as a statement. Dispatch to the same loop-body translator.
    if (
      ts.isExpressionStatement(stmt) &&
      ts.isCallExpression(unwrapExpression(stmt.expression)) &&
      !insideBranch
    ) {
      const call = unwrapExpression(stmt.expression) as ts.CallExpression;
      const okF = translateForEachStmt(
        call,
        checker,
        strategy,
        paramNames,
        state,
        propositions,
        applyConst,
        supply,
      );
      if (okF !== null) {
        if (!okF) {
          ok = false;
        }
        continue;
      }
    }

    if (
      ts.isExpressionStatement(stmt) &&
      expressionHasSideEffects(stmt.expression, checker)
    ) {
      propositions.push({
        kind: "unsupported",
        reason: "side-effectful expression",
      });
      ok = false;
      continue;
    }

    if (
      ts.isVariableStatement(stmt) &&
      stmt.declarationList.declarations.some(
        (d) =>
          d.initializer && expressionHasSideEffects(d.initializer, checker),
      )
    ) {
      propositions.push({
        kind: "unsupported",
        reason: "side-effectful variable initializer",
      });
      ok = false;
      continue;
    }

    if (
      (ts.isReturnStatement(stmt) || ts.isThrowStatement(stmt)) &&
      stmt.expression &&
      expressionHasSideEffects(stmt.expression, checker)
    ) {
      propositions.push({
        kind: "unsupported",
        reason: "side-effectful control-flow expression",
      });
      ok = false;
      continue;
    }

    // Bare `return;` at top level is a no-op (void function). Inside a
    // branch it's unsound — symbolic execution assumes each branch reaches
    // the join point with a well-defined state. Early exit would leave
    // later writes conditionally unreachable, which the merge cannot encode.
    if (ts.isReturnStatement(stmt) && !stmt.expression) {
      if (insideBranch) {
        propositions.push({
          kind: "unsupported",
          reason: "return in mutating branch",
        });
        ok = false;
      }
      continue;
    }

    // `return expr;` or `throw;` always break the path-merging model.
    if (ts.isReturnStatement(stmt) || ts.isThrowStatement(stmt)) {
      propositions.push({
        kind: "unsupported",
        reason: "return/throw in mutating body",
      });
      ok = false;
      continue;
    }

    // Nested block — flow state through sequentially
    if (ts.isBlock(stmt)) {
      const inner = symbolicExecute(
        stmt,
        checker,
        strategy,
        paramNames,
        state,
        propositions,
        applyConst,
        supply,
        insideBranch,
      );
      if (!inner) {
        ok = false;
      }
      continue;
    }

    // Conditional mutation: path merging via cond
    if (ts.isIfStatement(stmt)) {
      if (expressionHasSideEffects(stmt.expression, checker)) {
        ok = false;
        propositions.push({
          kind: "unsupported",
          reason: "impure if-condition in mutating body",
        });
        continue;
      }
      const gResult = translateBodyExpr(
        stmt.expression,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(gResult)) {
        ok = false;
        propositions.push({
          kind: "unsupported",
          reason: gResult.unsupported,
        });
        continue;
      }
      const gExpr = applyConst(bodyExpr(gResult));

      const sT = cloneSymbolicState(state);
      const thenProps: PropResult[] = [];
      const okT = symbolicExecute(
        stmt.thenStatement,
        checker,
        strategy,
        new Map(paramNames),
        sT,
        thenProps,
        applyConst,
        supply,
        true,
      );
      if (!okT) {
        ok = false;
        propositions.push(...thenProps);
        continue;
      }

      const sE = cloneSymbolicState(state);
      const elseProps: PropResult[] = [];
      if (stmt.elseStatement) {
        const okE = symbolicExecute(
          stmt.elseStatement,
          checker,
          strategy,
          new Map(paramNames),
          sE,
          elseProps,
          applyConst,
          supply,
          true,
        );
        if (!okE) {
          ok = false;
          propositions.push(...elseProps);
          continue;
        }
      }

      // Merge touched keys via cond(g => vT, true => vE)
      const touched = new Set<string>([...sT.writtenKeys, ...sE.writtenKeys]);
      let mergeOk = true;
      for (const key of touched) {
        const entryT = sT.writes.get(key);
        const entryE = sE.writes.get(key);
        // At least one branch wrote the key, so at least one entry exists.
        const pick = (entryT ?? entryE)!;
        if (entryT && entryE && entryT.kind !== entryE.kind) {
          ok = false;
          mergeOk = false;
          propositions.push({
            kind: "unsupported",
            reason:
              "branches wrote the same key with different kinds (property / map / set mismatch)",
          });
          break;
        }
        if (pick.kind === "property") {
          const entryTP = entryT as PropertyWriteEntry | undefined;
          const entryEP = entryE as PropertyWriteEntry | undefined;
          const objExpr = pick.objExpr;
          const prop = pick.prop;
          const identity = ast.app(ast.var(prop), [objExpr]);
          const vT = entryTP?.value ?? identity;
          const vE = entryEP?.value ?? identity;
          const merged = ast.cond([
            [gExpr, vT],
            [ast.litBool(true), vE],
          ]);
          putWrite(state, key, {
            kind: "property",
            prop,
            objExpr,
            value: merged,
          });
          addWrittenKey(state, key);
          continue;
        }
        const combineCond = (vA: OpaqueExpr, vB: OpaqueExpr): OpaqueExpr =>
          ast.cond([
            [gExpr, vA],
            [ast.litBool(true), vB],
          ]);
        if (pick.kind === "map") {
          // Map: merge per-override-pair under gExpr. Identity fallbacks
          // per side use the pre-state lookup at the override's (m, k)
          // tuple.
          const entryTM = entryT as MapRuleWriteEntry | undefined;
          const entryEM = entryE as MapRuleWriteEntry | undefined;
          const base = entryTM ?? entryEM!;
          const ruleVar = ast.var(base.ruleName);
          const keyVar = ast.var(base.keyPredName);
          const valueFallback = (o: MapOverride): OpaqueExpr =>
            ast.app(ruleVar, [o.objExpr, o.keyExpr]);
          const memberFallback = (o: MapOverride): OpaqueExpr =>
            ast.app(keyVar, [o.objExpr, o.keyExpr]);
          const mergedValue = mergeOverrides(
            entryTM?.valueOverrides ?? [],
            entryEM?.valueOverrides ?? [],
            (o) => o.keyTuple,
            valueFallback,
            combineCond,
          );
          const mergedMember = mergeOverrides(
            entryTM?.membershipOverrides ?? [],
            entryEM?.membershipOverrides ?? [],
            (o) => o.keyTuple,
            memberFallback,
            combineCond,
          );
          putWrite(state, key, {
            kind: "map",
            ruleName: base.ruleName,
            keyPredName: base.keyPredName,
            ownerType: base.ownerType,
            keyType: base.keyType,
            valueOverrides: mergedValue,
            membershipOverrides: mergedMember,
          });
          addWrittenKey(state, key);
          continue;
        }
        // Set: merge per-element under gExpr. Fallback is the pre-state
        // `y in s` at the canonical receiver (both branches of a Set
        // merge reference the same interface field, so `objExpr` is
        // structurally identical — pick either).
        const entryTS = entryT as SetRuleWriteEntry | undefined;
        const entryES = entryE as SetRuleWriteEntry | undefined;
        const baseS = entryTS ?? entryES!;
        const setRuleVar = ast.var(baseS.ruleName);
        const setFallback = (o: SetOverride): OpaqueExpr =>
          ast.binop(
            ast.opIn(),
            o.elemExpr,
            ast.app(setRuleVar, [baseS.objExpr]),
          );
        const mergedSet = mergeOverrides(
          entryTS?.memberOverrides ?? [],
          entryES?.memberOverrides ?? [],
          (o) => o.elemExpr,
          setFallback,
          combineCond,
        );
        putWrite(state, key, {
          kind: "set",
          ruleName: baseS.ruleName,
          ownerType: baseS.ownerType,
          elemType: baseS.elemType,
          objExpr: baseS.objExpr,
          memberOverrides: mergedSet,
          cleared: (entryTS?.cleared ?? false) || (entryES?.cleared ?? false),
        });
        addWrittenKey(state, key);
      }
      if (!mergeOk) {
        continue;
      }
      continue;
    }

    if (ts.isForOfStatement(stmt) && !insideBranch) {
      const okL = translateForOfLoop(
        stmt,
        checker,
        strategy,
        paramNames,
        state,
        propositions,
        applyConst,
        supply,
      );
      if (!okL) {
        ok = false;
      }
      continue;
    }

    if (
      ts.isForStatement(stmt) ||
      ts.isForOfStatement(stmt) ||
      ts.isForInStatement(stmt) ||
      ts.isWhileStatement(stmt) ||
      ts.isDoStatement(stmt)
    ) {
      propositions.push({ kind: "unsupported", reason: "loop assignment" });
      ok = false;
      continue;
    }

    if (ts.isTryStatement(stmt)) {
      if (stmt.catchClause) {
        propositions.push({
          kind: "unsupported",
          reason: "try/catch assignment",
        });
        ok = false;
      } else {
        const inner = symbolicExecute(
          stmt.tryBlock,
          checker,
          strategy,
          paramNames,
          state,
          propositions,
          applyConst,
          supply,
          insideBranch,
        );
        if (!inner) {
          ok = false;
        }
      }
      if (stmt.finallyBlock) {
        const inner = symbolicExecute(
          stmt.finallyBlock,
          checker,
          strategy,
          paramNames,
          state,
          propositions,
          applyConst,
          supply,
          insideBranch,
        );
        if (!inner) {
          ok = false;
        }
      }
      continue;
    }

    if (ts.isSwitchStatement(stmt)) {
      propositions.push({ kind: "unsupported", reason: "switch assignment" });
      ok = false;
    }
  }

  return ok;
}

/**
 * Generate frame conditions: for each rule in declarations not explicitly
 * modified, emit `rule' x = rule x` using the rule's own parameter names
 * as free variable references. Pantagruel's SMT translator auto-quantifies
 * free rule-param references in action-body propositions via
 * [bind_head_params] (see `lib/smt_doc.ml`), so the emission stays flat.
 * This also preserves declaration guards through pant's guard-injection
 * machinery on the quantified form, which would be lost if ts2pant
 * pre-wrapped with a local `all` here.
 */
function generateFrameConditions(
  modifiedRules: Set<string>,
  declarations: PantDeclaration[],
): PropResult[] {
  const ast = getAst();
  const frames: PropResult[] = [];

  for (const decl of declarations) {
    if (decl.kind !== "rule") {
      continue;
    }
    if (modifiedRules.has(decl.name)) {
      continue;
    }

    const paramArgs = decl.params.map((p) => ast.var(p.name));
    const lhs = ast.app(ast.primed(decl.name), paramArgs);
    const rhs = ast.app(ast.var(decl.name), paramArgs);
    frames.push({
      kind: "equation",
      quantifiers: [] as OpaqueParam[],
      lhs,
      rhs,
    });
  }

  return frames;
}
