import type { SourceFile } from "ts-morph";
import ts from "typescript";
import { type IRExpr, irLet, irWrap } from "./ir.js";
import { buildIR, isBuildUnsupported } from "./ir-build.js";
import { lowerExpr } from "./ir-emit.js";
import { type IR1Expr, ir1Binop, ir1FromL2, ir1IsNullish } from "./ir1.js";
import {
  buildL1Conditional,
  buildL1LetWhile,
  buildL1MemberAccess,
  isL1ConditionalForm,
  isL1StmtUnsupported,
  isL1Unsupported,
  lowerL1ToOpaque,
  tryBuildL1Cardinality,
  tryBuildL1PureSubExpression,
  tryRecognizeFunctorLift,
} from "./ir1-build.js";
import {
  buildL1AssignStmt,
  buildL1ForEachCall,
  buildL1ForOfMutation,
  buildL1IfMutation,
  isUnsupported,
} from "./ir1-build-body.js";
import { lowerL1MuSearch, type MuSearchLowerCtx } from "./ir1-lower.js";
import { lowerL1Body } from "./ir1-lower-body.js";
import {
  getOperandDeclaredType,
  type NullishTranslate,
  recognizeNullishForm,
} from "./nullish-recognizer.js";
import type {
  OpaqueCombiner,
  OpaqueExpr,
  OpaqueGuard,
  OpaqueParam,
} from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import { isKnownPureCall, isStaticallyBoolTyped } from "./purity.js";
import { translateRecordReturn } from "./translate-record.js";
import {
  classifyFunction,
  containsUnsupportedOperator,
  findFunction,
  isAssertionCall,
  isFollowableGuardCall,
  isPureExpression,
  isTranslateExprUnsupported,
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
  isUnsupportedUnknown,
  lookupMapKV,
  mapTsType,
  type NumericStrategy,
  resolveFieldOwner,
  type SynthCell,
  toPantTermName,
  UNSUPPORTED_UNKNOWN_REASON,
} from "./translate-types.js";
import type { PantDeclaration, PropResult } from "./types.js";

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
 * Allocate a parser-roundtrippable comprehension binder through
 * `cellRegisterName`, so the name is collision-safe against the
 * document-wide `NameRegistry` and emits as a real Pant identifier
 * (`hint`, `hint1`, …).
 *
 * Use this — not `freshHygienicBinder` directly — at any site that emits
 * a binder into a quantifier or comprehension that survives to Pant text.
 * See PR #84 post-mortem in `CLAUDE.md` for the bug class this prevents.
 */
export function allocComprehensionBinder(
  supply: UniqueSupply,
  hint: string,
): string {
  if (!supply.synthCell) {
    throw new Error(
      "allocComprehensionBinder requires synthCell for emitted binders",
    );
  }
  return cellRegisterName(supply.synthCell, hint);
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
 *     See `recognizeLetWhilePair` and `emitMuSearch`.
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

export interface MuSearch {
  counterName: string;
  initTsExpr: ts.Expression;
  predicateTsExpr: ts.Expression;
  /**
   * The expression-statement body of the while loop — the increment
   * step. Used by the L1 builder to construct the canonical
   * `Block([Let, While(_, Assign)])` form for `isCanonicalMuSearchForm`
   * to pattern-match against.
   */
  stepExpr: ts.Expression;
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
export interface MapMutation {
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
export interface SetMutation {
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
export function rejectEffect(
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

export interface PropertyWriteEntry {
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
export interface MapOverride {
  keyTuple: OpaqueExpr;
  objExpr: OpaqueExpr;
  keyExpr: OpaqueExpr;
  value: OpaqueExpr;
}

export interface MapRuleWriteEntry {
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
export interface SetOverride {
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
 * `cleared` is a Bool-valued symbolic predicate: literal `false` means no
 * `.clear()` is in effect (membership falls through to pre-state); literal
 * `true` means an unconditional clear (membership for any element not in
 * `memberOverrides` is `false`); a non-literal expression is a guarded
 * clear from a branch merge — under the symbolic predicate, membership
 * is `false`; otherwise pre-state. Encoded as `OpaqueExpr` rather than
 * boolean so `if (g) s.clear()` can express "cleared on path g" without
 * collapsing the merge to an unconditional clear.
 *
 * `objExpr` is the canonicalized receiver; the emission quantifier runs
 * only over the element (type `elemType`), while the LHS applies the
 * primed rule to this receiver as a free term.
 */
export interface SetRuleWriteEntry {
  kind: "set";
  ruleName: string;
  ownerType: string;
  elemType: string;
  objExpr: OpaqueExpr;
  memberOverrides: SetOverride[];
  cleared: OpaqueExpr;
}

export type WriteEntry =
  | PropertyWriteEntry
  | MapRuleWriteEntry
  | SetRuleWriteEntry;

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
  modifiedProps: ReadonlySet<string>;
  canonicalize: (e: OpaqueExpr) => OpaqueExpr;
}

export function makeSymbolicState(
  canonicalize: (e: OpaqueExpr) => OpaqueExpr = (e) => e,
): SymbolicState {
  return {
    writes: new Map(),
    writtenKeys: new Set(),
    modifiedProps: new Set(),
    canonicalize,
  };
}

export function cloneSymbolicState(s: SymbolicState): SymbolicState {
  return {
    writes: new Map(s.writes),
    writtenKeys: new Set(),
    // Sub-states get their own modifiedProps; merge logic is responsible
    // for accumulating sub-state modifiedProps into the outer state at
    // branch-merge time. Previously this was a shared mutable Set, but
    // shared mutation across cloned cells doesn't dogfood well — the
    // primed-rule semantics for an aliased Set is unclear and ts2pant
    // can't translate it. Explicit accumulation is the cleaner answer.
    modifiedProps: new Set(s.modifiedProps),
    canonicalize: s.canonicalize,
  };
}

/**
 * Pure-style helpers: take the input collection, return a new
 * collection with the element added. Callers reassign the state field:
 *
 *   state.writes = putWrite(state.writes, key, entry);
 *   state.writtenKeys = addWrittenKey(state.writtenKeys, key);
 *   state.modifiedProps = addModifiedProp(state.modifiedProps, prop);
 *
 * Pure inputs/outputs translate cleanly to Pantagruel rules
 * (`putWrite m k v = …`) — easier to dogfood than the prior
 * mutate-the-state-field shape.
 */

export function putWrite(
  writes: ReadonlyMap<string, WriteEntry>,
  key: string,
  entry: WriteEntry,
): ReadonlyMap<string, WriteEntry> {
  const next = new Map(writes);
  next.set(key, entry);
  return next;
}

export function addWrittenKey(
  keys: ReadonlySet<string>,
  key: string,
): ReadonlySet<string> {
  const next = new Set(keys);
  next.add(key);
  return next;
}

export function addModifiedProp(
  props: ReadonlySet<string>,
  prop: string,
): ReadonlySet<string> {
  const next = new Set(props);
  next.add(prop);
  return next;
}

function setCanonicalize(
  state: SymbolicState,
  fn: (e: OpaqueExpr) => OpaqueExpr,
): void {
  state.canonicalize = fn;
}

export function symbolicKey(prop: string, objExpr: OpaqueExpr): string {
  return `${prop}::${getAst().strExpr(objExpr)}`;
}

/**
 * Canonical Bool-literal `false` expression and a static-equality probe.
 * Used to short-circuit Set `cleared` flows when the predicate is provably
 * `litBool(false)` — i.e., no clear has happened — so existing snapshots
 * for plain pre-state membership reads are unchanged.
 */
function setClearedFalse(): OpaqueExpr {
  return getAst().litBool(false);
}

function isStaticBoolLit(e: OpaqueExpr, value: boolean): boolean {
  const ast = getAst();
  return ast.strExpr(e) === ast.strExpr(ast.litBool(value));
}

/**
 * "If `cleared`, false; otherwise pre-state membership." Used by both the
 * Set read path and the Set emission path to project a symbolic clear
 * predicate over a pre-state membership expression.
 *
 * Folds when `cleared` is statically `false` (returns `pre`) or `true`
 * (returns literal `false`) so unconditional and never-cleared cases keep
 * their existing emission shape; a non-literal `cleared` (a guarded clear
 * from a branch merge) emits `cond cleared => false, true => pre`.
 */
export function clearedFallback(
  cleared: OpaqueExpr,
  pre: OpaqueExpr,
): OpaqueExpr {
  const ast = getAst();
  if (isStaticBoolLit(cleared, false)) {
    return pre;
  }
  if (isStaticBoolLit(cleared, true)) {
    return ast.litBool(false);
  }
  return ast.cond([
    [cleared, ast.litBool(false)],
    [ast.litBool(true), pre],
  ]);
}

/**
 * Combine branch-local `cleared` predicates at a merge point. Folds the
 * statically-known shapes so emission stays compact:
 *
 * - both branches agree → that expression (no information from gExpr)
 * - then-only clear (`true`/`false`) → `gExpr`
 * - else-only clear (`false`/`true`) → `~gExpr`
 * - mixed → `cond gExpr => tCleared, true => eCleared`
 */
export function mergeClearedCond(
  gExpr: OpaqueExpr,
  tCleared: OpaqueExpr,
  eCleared: OpaqueExpr,
): OpaqueExpr {
  const ast = getAst();
  if (ast.strExpr(tCleared) === ast.strExpr(eCleared)) {
    return tCleared;
  }
  if (isStaticBoolLit(tCleared, true) && isStaticBoolLit(eCleared, false)) {
    return gExpr;
  }
  if (isStaticBoolLit(tCleared, false) && isStaticBoolLit(eCleared, true)) {
    return ast.unop(ast.opNot(), gExpr);
  }
  return ast.cond([
    [gExpr, tCleared],
    [ast.litBool(true), eCleared],
  ]);
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
 * `combine`.
 *
 * For overrides that only exist in one side, the caller supplies what
 * the *other* side projects for that key. Map merges (where both sides
 * fall through to the same pre-state lookup `R m k` / `Rkey m k`) pass
 * one symmetric `fallback`. Set merges need asymmetric fallbacks because
 * each side may have its own `cleared` predicate: an a-side-only
 * override's b-fallback is `clearedFallback(b.cleared, baseIn)` and a
 * b-side-only override's a-fallback uses `a.cleared` instead. The
 * second positional `fallback` defaults to the first to preserve the
 * symmetric Map call sites unchanged.
 *
 * Order is preserved from `aSide` first, then any `bSide` keys not already
 * seen. The canonical form comes from `ast.strExpr(keyOf(o))`. Generic over
 * the override record so Map (tuple-keyed) and Set (element-keyed) writes
 * share the same merge plumbing.
 */
export function mergeOverrides<O extends { value: OpaqueExpr }>(
  aSide: ReadonlyArray<O>,
  bSide: ReadonlyArray<O>,
  keyOf: (o: O) => OpaqueExpr,
  fallbackForMissingB: (o: O) => OpaqueExpr,
  combine: (vA: OpaqueExpr, vB: OpaqueExpr) => OpaqueExpr,
  fallbackForMissingA: (o: O) => OpaqueExpr = fallbackForMissingB,
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
    const vB = bByKey.get(k) ?? fallbackForMissingB(o);
    out.push({ ...o, value: combine(o.value, vB) });
  }
  for (const o of bSide) {
    const k = canonical(keyOf(o));
    if (seen.has(k)) {
      continue;
    }
    const vA = fallbackForMissingA(o);
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
export function installMapWrite(
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
  state.writes = putWrite(state.writes, key, entry);
  state.writtenKeys = addWrittenKey(state.writtenKeys, key);
  state.modifiedProps = addModifiedProp(state.modifiedProps, effect.ruleName);
  state.modifiedProps = addModifiedProp(
    state.modifiedProps,
    effect.keyPredName,
  );
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
 * State-map key for Set writes. Unlike Map (whose `MapOverride`s carry
 * per-override `objExpr`/`keyExpr`), `SetRuleWriteEntry` stores a
 * single receiver at the entry level — the membership equation
 * `all y | y in tags' c <-> …` quantifies only over the element. So
 * the key has to include the canonicalized receiver, otherwise two
 * mutations on different receivers (e.g., `c1.tags.add(x);
 * c2.tags.add(y);`) would coalesce into a single entry that emits
 * exactly one equation and silently drops the other receiver's
 * constraint. Parallel to `mapWriteKey` (which includes only rule
 * identity since per-receiver fan-out is encoded in the override
 * tuples).
 */
function setWriteKey(
  ruleName: string,
  ownerType: string,
  elemType: string,
  objExpr: OpaqueExpr,
): string {
  return `set::${ruleName}::${ownerType}::${elemType}::${getAst().strExpr(objExpr)}`;
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
export function installSetWrite(
  state: SymbolicState,
  effect: SetMutation,
  applyConst: (e: OpaqueExpr) => OpaqueExpr,
): void {
  const ast = getAst();
  const objExpr = applyConst(effect.objExpr);
  // Canonicalize the receiver before keying so `const x = c; x.tags.add`
  // and `c.tags.add` resolve to the same entry, the same way property
  // writes canonicalize through `state.canonicalize`. Two genuinely
  // distinct receivers fan out into separate entries, each emitting
  // their own membership equation.
  const key = setWriteKey(
    effect.ruleName,
    effect.ownerType,
    effect.elemType,
    state.canonicalize(objExpr),
  );

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
          cleared: setClearedFalse(),
        };

  if (effect.op === "clear") {
    entry.memberOverrides = [];
    entry.cleared = ast.litBool(true);
    state.writes = putWrite(state.writes, key, entry);
    state.writtenKeys = addWrittenKey(state.writtenKeys, key);
    state.modifiedProps = addModifiedProp(state.modifiedProps, effect.ruleName);
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
  state.writes = putWrite(state.writes, key, entry);
  state.writtenKeys = addWrittenKey(state.writtenKeys, key);
  state.modifiedProps = addModifiedProp(state.modifiedProps, effect.ruleName);
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
  const entry = state.writes.get(
    setWriteKey(ruleName, ownerType, elemType, canonObj),
  );
  if (entry === undefined || entry.kind !== "set") {
    return baseIn(canonObj, canonQuery);
  }
  const preState = baseIn(canonObj, canonQuery);
  const tail = clearedFallback(entry.cleared, preState);
  if (entry.memberOverrides.length === 0) {
    return tail;
  }
  return ast.cond([
    ...entry.memberOverrides.map(
      (o) =>
        [ast.binop(ast.opEq(), canonQuery, o.elemExpr), o.value] as [
          OpaqueExpr,
          OpaqueExpr,
        ],
    ),
    [ast.litBool(true), tail],
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
    state.modifiedProps = addModifiedProp(state.modifiedProps, entry.ruleName);
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
    state.modifiedProps = addModifiedProp(
      state.modifiedProps,
      entry.keyPredName,
    );
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
  if (
    entry.memberOverrides.length === 0 &&
    isStaticBoolLit(entry.cleared, false)
  ) {
    return;
  }
  const y = allocBinder("y");
  const yVar = ast.var(y);
  const memberIn = (rule: OpaqueExpr) => ast.binop(ast.opIn(), yVar, rule);
  const primedApp = ast.app(ast.primed(entry.ruleName), [entry.objExpr]);
  const preApp = ast.app(ast.var(entry.ruleName), [entry.objExpr]);
  const tail = clearedFallback(entry.cleared, memberIn(preApp));
  const condArms: [OpaqueExpr, OpaqueExpr][] = entry.memberOverrides.map(
    (o) =>
      [ast.binop(ast.opEq(), yVar, o.elemExpr), o.value] as [
        OpaqueExpr,
        OpaqueExpr,
      ],
  );
  const rhs =
    condArms.length === 0
      ? tail
      : ast.cond([...condArms, [ast.litBool(true), tail]]);
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
  state.modifiedProps = addModifiedProp(state.modifiedProps, entry.ruleName);
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
      // Body-level Forall quantifiers carry param Pant types in their
      // binding heads (`all x: <typeName> | ...`); letting the unknown
      // sentinel reach `paramList` would emit it inside emitted
      // equations. The signature pass already returned an unsupported
      // declaration for this case, so the right move here is to bail
      // with a single unsupported PropResult rather than emit broken
      // equations.
      if (isUnsupportedUnknown(typeName)) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} param '${param.name}': ${UNSUPPORTED_UNKNOWN_REASON}`,
          },
        ];
      }
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

  // M4 Patch 5: functor-lift on the if-conversion shape
  //   `if (x == null) return []; return [f(x)];`
  // Detected at the prelude level (one earlyReturn arm + an
  // expression terminal) so the lift can produce one comprehension
  // instead of an `unsupported`-rejecting cardinality-dispatch Cond.
  if (
    extracted.bindings.length === 1 &&
    extracted.bindings[0]!.kind === "earlyReturn" &&
    ts.isExpression(extracted.returnExpr)
  ) {
    const arm = extracted.bindings[0] as Extract<
      ConstBinding,
      { kind: "earlyReturn" }
    >;
    const lifted = tryRecognizeFunctorLift(
      {
        guard: arm.predicateExpr,
        thenExpr: arm.valueExpr,
        elseExpr: extracted.returnExpr,
        contextNode: node,
      },
      {
        checker,
        strategy,
        paramNames,
        state: undefined,
        supply,
      },
    );
    if (lifted !== null) {
      const liftedRhs = lowerL1ToOpaque(lifted);
      const argExprs = params.map((p) => ast.var(p.name));
      const lhs = ast.app(ast.var(functionName), argExprs);
      return [
        {
          kind: "equation",
          quantifiers: [] as OpaqueParam[],
          lhs,
          rhs: liftedRhs,
        },
      ];
    }
  }

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

  // Pure expression terminals route through the IR unconditionally. The
  // record, functor-lift, and L1 conditional cases above remain ordered
  // ahead of this terminal expression path.
  let rhs: OpaqueExpr;

  // Layer 1 imperative-IR conditional pipeline (workstream M1).
  // Routes when the body has prelude arms (early-return if-conversion)
  // or a conditional terminal (if/switch/ternary/Bool-typed `&&`/`||`).
  // Plain non-conditional returns fall through to the pure IR path.
  const l1IsConditionalReturn =
    ts.isIfStatement(extracted.returnExpr) ||
    ts.isSwitchStatement(extracted.returnExpr) ||
    (ts.isExpression(extracted.returnExpr) &&
      isL1ConditionalForm(extracted.returnExpr, checker));
  if (inlined.arms.length > 0 || l1IsConditionalReturn) {
    const l1Ctx = {
      checker,
      strategy,
      paramNames: inlined.scopedParams,
      state: undefined as SymbolicState | undefined,
      supply,
    };
    let bodyOpaque: OpaqueExpr;
    if (l1IsConditionalReturn) {
      // Build the conditional terminal as L1 first, then merge prelude
      // arms (already OpaqueExpr from `inlineConstBindings`) at the
      // OpaqueExpr layer. When the L1 terminal is itself a cond we
      // splice its arms in to keep the output flat (matching the
      // legacy single-cond shape).
      const terminalL1 = buildL1Conditional(extracted.returnExpr, l1Ctx);
      if (isL1Unsupported(terminalL1)) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — ${terminalL1.unsupported}`,
          },
        ];
      }
      if (terminalL1.kind === "cond") {
        const armsOpaque: Array<[OpaqueExpr, OpaqueExpr]> = [
          ...inlined.arms.map(([g, v]) => [g, v] as [OpaqueExpr, OpaqueExpr]),
          ...terminalL1.arms.map(
            ([g, v]) =>
              [lowerL1ToOpaque(g), lowerL1ToOpaque(v)] as [
                OpaqueExpr,
                OpaqueExpr,
              ],
          ),
          [ast.litBool(true), lowerL1ToOpaque(terminalL1.otherwise)] as [
            OpaqueExpr,
            OpaqueExpr,
          ],
        ];
        bodyOpaque = ast.cond(armsOpaque);
      } else {
        const terminalOpaque = lowerL1ToOpaque(terminalL1);
        if (inlined.arms.length === 0) {
          bodyOpaque = terminalOpaque;
        } else {
          bodyOpaque = ast.cond([
            ...inlined.arms.map(([g, v]) => [g, v] as [OpaqueExpr, OpaqueExpr]),
            [ast.litBool(true), terminalOpaque] as [OpaqueExpr, OpaqueExpr],
          ]);
        }
      }
    } else {
      // Arms-only path: terminal is a plain expression that we route
      // through `buildIR` (the canonical pure path) and merge with the
      // prelude arms at the OpaqueExpr layer.
      if (!ts.isExpression(extracted.returnExpr)) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — unsupported pure return terminal`,
          },
        ];
      }
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
      const terminalOpaque = lowerExpr(ir);
      bodyOpaque = ast.cond([
        ...inlined.arms.map(([g, v]) => [g, v] as [OpaqueExpr, OpaqueExpr]),
        [ast.litBool(true), terminalOpaque] as [OpaqueExpr, OpaqueExpr],
      ]);
    }
    // Apply const-binding substitutions at the OpaqueExpr layer via
    // `substituteBinder` — semantically identical to the IR `Let`
    // chain that lowered through the same primitive. Right-fold
    // semantics: substitute the last binding first.
    for (let i = inlined.translatedBindings.length - 1; i >= 0; i--) {
      const tb = inlined.translatedBindings[i]!;
      bodyOpaque = ast.substituteBinder(
        bodyOpaque,
        tb.hygienicName,
        tb.initExpr,
      );
    }
    rhs = bodyOpaque;
  } else if (ts.isExpression(extracted.returnExpr)) {
    const ir = buildIR(
      extracted.returnExpr,
      checker,
      strategy,
      inlined.scopedParams,
      supply,
    );
    let bodyOpaque: OpaqueExpr;
    if (isBuildUnsupported(ir)) {
      // Native pure-IR construction did not cover this surface form.
      // Fall back to `translateBodyExpr` for the OpaqueExpr —
      // post-M6 this is a direct opaque-layer construction (no
      // `irWrap` round-trip), so the IR pipeline remains the primary
      // path and only specific shapes (`%`, `**`, raw array
      // literals, etc.) reach the legacy emitter. When the legacy
      // emitter also rejects, prefer the buildIR-side message —
      // recognizer-specific reasons (e.g. "array callback block body
      // must be a single return") are more actionable than the
      // legacy fall-through that returns the source text verbatim.
      const legacy = translateBodyExpr(
        extracted.returnExpr,
        checker,
        strategy,
        inlined.scopedParams,
        undefined,
        supply,
      );
      if (isBodyUnsupported(legacy) || "effect" in legacy) {
        const reason = isBodyUnsupported(legacy)
          ? legacy.unsupported
          : `${functionName} — collection mutation in pure return position`;
        return [
          {
            kind: "unsupported",
            reason: ir.unsupported.startsWith(
              "unsupported pure expression form",
            )
              ? reason
              : ir.unsupported,
          },
        ];
      }
      bodyOpaque = bodyExpr(legacy);
    } else {
      bodyOpaque = lowerExpr(ir);
    }
    for (let i = inlined.translatedBindings.length - 1; i >= 0; i--) {
      const tb = inlined.translatedBindings[i]!;
      bodyOpaque = ast.substituteBinder(
        bodyOpaque,
        tb.hygienicName,
        tb.initExpr,
      );
    }
    rhs = bodyOpaque;
  } else {
    return [
      {
        kind: "unsupported",
        reason: `${functionName} — unsupported pure return terminal`,
      },
    ];
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
/**
 * Recognize a `let + while` pair as one prelude unit. Purely structural:
 * the let must have a single identifier declarator with an initializer,
 * the while body must be a single expression-statement (or a single-stmt
 * block thereof). No μ-search-specific semantics here — those live at
 * `ir1-lower.ts:isCanonicalMuSearchForm` and the L1 path in
 * `translateMuSearchInit`. This recognizer's only job is to consume
 * the pair structurally so `extractReturnExpression` can keep walking.
 */
function recognizeLetWhilePair(
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

  // Body must be a single expression-statement (unbraced or single-stmt
  // block). No semantic check on the step — that's the L1 recognizer's
  // job.
  const body = whileStmt.statement;
  const bodyStmt: ts.Statement | null = ts.isBlock(body)
    ? body.statements.length === 1
      ? body.statements[0]!
      : null
    : body;
  if (!bodyStmt || !ts.isExpressionStatement(bodyStmt)) {
    return null;
  }

  return {
    counterName,
    initTsExpr: decl.initializer,
    predicateTsExpr: whileStmt.expression,
    stepExpr: bodyStmt.expression,
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
    const mu = recognizeLetWhilePair(stmts, i, checker);
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
  // Switch as a terminal — handled by the L1 conditional builder
  // (workstream M1). The L1 path requires a default that's last, every
  // case ending in `return EXPR`, and only literal case labels;
  // anything else surfaces as UNSUPPORTED at build time.
  if (ts.isSwitchStatement(last)) {
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
      if (recognizeLetWhilePair(stmts, i, checker)) {
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
      // A let; while pair where recognizeLetWhilePair failed — probably a
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
 * Translate a recognized μ-search prelude binding to an OpaqueExpr.
 * Pure orchestrator — builds the canonical L1 form, hands off to
 * `lowerL1MuSearch` for the L1 → L2 lowering (which carries all
 * μ-search semantics), and lowers the L2 result to OpaqueExpr.
 *
 * Per the layering principle (workstream `ts2pant-imperative-ir.md`):
 * `translate-body.ts` has no Pantagruel-target awareness for μ-search.
 * The canonical-shape check, strategy validation, binder allocation,
 * predicate substitution, and `comb-typed` construction all live in
 * `ir1-lower.ts:lowerL1MuSearch`. This function only wires the
 * lowering context — including the synthCell-aware binder allocator
 * that needs translate-body's internal supply / scopedParams state.
 */
function translateMuSearchInit(
  mu: MuSearch,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  scopedParams: ReadonlyMap<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BindingInitResult {
  const buildCtx = {
    checker,
    strategy,
    paramNames: scopedParams,
    state,
    supply,
  };
  const form = buildL1LetWhile(mu, buildCtx);
  if (isL1StmtUnsupported(form)) {
    return { error: form.unsupported };
  }
  const lowerCtx: MuSearchLowerCtx = {
    strategy,
    counterPantName: scopedParams.get(mu.counterName) ?? mu.counterName,
    allocateBinder: (hint) => {
      // synthCell path: document-wide NameRegistry (kebab-cased,
      // numeric-suffixed) — collision-safe across the document.
      if (supply.synthCell) {
        return cellRegisterName(supply.synthCell, hint);
      }
      // Standalone fallback: avoid colliding with the current frame's
      // Pant names so predicate substitution can't alias a param.
      const usedNames = new Set(scopedParams.values());
      let name: string;
      do {
        name = `${hint}${nextSupply(supply)}`;
      } while (usedNames.has(name));
      return name;
    },
  };
  const lowered = lowerL1MuSearch(form, mu.counterName, lowerCtx);
  if (isLowerUnsupported(lowered)) {
    return { error: lowered.unsupported };
  }
  return { value: lowerExpr(lowered) };
}

function isLowerUnsupported(
  r: IRExpr | { unsupported: string },
): r is { unsupported: string } {
  return typeof r === "object" && r !== null && "unsupported" in r;
}

export function isGuardStatement(
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
    // M4 P3: an assertion arg containing loose equality cannot translate.
    // Filtering the call out of the body would silently drop the runtime
    // check on both sides; keep it so the body translator surfaces the
    // rejection.
    if (call.arguments.some(containsUnsupportedOperator)) {
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
  // M4 P3: same as classifyGuardIf — refuse to classify an if-throw guard
  // whose condition contains an explicitly-unsupported operator. The body
  // translator will then handle the if-statement on the regular path,
  // where it will surface a specific `unsupported` reason.
  if (containsUnsupportedOperator(stmt.expression)) {
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
export function expressionReferencesNames(
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
export function unwrapExpression(expr: ts.Expression): ts.Expression {
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
    // Strip transparent wrappers, but handle `!` separately: when the
    // receiver's Pant emission is list-lifted `[T]` (Alloy `lone`
    // multiplicity for a nullable `T | null`), `x!` lowers to singleton
    // extraction `(x 1)` — the same shape `??` and `?.` produce. When the
    // receiver's Pant emission is already unboxed `T` (e.g. `m.get(k)`,
    // which the Map-encoding emits as a guarded rule application of type
    // `V`), the `!` is structural and passes through. Mirrors the
    // QuestionQuestionToken / questionDotToken handlers below.
    while (
      ts.isParenthesizedExpression(expr) ||
      ts.isAsExpression(expr) ||
      ts.isSatisfiesExpression(expr)
    ) {
      expr = expr.expression;
    }
    if (ts.isNonNullExpression(expr)) {
      const innerExpr = expr.expression;
      const innerResult = translateBodyExpr(
        innerExpr,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(innerResult)) {
        return innerResult;
      }
      // Use the operand's *declared* type (narrowing-free) so a `!`
      // inside a flow-narrowed branch — `if (x != null) return x!;` —
      // still sees `x` as nullable and lowers to singleton extraction.
      // `checker.getTypeAtLocation` would return the narrowed `T` here
      // and silently emit a no-op, but the Pant symbol for `x` is
      // still the list-lifted `[T]` parameter, so the typecheck would
      // fail. Same fix the nullish recognizer applied (PR for the
      // long-form chain narrowing bug — see `getOperandDeclaredType`
      // in nullish-recognizer.ts).
      const receiverTsType = getOperandDeclaredType(innerExpr, checker);
      // The Map encoding is the one place where a TS-nullable receiver
      // (`m.get(k)`'s `V | undefined`) lowers to an *unboxed* Pant
      // value: the guarded-rule emission yields `entries c k` of type
      // `V`, not `[V]`. Singleton-extracting that would be a type
      // error. Every other nullable-typed expression (parameter refs,
      // field accesses, user functions returning `T | null` whose
      // mapped Pant return type is `[T]`) follows mapTsType's list-
      // lift, so `!` lowers to singleton extraction `(x 1)`. The
      // narrow carve-out matches the dispatch in `translateCallExpr`
      // for `.get` on a Map type (line ~3777).
      const innerUnwrapped = unwrapExpression(innerExpr);
      const isMapGetCall =
        ts.isCallExpression(innerUnwrapped) &&
        ts.isPropertyAccessExpression(innerUnwrapped.expression) &&
        innerUnwrapped.expression.name.text === "get" &&
        isMapType(
          checker.getTypeAtLocation(innerUnwrapped.expression.expression),
        );
      if (isNullableTsType(receiverTsType) && !isMapGetCall) {
        return {
          expr: ast.app(bodyExpr(innerResult), [ast.litNat(1)]),
        };
      }
      return innerResult;
    }
  }

  // M4: nullish surface forms (`x == null`, `x === undefined`, the
  // `||`-long form, `typeof x === 'undefined'`, etc.) collapse to a
  // canonical L1 `IsNullish` (with `unop(not, …)` for negated forms).
  // Runs *before* the L1 conditional dispatch so a Bool-typed
  // `||`/`&&` chain that's actually a long-form nullish test does not
  // get consumed by `buildFromShortCircuit`. Operand mismatch (e.g.,
  // `a === null || b === undefined`) returns `null` and falls through
  // to the normal Bool-typed short-circuit path.
  if (ts.isBinaryExpression(expr)) {
    // `translateBodyExpr` itself converts effectful call expressions
    // into `{ unsupported }` via `rejectEffect` (the CallExpression
    // arm below), so the effect variant of `BodyResult` never
    // reaches us here — the surfaced rejection carries the upstream
    // "collection mutation outside statement position" message.
    const l1Ctx = {
      checker,
      strategy,
      paramNames,
      state,
      supply,
    };
    const translate: NullishTranslate = (sub) => {
      const subL1 = tryBuildL1PureSubExpression(sub, l1Ctx);
      if (subL1 === null) {
        return { unsupported: `unsupported nullish operand: ${sub.getText()}` };
      }
      return subL1;
    };
    const recognized = recognizeNullishForm(expr, checker, translate);
    if (recognized !== null) {
      if ("unsupported" in recognized) {
        return { unsupported: recognized.unsupported };
      }
      return { expr: lowerL1ToOpaque(recognized) };
    }
  }

  // Layer 1 imperative-IR conditional pipeline (workstream M1).
  // Conditional value forms — if/else statements, ternaries, switch,
  // and Bool-typed `&&`/`||` — flow through `buildL1Conditional` and
  // collapse to one canonical `Cond` form. The legacy if-statement and
  // ternary handlers were deleted at the M1 patch-3 cutover; L1
  // rejection here is terminal (no fall-through).
  if (
    ts.isIfStatement(expr) ||
    ts.isSwitchStatement(expr) ||
    isL1ConditionalForm(expr, checker)
  ) {
    const l1 = buildL1Conditional(
      expr as ts.Expression | ts.IfStatement | ts.SwitchStatement,
      { checker, strategy, paramNames, state, supply },
    );
    if (isL1Unsupported(l1)) {
      return { unsupported: l1.unsupported };
    }
    return { expr: lowerL1ToOpaque(l1) };
  }

  // Property access. Dispatch order:
  //   (1) Optional chain → functor-lift via comprehension (legacy path;
  //       M5 doesn't touch ?.).
  //   (2) Cardinality (`.length` / `.size` on list-shaped types) →
  //       L1 `Unop(card, x)`. Pant's primitive for cardinality is
  //       `#x`, not a `length` / `size` rule application — routing
  //       through Member would emit a dangling EUF rule. Fires before
  //       Member dispatch.
  //   (3) Plain (non-optional) property access → canonical L1 Member.
  //       The lowering at `ir1-lower.ts` produces `App(qualifiedRule,
  //       [receiver])` — byte-equal to the legacy direct construction.
  if (ts.isPropertyAccessExpression(expr)) {
    const prop = expr.name.text;
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
        const binderName = allocComprehensionBinder(supply, "n");
        return {
          expr: ast.each(
            [],
            [ast.gIn(binderName, bodyExpr(obj))],
            ast.app(ast.var(ruleName), [ast.var(binderName)]),
          ),
        };
      }
      // Receiver isn't actually nullable in TS — `?.` degenerates to
      // `.`; fall through to cardinality / Member dispatch.
    }
    const l1Ctx = {
      checker,
      strategy,
      paramNames,
      state,
      supply,
    };
    const card = tryBuildL1Cardinality(expr, l1Ctx);
    if (card !== null) {
      return { expr: lowerL1ToOpaque(card) };
    }
    const member = buildL1MemberAccess(expr, l1Ctx);
    if ("unsupported" in member) {
      return member;
    }
    return { expr: lowerL1ToOpaque(member) };
  }

  // M5 P3: string-literal element access (`obj["field"]`) collapses to
  // the same canonical L1 Member as dotted access. Computed indices
  // (`obj[k]`, `obj[1+1]`) reject with the
  // `computed property access ...` reason returned by the helper.
  // Optional-chain element access (`obj?.["f"]`) falls through to the
  // raw-text fallback in `translateExpr`; the optional-chain functor
  // lift recognizer in this dispatch only handles `?.` on
  // PropertyAccess today, and adding the element-access variant is a
  // separate concern.
  if (
    ts.isElementAccessExpression(expr) &&
    (expr.flags & ts.NodeFlags.OptionalChain) === 0
  ) {
    const l1Ctx = {
      checker,
      strategy,
      paramNames,
      state,
      supply,
    };
    const member = buildL1MemberAccess(expr, l1Ctx);
    if ("unsupported" in member) {
      return member;
    }
    return { expr: lowerL1ToOpaque(member) };
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
      // Cardinality-zero null test, byte-equivalent to the canonical
      // L1 `IsNullish` lowering (`eq(card(_), 0)`) — built directly at
      // the OpaqueExpr layer because the LHS `xExpr` was already
      // translated through `translateBodyExpr`.
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
    // M4 P3: reject loose equality unconditionally. Patch 2's nullish
    // recognizer consumes `x == null` / `x != null` family before we
    // reach this point; everything else is `==`/`!=` whose intent is
    // ambiguous between value-equality and JS coercion semantics — we
    // steer the programmer toward `===`/`!==` rather than guess.
    if (
      expr.operatorToken.kind === ts.SyntaxKind.EqualsEqualsToken ||
      expr.operatorToken.kind === ts.SyntaxKind.ExclamationEqualsToken
    ) {
      return {
        unsupported: "loose equality (== / !=) is unsupported; use === / !==",
      };
    }
    // M4 P3: canonicalize strict equality through Layer 1
    // `BinOp(eq | neq, ...)`. Sub-expressions wrap via `ir1FromL2`
    // (M5 territory — sub-expressions reach L1 natively then).
    if (
      expr.operatorToken.kind === ts.SyntaxKind.EqualsEqualsEqualsToken ||
      expr.operatorToken.kind === ts.SyntaxKind.ExclamationEqualsEqualsToken
    ) {
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
      // Build the strict-equality binop directly at the OpaqueExpr
      // layer — both operands were translated through
      // `translateBodyExpr` so they're already opaque.
      const op =
        expr.operatorToken.kind === ts.SyntaxKind.EqualsEqualsEqualsToken
          ? ast.opEq()
          : ast.opNeq();
      return { expr: ast.binop(op, bodyExpr(left), bodyExpr(right)) };
    }
    if (
      (expr.operatorToken.kind === ts.SyntaxKind.AmpersandAmpersandToken ||
        expr.operatorToken.kind === ts.SyntaxKind.BarBarToken) &&
      (!isStaticallyBoolTyped(expr.left, checker) ||
        !isStaticallyBoolTyped(expr.right, checker))
    ) {
      return {
        unsupported:
          "&&/|| requires both operands to be statically Bool-typed (workstream conservative-refusal 3(b))",
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
    const result = translateExpr(
      expr,
      checker,
      strategy,
      paramNames,
      supply.synthCell,
    );
    if (isTranslateExprUnsupported(result)) {
      return { unsupported: result.unsupported };
    }
    return { expr: result };
  }

  return { unsupported: "non-expression statement" };
}

export function extractReturnFromBranch(
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
  // Binders that survive into emitted comprehensions go through the
  // document-wide registry. In composing paths, callback-only binders are
  // substituted away before emission and should stay hygienic.
  const sourceBinder = isComposing
    ? pending.binder
    : allocComprehensionBinder(supply, "x");
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
  // In the composing case, the callback's `x` is substituted away with the
  // prior projection, so it should not reserve a public registry name.
  const xBinder = pending
    ? freshHygienicBinder(supply)
    : allocComprehensionBinder(supply, "x");
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

/** Extract the root identifier of a chained property access (`a.b.c` → `a`). */
export function getRootIdentifier(expr: ts.Expression): string | null {
  expr = unwrapExpression(expr);
  if (ts.isIdentifier(expr)) {
    return expr.text;
  }
  if (ts.isPropertyAccessExpression(expr)) {
    return getRootIdentifier(expr.expression);
  }
  return null;
}

export function translateCallExpr(
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
      // M5: route Stage A receiver-property qualification through the
      // canonical L1 Member helper. It bundles `qualifyFieldAccess` +
      // receiver translation in one call and yields the rule name and
      // receiver L1 expression for the effect descriptor.
      const memberR = buildL1MemberAccess(normalizedReceiver, {
        checker,
        strategy,
        paramNames,
        state,
        supply,
      });
      if (isL1Unsupported(memberR)) {
        return { unsupported: memberR.unsupported };
      }
      if (memberR.kind !== "member") {
        return {
          unsupported:
            "Set mutation receiver did not resolve to canonical L1 Member",
        };
      }
      const fieldName = memberR.name;
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
      const objOpaque = lowerL1ToOpaque(memberR.receiver);
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
          objExpr: objOpaque,
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
        const innerObj = normalizedReceiver.expression;
        // M5: Stage A receiver-property qualification through the
        // canonical L1 Member helper. The qualified rule + receiver
        // OpaqueExpr feed the Map effect descriptor.
        const memberR = buildL1MemberAccess(normalizedReceiver, {
          checker,
          strategy,
          paramNames,
          state,
          supply,
        });
        if (isL1Unsupported(memberR)) {
          return { unsupported: memberR.unsupported };
        }
        if (memberR.kind !== "member") {
          return {
            unsupported:
              "Map mutation receiver did not resolve to canonical L1 Member",
          };
        }
        const fieldName = memberR.name;
        const ownerType = mapTsType(
          checker.getTypeAtLocation(innerObj),
          checker,
          strategy,
          supply.synthCell,
        );
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
            objExpr: lowerL1ToOpaque(memberR.receiver),
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
        const innerObj = normalizedReceiver.expression;
        // M5: Stage A receiver-property qualification through the L1
        // Member helper. The receiver L1 lowers to an OpaqueExpr for
        // `readMapThroughWrites`.
        const memberR = buildL1MemberAccess(normalizedReceiver, {
          checker,
          strategy,
          paramNames,
          state,
          supply,
        });
        if (isL1Unsupported(memberR)) {
          return { unsupported: memberR.unsupported };
        }
        if (memberR.kind !== "member") {
          return {
            unsupported:
              "Map read receiver did not resolve to canonical L1 Member",
          };
        }
        const fieldName = memberR.name;
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
            lowerL1ToOpaque(memberR.receiver),
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
          // M5: route Stage A field qualification through L1 Member.
          // Falls through to the generic membership construction below
          // if the helper rejects (ambiguous owner / non-Member result),
          // mirroring the legacy `fieldName === null` graceful fallback.
          const memberR = buildL1MemberAccess(normalizedReceiver, {
            checker,
            strategy,
            paramNames,
            state,
            supply,
          });
          if (!isL1Unsupported(memberR) && memberR.kind === "member") {
            const fieldName = memberR.name;
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
              return {
                expr: readSetThroughWrites(
                  state,
                  fieldName,
                  ownerType,
                  elemType,
                  lowerL1ToOpaque(memberR.receiver),
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
    allocComprehensionBinder(supply, hint);
  for (const [, entry] of state.writes) {
    switch (entry.kind) {
      case "property":
        propositions.push({
          kind: "equation",
          quantifiers: [] as OpaqueParam[],
          lhs: ast.app(ast.primed(entry.prop), [entry.objExpr]),
          rhs: entry.value,
        });
        state.modifiedProps = addModifiedProp(state.modifiedProps, entry.prop);
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
          state.writes = putWrite(state.writes, key, {
            kind: "property",
            prop: entryR.prop,
            objExpr: entryR.objExpr,
            value: merged,
          });
          state.writtenKeys = addWrittenKey(state.writtenKeys, key);
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
          state.writes = putWrite(state.writes, key, {
            kind: "map",
            ruleName: entryR.ruleName,
            keyPredName: entryR.keyPredName,
            ownerType: entryR.ownerType,
            keyType: entryR.keyType,
            valueOverrides: mergedValue,
            membershipOverrides: mergedMember,
          });
          state.writtenKeys = addWrittenKey(state.writtenKeys, key);
          continue;
        }
        // Set: each side projects through its own `cleared` predicate,
        // so the missing-on-prior fallback uses prior's `cleared` and
        // missing-on-cont fallback uses entryR's. Without this, an
        // element only-on-cont would fall through to raw pre-state
        // membership even when prior cleared the set on the early-exit
        // path (and vice versa). `cleared` itself merges through
        // `combineContCond` so a one-sided clear emerges as a guarded
        // predicate rather than collapsing to an unconditional clear.
        const priorSet =
          prior !== undefined && prior.kind === "set" ? prior : undefined;
        const setRuleVar = ast.var(entryR.ruleName);
        const baseIn = (o: SetOverride): OpaqueExpr =>
          ast.binop(
            ast.opIn(),
            o.elemExpr,
            ast.app(setRuleVar, [entryR.objExpr]),
          );
        const priorCleared = priorSet?.cleared ?? ast.litBool(false);
        const fallbackForMissingPrior = (o: SetOverride): OpaqueExpr =>
          clearedFallback(priorCleared, baseIn(o));
        const fallbackForMissingCont = (o: SetOverride): OpaqueExpr =>
          clearedFallback(entryR.cleared, baseIn(o));
        const mergedSet = mergeOverrides(
          entryR.memberOverrides,
          priorSet?.memberOverrides ?? [],
          (o) => o.elemExpr,
          fallbackForMissingPrior,
          combineContCond,
          fallbackForMissingCont,
        );
        const mergedCleared =
          ast.strExpr(entryR.cleared) === ast.strExpr(priorCleared)
            ? entryR.cleared
            : combineContCond(entryR.cleared, priorCleared);
        state.writes = putWrite(state.writes, key, {
          kind: "set",
          ruleName: entryR.ruleName,
          ownerType: entryR.ownerType,
          elemType: entryR.elemType,
          objExpr: entryR.objExpr,
          memberOverrides: mergedSet,
          cleared: mergedCleared,
        });
        state.writtenKeys = addWrittenKey(state.writtenKeys, key);
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

    // Property assignment: obj.prop = rhs. Routes through L1: build
    // canonical `Assign(Member(receiver, name), value)` via
    // `buildL1AssignStmt`, then `lowerL1Body` installs the write into
    // `SymbolicState`. Output is byte-equal to the legacy direct
    // construction (M5 hard-rule cutover for the property-access
    // equivalence class — assignment targets are L1 Member).
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
        const built = buildL1AssignStmt(stmt, {
          checker,
          strategy,
          paramNames,
          state,
          supply,
          applyConst,
        });
        if (isUnsupported(built)) {
          ok = false;
          propositions.push({
            kind: "unsupported",
            reason: built.unsupported,
          });
          continue;
        }
        if (!lowerL1Body(built, state, propositions, { applyConst })) {
          ok = false;
        }
        continue;
      }
    }

    // `arr.forEach(x => { body })` as a top-level statement —
    // recognized by the L1 build pass and lowered as a foreach
    // (Shape A property writes; Shape B and other shapes deferred).
    if (
      ts.isExpressionStatement(stmt) &&
      ts.isCallExpression(unwrapExpression(stmt.expression)) &&
      !insideBranch
    ) {
      const call = unwrapExpression(stmt.expression) as ts.CallExpression;
      if (
        ts.isPropertyAccessExpression(call.expression) &&
        call.expression.name.text === "forEach"
      ) {
        const built = buildL1ForEachCall(call, {
          checker,
          strategy,
          paramNames,
          state,
          supply,
          applyConst,
        });
        if (isUnsupported(built)) {
          propositions.push({
            kind: "unsupported",
            reason: built.unsupported,
          });
          ok = false;
          continue;
        }
        if (!lowerL1Body(built, state, propositions, { applyConst })) {
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

    // Branched mutation: build L1 cond-stmt, lower via lowerL1Body
    // (single-fold over L1 statements; threads SymbolicState).
    // Slice 1 handles simple property assigns; richer branch shapes
    // (Map/Set effects, compound assigns, etc.) reject from the build
    // pass and fall through to the legacy unsupported below.
    if (ts.isIfStatement(stmt)) {
      const built = buildL1IfMutation(stmt, {
        checker,
        strategy,
        paramNames,
        state,
        supply,
        applyConst,
      });
      if (isUnsupported(built)) {
        propositions.push({
          kind: "unsupported",
          reason: built.unsupported,
        });
        ok = false;
        continue;
      }
      if (!lowerL1Body(built, state, propositions, { applyConst })) {
        ok = false;
      }
      continue;
    }

    // `for (const x of arr) { body }` — same path as forEach.
    if (ts.isForOfStatement(stmt) && !insideBranch) {
      const built = buildL1ForOfMutation(stmt, {
        checker,
        strategy,
        paramNames,
        state,
        supply,
        applyConst,
      });
      if (isUnsupported(built)) {
        propositions.push({
          kind: "unsupported",
          reason: built.unsupported,
        });
        ok = false;
        continue;
      }
      if (!lowerL1Body(built, state, propositions, { applyConst })) {
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
  modifiedRules: ReadonlySet<string>,
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
