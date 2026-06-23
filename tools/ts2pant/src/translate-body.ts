// @archlint.module core
// @archlint.domain ts2pant.translate-body

import type { SourceFile } from "ts-morph";
import ts from "typescript";
import {
  type AssumptionEnv,
  enterFrame,
  exitFrame,
  type Fact,
  pushFact,
} from "./assumption-env.js";
import { buildIR, isBuildUnsupported } from "./ir-build.js";
import { lowerExpr } from "./ir-emit.js";
import {
  type IR1Expr,
  type IR1Stmt,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1Break,
  ir1Cond,
  ir1Continue,
  ir1Each,
  ir1For,
  ir1Let,
  ir1LitNat,
  ir1Return,
  ir1Throw,
  ir1Var,
  ir1While,
} from "./ir1.js";
import {
  buildL1Conditional,
  buildL1IncrementStep,
  buildL1MemberAccess,
  buildL1MuSearchCombTyped,
  createL1AssumptionEnv,
  isL1ConditionalForm,
  isL1StmtUnsupported,
  isL1Unsupported,
  type L1BuildContext,
  lowerL1ToOpaque,
  tryBuildBuiltinCall,
  tryBuildL1Cardinality,
  tryBuildL1PureSubExpression,
  tryRecognizeFunctorLift,
} from "./ir1-build.js";
import {
  type BuildBodyCtx,
  buildL1AssignStmt,
  buildL1EffectCall,
  buildL1ForEachCall,
  buildL1ForOfMutation,
  buildL1IfMutation,
  buildL1SubExpr,
  isUnsupported,
} from "./ir1-build-body.js";
import { lowerL1BodyToSsaProps } from "./ir1-lower-body.js";
import {
  appendFramesForUnmodifiedRules,
  type IR1SsaBodyLowerResult,
  type IR1SsaFinalProperty,
  ir1SsaBodyLowerSuccess,
  ir1SsaBodyLowerUnsupported,
} from "./ir1-ssa-lower.js";
import {
  lowerScalarSsaEarlyExitMerge,
  type ScalarSsaEarlyExitPropertyInput,
} from "./ir1-ssa-scalars.js";
import { freeVarsIR1Expr, substituteIR1ExprSubtree } from "./ir1-substitute.js";
import {
  negateFact,
  recognizeNarrowingPredicate,
  recognizeNullishNarrowing,
  recognizeTypePredicateNarrowing,
} from "./narrowing-recognizer.js";
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
import {
  expressionHasSideEffects,
  isEffectFree,
  isStaticallyBoolTyped,
} from "./purity.js";
import {
  applyOpaqueAliases,
  freshHygienicBinder,
  makeUniqueSupply,
  nextSupply,
  type UniqueSupply,
} from "./supply.js";
import {
  translateRecordConditionalReturn,
  translateRecordReturn,
} from "./translate-record.js";
import {
  classifyFunction,
  containsUnsupportedOperator,
  findFunction,
  isAssertionCall,
  isFollowableGuardCall,
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
  lookupMapKV,
  mapTsType,
  type NumericStrategy,
  okSort,
  resolveFieldOwner,
  type SynthCell,
  toPantTermName,
  UNSUPPORTED_NON_DISCRIMINATED_UNION_FIELD_ACCESS_REASON,
} from "./translate-types.js";
import {
  type ExtractedBlockConstBinding,
  extractBlockReturn,
} from "./ts-ast-block-return.js";
import type { PantDeclaration, PropResult } from "./types.js";

export { expressionHasSideEffects } from "./purity.js";

function recognizeBranchFact(
  test: ts.Expression,
  checker: ts.TypeChecker,
): Fact | null {
  return (
    recognizeNullishNarrowing(test, checker) ??
    recognizeTypePredicateNarrowing(test, checker) ??
    recognizeNarrowingPredicate(test, checker)
  );
}

// --- Const-binding inlining infrastructure (let-elimination) ---

// The unique-id supply and opaque-alias side-channel live in the leaf
// `supply.ts` module (below the build pipeline) so consumers can draw fresh
// ids without importing into the translate-body ↔ ir1-build cycle.

/**
 * Allocate a parser-roundtrippable comprehension binder through
 * `cellRegisterName`, so the name is collision-safe against the
 * document-wide `NameRegistry` and emits as a real Pant identifier
 * (`hint`, `hint1`, …).
 *
 * Use this — not `freshHygienicBinder` directly — at any site that emits
 * a binder into a quantifier or comprehension that survives to Pant text.
 * See PR #84 post-mortem in `AGENTS.md` for the bug class this prevents.
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

function allocLocalBindingName(
  supply: UniqueSupply,
  hint: string,
  scopedParams: ReadonlyMap<string, string>,
): string {
  if (supply.synthCell) {
    return cellRegisterName(supply.synthCell, hint);
  }
  const used = new Set(scopedParams.values());
  if (!used.has(hint)) {
    return hint;
  }
  let name: string;
  do {
    name = `${hint}${nextSupply(supply)}`;
  } while (used.has(name));
  return name;
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
type PreludeStmt =
  | {
      kind: "const";
      tsName: string;
      initializer: ts.Expression;
      localName?: string;
    }
  | { kind: "muSearch"; tsName: string; mu: MuSearch; localName?: string }
  | ({ kind: "forOf" } & RecognizedForOfPush)
  | { kind: "builderPush"; accName: string; valueExpr: ts.Expression }
  | { kind: "builderSetAdd"; accName: string; valueExpr: ts.Expression }
  | { kind: "reassign"; tsName: string; valueExpr: ts.Expression }
  | { kind: "stmt"; stmt: ts.Statement }
  | {
      kind: "earlyReturn";
      predicateExpr: ts.Expression;
      valueExpr?: ts.Expression;
      blockBindings: readonly ExtractedBlockConstBinding[];
      nestedBlock?: ts.Block;
    };

export interface RecognizedForOfPush {
  binder: string;
  src: IR1Expr;
  proj: IR1Expr;
  guards: IR1Expr[];
  accName: string;
}

/** Names a binding introduces into scope (none for an early-return arm). */
function bindingNames(b: PreludeStmt): readonly string[] {
  return b.kind === "const" || b.kind === "muSearch" ? [b.tsName] : [];
}

const VAR_BINDINGS_UNSUPPORTED =
  "var bindings are not supported (use let or const)";
const LET_CLOSURE_REASSIGNMENT_UNSUPPORTED =
  "let captured by closure that reassigns it is not supported";

export interface MuSearch {
  counterName: string;
  initTsExpr: ts.Expression;
  predicateTsExpr: ts.Expression;
  /**
   * The expression-statement body of the while loop — the increment
   * step. Used by the L1 builder to construct the canonical
   * L1 `comb-typed` form after canonical step validation.
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
 * Map-typed receiver. The effect is not an expression; it can only appear
 * as a statement and is consumed by the IR1 SSA mutating-body builder.
 * Encountering an effect in expression position is a translation failure.
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
 * MapMutation, it is a statement-position effect consumed by the IR1 SSA
 * mutating-body builder.
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

/**
 * Turn an `effect` result into an `unsupported` marker. Expression-position
 * consumers of `translateBodyExpr` use this after the unsupported check so
 * the remaining value narrows to the `{ expr, pendingComprehension? }` form.
 * The SSA mutating-body builder consumes statement-position effects directly.
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
// property access see the accumulated value. See AGENTS.md § Guarded Commands.

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
 * views, and compatibility updates replace the whole map/set via private
 * immutable helpers. Cell-field reassignment is within
 * ts2pant's self-translation envelope (translatable as primed rules on the
 * cell), whereas the prior `.set`/`.add` in-place mutation was not.
 *
 * `modifiedProps` remains for compatibility with builder contexts that clone
 * state, but frame conditions now derive from IR1 SSA lowering results.
 *
 * `canonicalize` applies ambient opaque aliases to an expression before it is
 * used as a state key. Writes store keys under that canonical form so
 * state-aware reads and later writes resolve to the same key.
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
 * Pure inputs/outputs translate cleanly to Pantagruel rules
 * (`putWrite m k v = …`) — easier to dogfood than the prior
 * mutate-the-state-field shape.
 */

function putWrite(
  writes: ReadonlyMap<string, WriteEntry>,
  key: string,
  entry: WriteEntry,
): ReadonlyMap<string, WriteEntry> {
  const next = new Map(writes);
  next.set(key, entry);
  return next;
}

function addWrittenKey(
  keys: ReadonlySet<string>,
  key: string,
): ReadonlySet<string> {
  const next = new Set(keys);
  next.add(key);
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
 * State-map key for Map writes. Writes coalesce into one MapRuleWriteEntry
 * so multiple `.set`/`.delete` calls to the same rule accumulate as distinct
 * `(tuple |-> value)` override pairs (collapsed into one override expression
 * at emission). Keying by just `ruleName` is too coarse for Stage A, where
 * two different interfaces can share a field name (e.g., `A.cache` and
 * `B.cache`) — emission quantifies one `ownerType`/`keyType` per entry, so
 * crossed-field writes would be emitted against the first writer's types.
 * Include the full rule identity (rule, key predicate, owner, key) so only
 * writes that share a target rule coalesce.
 *
 * @pant mapWriteKey ruleName keyPredName ownerType keyType = "map::" + ruleName + "::" + keyPredName + "::" + ownerType + "::" + keyType.
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
 *
 * This remains private compatibility code for expression translation that
 * still receives a builder state; production mutating-body emission lowers
 * collection reads and writes through IR1 SSA.
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
  // normalize to the same form the SSA write site stores. Otherwise a
  // `const x = a; x.cache.set(k, v); x.cache.get(k)` would have `(a, k) |-> v` on the
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
 *
 * Used only by the private compatibility read helper above.
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
 *
 * This remains private compatibility code for expression translation that
 * still receives a builder state; production mutating-body emission lowers
 * collection reads and writes through IR1 SSA.
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
  assumptionEnv?: AssumptionEnv | undefined;
  recognitionHook?: (env: AssumptionEnv, location: string) => void;
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
    assumptionEnv,
    recognitionHook,
  } = opts;
  const checker = sourceFile.getProject().getTypeChecker().compilerObject;
  const program = sourceFile.getProject().getProgram().compilerObject;
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
      if (!typeName.ok) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} param '${param.name}': ${typeName.reason}`,
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
      paramList.push({ name: pantName, type: typeName.sort });
    }
  }

  // Merge non-parameter renames from `paramNameMap` (e.g., module-level
  // `const NAME = <literal>` declarations registered by the pipeline)
  // into the body's local `paramNames` map. The L1 build dispatcher
  // resolves identifier references through this map, so without the
  // merge the body would emit raw TS names (`UNSUPPORTED_UNKNOWN`)
  // instead of the kebab'd 0-arity rule references
  // (`unsupported-unknown`) the document declares.
  if (paramNameMap) {
    for (const [tsName, pantName] of paramNameMap) {
      if (!paramNames.has(tsName)) {
        paramNames.set(tsName, pantName);
      }
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
      program,
      assumptionEnv,
      recognitionHook,
    );
  } else {
    return translateMutatingBody(
      node,
      baseName,
      checker,
      strategy,
      paramNames,
      declarations ?? [],
      synthCell,
      program,
      assumptionEnv,
      recognitionHook,
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
  program?: ts.Program,
  suppliedEnv?: AssumptionEnv,
  recognitionHook?: (env: AssumptionEnv, location: string) => void,
): PropResult[] {
  const ast = getAst();

  if (!node.body) {
    return [];
  }

  const supply = makeUniqueSupply(synthCell, program);
  const env = suppliedEnv ?? createL1AssumptionEnv();
  const extracted = extractReturnExpression(node.body, {
    checker,
    strategy,
    paramNames,
    state: undefined,
    supply,
    env,
  });
  if (!extracted) {
    const reason = describeRejectedBody(node.body, checker);
    return [{ kind: "unsupported", reason: `${functionName} — ${reason}` }];
  }

  const expectedReturnSort = mappedFunctionReturnSort(
    node,
    checker,
    strategy,
    synthCell,
  );

  // M4 Patch 5: functor-lift on the if-conversion shape
  //   `if (x == null) return []; return [f(x)];`
  // Detected at the prelude level (one earlyReturn arm + an
  // expression terminal) so the lift can produce one comprehension
  // instead of an `unsupported`-rejecting cardinality-dispatch Cond.
  if (
    extracted.bindings.length === 1 &&
    extracted.bindings[0]!.kind === "earlyReturn" &&
    extracted.bindings[0]!.valueExpr !== undefined &&
    ts.isExpression(extracted.returnExpr)
  ) {
    const arm = extracted.bindings[0] as Extract<
      PreludeStmt,
      { kind: "earlyReturn" }
    >;
    const lifted = tryRecognizeFunctorLift(
      {
        guard: arm.predicateExpr,
        thenExpr: arm.valueExpr!,
        elseExpr: extracted.returnExpr,
        contextNode: node,
      },
      {
        checker,
        strategy,
        paramNames,
        state: undefined,
        supply,
        env,
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

  const forOfComprehension = tryBuildForOfComprehensionReturn(
    extracted,
    checker,
    strategy,
    paramNames,
    supply,
    env,
  );
  if (forOfComprehension !== null) {
    if ("error" in forOfComprehension) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — ${forOfComprehension.error}`,
        },
      ];
    }
    const argExprs = params.map((p) => ast.var(p.name));
    const lhs = ast.app(ast.var(functionName), argExprs);
    return [
      ...forOfComprehension.prelude.ruleDecls,
      ...forOfComprehension.loweredLets.propositions,
      {
        kind: "equation",
        quantifiers: [] as OpaqueParam[],
        lhs,
        rhs: lowerL1ToOpaque(forOfComprehension.each),
      },
    ];
  }

  const localListBuilder = tryBuildLocalListBuilderReturn(
    extracted,
    checker,
    strategy,
    paramNames,
    supply,
    env,
  );
  if (localListBuilder !== null) {
    if ("error" in localListBuilder) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — ${localListBuilder.error}`,
        },
      ];
    }
    if (localListBuilder.diagnostics.length > 0) {
      return localListBuilder.diagnostics;
    }
    const argExprs = params.map((p) => ast.var(p.name));
    const resultApp = () => ast.app(ast.var(functionName), argExprs);
    const assertions: PropResult[] = [
      {
        kind: "assertion",
        quantifiers: [] as OpaqueParam[],
        body: ast.binop(
          ast.opEq(),
          ast.unop(ast.opCard(), resultApp()),
          ast.litNat(localListBuilder.pushed.length),
        ),
      },
      ...localListBuilder.pushed.map((value, idx): PropResult => {
        const valueExpr = applyOpaqueAliases(lowerL1ToOpaque(value), supply);
        return {
          kind: "assertion",
          quantifiers: [] as OpaqueParam[],
          body: ast.binop(
            ast.opEq(),
            ast.app(resultApp(), [ast.litNat(idx + 1)]),
            valueExpr,
          ),
        };
      }),
    ];
    return [
      ...localListBuilder.prelude.ruleDecls,
      ...localListBuilder.loweredLets.propositions,
      ...assertions,
    ];
  }

  const localSetBuilder = tryBuildLocalSetBuilderReturn(
    node,
    extracted,
    checker,
    strategy,
    paramNames,
    supply,
    env,
  );
  if (localSetBuilder !== null) {
    if ("error" in localSetBuilder) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — ${localSetBuilder.error}`,
        },
      ];
    }
    if (localSetBuilder.diagnostics.length > 0) {
      return localSetBuilder.diagnostics;
    }
    const argExprs = params.map((p) => ast.var(p.name));
    const resultApp = ast.app(ast.var(functionName), argExprs);
    const binder = allocSetMembershipBinder(supply, params, localSetBuilder);
    const binderVar = ast.var(binder);
    const memberExpr = ast.binop(ast.opIn(), binderVar, resultApp);
    const body =
      localSetBuilder.added.length === 0
        ? ast.unop(ast.opNot(), memberExpr)
        : ast.binop(
            ast.opIff(),
            memberExpr,
            localSetBuilder.added
              .map((value) =>
                ast.binop(
                  ast.opEq(),
                  binderVar,
                  applyOpaqueAliases(lowerL1ToOpaque(value), supply),
                ),
              )
              .reduce((acc, expr) => ast.binop(ast.opOr(), acc, expr)),
          );
    return [
      ...localSetBuilder.prelude.ruleDecls,
      ...localSetBuilder.loweredLets.propositions,
      {
        kind: "assertion",
        quantifiers: [
          ast.param(binder, ast.tName(localSetBuilder.elemType)),
        ] as OpaqueParam[],
        body,
      },
    ];
  }

  const unsupportedLocalCollectionConstructor =
    describeUnsupportedLocalCollectionConstructorReturn(extracted);
  if (unsupportedLocalCollectionConstructor !== null) {
    return [
      {
        kind: "unsupported",
        reason: `${functionName} — ${unsupportedLocalCollectionConstructor}`,
      },
    ];
  }

  const prelude = lowerPreludeBindings(
    extracted.bindings,
    checker,
    strategy,
    paramNames,
    supply,
    env,
    undefined,
    expectedReturnSort,
  );
  if ("error" in prelude) {
    return [
      { kind: "unsupported", reason: `${functionName} — ${prelude.error}` },
    ];
  }
  const loweredLets =
    prelude.letStmts.length === 0
      ? ir1SsaBodyLowerSuccess()
      : lowerL1BodyToSsaProps(
          ir1Block(prelude.letStmts as [IR1Stmt, ...IR1Stmt[]]),
          [],
          {
            applyConst: (e) => applyOpaqueAliases(e, supply),
          },
        );
  if (loweredLets.diagnostics.length > 0) {
    return loweredLets.diagnostics;
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
      b.kind === "earlyReturn" &&
      b.valueExpr !== undefined &&
      ts.isObjectLiteralExpression(b.valueExpr),
  );
  const terminalIsObjLit =
    ts.isExpression(extracted.returnExpr) &&
    ts.isObjectLiteralExpression(extracted.returnExpr);
  const terminalIfHasObjLitBranch =
    ts.isIfStatement(extracted.returnExpr) &&
    ifTerminalHasObjLitBranch(extracted.returnExpr, checker);

  if (terminalIsObjLit && prelude.arms.length === 0) {
    return translateRecordReturn(
      extracted.returnExpr as ts.ObjectLiteralExpression,
      functionName,
      params,
      node,
      checker,
      strategy,
      prelude.scopedParams,
      supply,
      synthCell,
      (e) => applyOpaqueAliases(e, supply),
    );
  }

  if (terminalIsObjLit && prelude.arms.length > 0) {
    const recordArms = collectPreludeRecordArms(
      extracted.bindings,
      prelude.arms,
    );
    if ("error" in recordArms) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — ${recordArms.error}`,
        },
      ];
    }
    const conditionalResults = translateRecordConditionalReturn(
      recordArms.arms,
      extracted.returnExpr as ts.ObjectLiteralExpression,
      functionName,
      params,
      node,
      checker,
      strategy,
      prelude.scopedParams,
      supply,
      synthCell,
      (e) => applyOpaqueAliases(e, supply),
    );
    return [
      ...prelude.ruleDecls,
      ...loweredLets.propositions,
      ...conditionalResults,
    ];
  }

  if (ts.isIfStatement(extracted.returnExpr) && terminalIfHasObjLitBranch) {
    const preludeRecordArms = collectPreludeRecordArms(
      extracted.bindings,
      prelude.arms,
    );
    if ("error" in preludeRecordArms) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — ${preludeRecordArms.error}`,
        },
      ];
    }
    const recordIf = collectIfRecordConditionalArms(extracted.returnExpr, {
      checker,
      strategy,
      paramNames: prelude.scopedParams,
      state: undefined,
      supply,
      env,
      ...(expectedReturnSort === undefined
        ? {}
        : { expectedSort: expectedReturnSort }),
    });
    if ("error" in recordIf) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — ${recordIf.error}`,
        },
      ];
    }
    const conditionalResults = translateRecordConditionalReturn(
      [...preludeRecordArms.arms, ...recordIf.arms],
      recordIf.otherwise,
      functionName,
      params,
      node,
      checker,
      strategy,
      prelude.scopedParams,
      supply,
      synthCell,
      (e) => applyOpaqueAliases(e, supply),
    );
    return [
      ...prelude.ruleDecls,
      ...loweredLets.propositions,
      ...conditionalResults,
    ];
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
  if (prelude.arms.length > 0 || l1IsConditionalReturn) {
    if (prelude.fallthroughFacts.length > 0) {
      enterFrame(env);
      for (const fact of prelude.fallthroughFacts) {
        pushFact(env, fact);
      }
      recognitionHook?.(env, "early-return.fallthrough");
    }
    const l1Ctx = {
      checker,
      strategy,
      paramNames: prelude.scopedParams,
      state: undefined as SymbolicState | undefined,
      supply,
      env,
      ...(expectedReturnSort === undefined
        ? {}
        : { expectedSort: expectedReturnSort }),
      ...(recognitionHook === undefined ? {} : { recognitionHook }),
    };
    let bodyOpaque: OpaqueExpr;
    try {
      if (l1IsConditionalReturn) {
        // Build the conditional terminal as L1 first, then merge prelude
        // early-return arms at the OpaqueExpr layer. When the L1 terminal is
        // itself a cond we
        // splice its arms in to keep the output flat (matching the
        // legacy single-cond shape).
        const terminalL1 = ts.isIfStatement(extracted.returnExpr)
          ? (lowerNestedPureIfStatementToL1(
              extracted.returnExpr,
              {
                checker,
                strategy,
                paramNames: prelude.scopedParams,
                supply,
                env,
                ...(expectedReturnSort === undefined
                  ? {}
                  : { expectedReturnSort }),
              },
              prelude.scopedParams,
            ) ?? {
              unsupported:
                "if-then branch must contain a single return-with-value",
            })
          : buildL1Conditional(extracted.returnExpr, l1Ctx);
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
            ...prelude.arms.map(([g, v]) => [g, v] as [OpaqueExpr, OpaqueExpr]),
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
          if (prelude.arms.length === 0) {
            bodyOpaque = terminalOpaque;
          } else {
            bodyOpaque = ast.cond([
              ...prelude.arms.map(
                ([g, v]) => [g, v] as [OpaqueExpr, OpaqueExpr],
              ),
              [ast.litBool(true), terminalOpaque] as [OpaqueExpr, OpaqueExpr],
            ]);
          }
        }
      } else {
        // Arms-only path: terminal is a plain expression that we route
        // through `buildIR` (the canonical pure path) and merge with the
        // prelude arms at the OpaqueExpr layer. Mirror the plain-return
        // path's legacy fallback so adding a prelude arm doesn't regress
        // a function whose terminal expression only translates through
        // `translateBodyExpr` (`%`, `**`, raw array literals, etc.).
        if (!ts.isExpression(extracted.returnExpr)) {
          return [
            {
              kind: "unsupported",
              reason: `${functionName} — unsupported pure return terminal`,
            },
          ];
        }
        let terminalOpaque: OpaqueExpr;
        const terminalDefinednessSnapshot = supply.synthCell
          ? [...supply.synthCell.definednessObligations]
          : null;
        const terminalL1 = buildL1SubExpr(extracted.returnExpr, {
          checker,
          strategy,
          paramNames: prelude.scopedParams,
          state: makeSymbolicState(),
          supply,
          env,
          ...(expectedReturnSort === undefined
            ? {}
            : { expectedSort: expectedReturnSort }),
        });
        if (!isUnsupported(terminalL1)) {
          terminalOpaque = lowerL1ToOpaque(terminalL1);
        } else {
          if (supply.synthCell && terminalDefinednessSnapshot) {
            supply.synthCell.definednessObligations =
              terminalDefinednessSnapshot;
          }
          const ir = buildIR(
            extracted.returnExpr,
            checker,
            strategy,
            prelude.scopedParams,
            supply,
          );
          if (!isBuildUnsupported(ir)) {
            terminalOpaque = lowerExpr(ir);
          } else {
            const legacy = translateBodyExpr(
              extracted.returnExpr,
              checker,
              strategy,
              prelude.scopedParams,
              undefined,
              supply,
              env,
            );
            if (isBodyUnsupported(legacy) || "effect" in legacy) {
              const reason = isBodyUnsupported(legacy)
                ? legacy.unsupported
                : `${functionName} — collection mutation in pure return position`;
              return [
                {
                  kind: "unsupported",
                  reason: ir.unsupported.startsWith(
                    "unsupported pure expression",
                  )
                    ? reason
                    : ir.unsupported,
                },
              ];
            }
            terminalOpaque = bodyExpr(legacy);
          }
        }
        bodyOpaque = ast.cond([
          ...prelude.arms.map(([g, v]) => [g, v] as [OpaqueExpr, OpaqueExpr]),
          [ast.litBool(true), terminalOpaque] as [OpaqueExpr, OpaqueExpr],
        ]);
      }
    } finally {
      if (prelude.fallthroughFacts.length > 0) {
        exitFrame(env);
      }
    }
    rhs = bodyOpaque;
  } else if (ts.isExpression(extracted.returnExpr)) {
    const l1Opaque = buildL1SubExpr(extracted.returnExpr, {
      checker,
      strategy,
      paramNames: prelude.scopedParams,
      state: makeSymbolicState(),
      supply,
      env,
      ...(expectedReturnSort === undefined
        ? {}
        : { expectedSort: expectedReturnSort }),
    });
    const ir =
      l1Opaque === null || isUnsupported(l1Opaque)
        ? buildIR(
            extracted.returnExpr,
            checker,
            strategy,
            prelude.scopedParams,
            supply,
          )
        : null;
    let bodyOpaque: OpaqueExpr;
    if (l1Opaque !== null && !isUnsupported(l1Opaque)) {
      bodyOpaque = lowerL1ToOpaque(l1Opaque);
    } else if (ir !== null && isBuildUnsupported(ir)) {
      // Native pure-IR construction did not cover this surface form.
      // Fall back to `translateBodyExpr` for the OpaqueExpr; the IR
      // pipeline remains the primary path and only specific shapes
      // (`%`, `**`, raw array literals, etc.) reach the legacy
      // emitter. When the legacy emitter also rejects, prefer the
      // buildIR-side message — recognizer-specific reasons (e.g.
      // "array callback block body
      // must be a single return") are more actionable than the
      // legacy fall-through that returns the source text verbatim.
      const legacy = translateBodyExpr(
        extracted.returnExpr,
        checker,
        strategy,
        prelude.scopedParams,
        undefined,
        supply,
        env,
      );
      if (isBodyUnsupported(legacy) || "effect" in legacy) {
        const reason = isBodyUnsupported(legacy)
          ? legacy.unsupported
          : `${functionName} — collection mutation in pure return position`;
        return [
          {
            kind: "unsupported",
            reason: ir.unsupported.startsWith("unsupported pure expression")
              ? reason
              : ir.unsupported,
          },
        ];
      }
      bodyOpaque = bodyExpr(legacy);
    } else if (ir !== null) {
      bodyOpaque = lowerExpr(ir);
    } else {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — unsupported pure return terminal`,
        },
      ];
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
    ...prelude.ruleDecls,
    ...loweredLets.propositions,
    {
      kind: "equation",
      quantifiers: [] as OpaqueParam[],
      lhs,
      rhs,
    },
  ];
}

interface ExtractedBody {
  bindings: PreludeStmt[];
  returnExpr: ts.Expression | ts.IfStatement | ts.SwitchStatement;
  guardStmts: readonly ts.Statement[];
}

interface LocalListBuilderResult {
  returnedAccumulatorName: string;
  pushed: IR1Expr[];
  prelude: LoweredPreludeBindings;
  loweredLets: IR1SsaBodyLowerResult;
  diagnostics: PropResult[];
}

interface LocalSetBuilderResult {
  returnedAccumulatorName: string;
  elemType: string;
  added: IR1Expr[];
  prelude: LoweredPreludeBindings;
  loweredLets: IR1SsaBodyLowerResult;
  diagnostics: PropResult[];
}

function tryBuildForOfComprehensionReturn(
  extracted: ExtractedBody,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  supply: UniqueSupply,
  env: AssumptionEnv,
):
  | {
      prelude: LoweredPreludeBindings;
      loweredLets: IR1SsaBodyLowerResult;
      each: IR1Expr;
    }
  | { error: string }
  | null {
  if (!ts.isExpression(extracted.returnExpr)) {
    return null;
  }
  const returnedExpr = unwrapExpression(extracted.returnExpr);
  if (!ts.isIdentifier(returnedExpr)) {
    return null;
  }
  const accName = returnedExpr.text;
  const forOfBindings = extracted.bindings.filter(
    (binding): binding is Extract<PreludeStmt, { kind: "forOf" }> =>
      binding.kind === "forOf" && binding.accName === accName,
  );
  if (forOfBindings.length === 0) {
    return null;
  }
  if (forOfBindings.length !== 1) {
    return { error: "for-of build-list comprehension must have one loop" };
  }

  const accDecls = extracted.bindings.filter(
    (binding) =>
      binding.kind === "const" &&
      binding.tsName === accName &&
      isEmptyArrayLiteral(binding.initializer),
  );
  if (accDecls.length !== 1) {
    return {
      error:
        "for-of build-list comprehension must return its empty-array accumulator",
    };
  }
  if (guardStatementsReadName(extracted.guardStmts, accName)) {
    return {
      error:
        "for-of build-list comprehension guard may not read the accumulator",
    };
  }
  if (
    extracted.bindings.some(
      (binding) =>
        (binding.kind === "reassign" && binding.tsName === accName) ||
        (binding.kind === "stmt" && nodeWritesName(binding.stmt, accName)),
    )
  ) {
    return {
      error:
        "for-of build-list comprehension accumulator has unsupported writes",
    };
  }

  const forOf = forOfBindings[0]!;
  const otherBindings = extracted.bindings.filter(
    (binding) => binding !== forOf && binding !== accDecls[0],
  );
  const prelude = lowerPreludeBindings(
    otherBindings,
    checker,
    strategy,
    paramNames,
    supply,
    env,
    undefined,
  );
  if ("error" in prelude) {
    return prelude;
  }
  if (prelude.arms.length > 0) {
    return {
      error:
        "for-of build-list comprehension with early returns is not supported",
    };
  }
  const loweredLets =
    prelude.letStmts.length === 0
      ? ir1SsaBodyLowerSuccess()
      : lowerL1BodyToSsaProps(
          ir1Block(prelude.letStmts as [IR1Stmt, ...IR1Stmt[]]),
          [],
          {
            applyConst: (e) => applyOpaqueAliases(e, supply),
          },
        );
  if (loweredLets.diagnostics.length > 0) {
    const first = loweredLets.diagnostics[0];
    return {
      error:
        first?.kind === "unsupported"
          ? first.reason
          : "for-of build-list comprehension prelude did not lower",
    };
  }
  return {
    prelude,
    loweredLets,
    each: ir1Each(forOf.binder, forOf.src, forOf.guards, forOf.proj),
  };
}

function tryBuildLocalListBuilderReturn(
  extracted: ExtractedBody,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  supply: UniqueSupply,
  env: AssumptionEnv,
): LocalListBuilderResult | { error: string } | null {
  const builderPushes = extracted.bindings.filter(
    (binding): binding is Extract<PreludeStmt, { kind: "builderPush" }> =>
      binding.kind === "builderPush",
  );
  if (builderPushes.length === 0) {
    return null;
  }
  if (!ts.isExpression(extracted.returnExpr)) {
    return {
      error: "list builder must return its local accumulator",
    };
  }
  const returnedExpr = unwrapExpression(extracted.returnExpr);
  if (!ts.isIdentifier(returnedExpr)) {
    return {
      error: "list builder must return its local accumulator",
    };
  }
  const accName = returnedExpr.text;
  const accDecls = extracted.bindings.filter(
    (binding): binding is Extract<PreludeStmt, { kind: "const" }> =>
      binding.kind === "const" &&
      binding.tsName === accName &&
      isEmptyArrayLiteral(binding.initializer),
  );
  if (accDecls.length !== 1) {
    return {
      error:
        "list builder must return the same local empty-array accumulator it pushes",
    };
  }
  if (builderPushes.some((push) => push.accName !== accName)) {
    return {
      error: "list builder may only push to the returned local accumulator",
    };
  }
  if (guardStatementsReadName(extracted.guardStmts, accName)) {
    return {
      error: "list builder guard may not read the accumulator",
    };
  }
  const accNames = new Set([accName]);
  let seenPush = false;
  const preludeBindings: PreludeStmt[] = [];
  for (const binding of extracted.bindings) {
    if (binding === accDecls[0]) {
      continue;
    }
    if (binding.kind === "builderPush") {
      seenPush = true;
      if (expressionReferencesNames(binding.valueExpr, accNames)) {
        return {
          error: "list builder push argument may not read the accumulator",
        };
      }
      continue;
    }
    if (binding.kind === "const") {
      if (seenPush) {
        return {
          error:
            "list builder const bindings must appear before pushed expressions",
        };
      }
      if (expressionReferencesNames(binding.initializer, accNames)) {
        return {
          error: "list builder accumulator alias or escape is not supported",
        };
      }
      preludeBindings.push(binding);
      continue;
    }
    return {
      error:
        "list builder only supports const bindings and direct accumulator.push calls before return",
    };
  }

  const prelude = lowerPreludeBindings(
    preludeBindings,
    checker,
    strategy,
    paramNames,
    supply,
    env,
    undefined,
  );
  if ("error" in prelude) {
    return prelude;
  }
  if (prelude.arms.length > 0) {
    return {
      error: "list builder with early returns is not supported",
    };
  }
  const loweredLets =
    prelude.letStmts.length === 0
      ? ir1SsaBodyLowerSuccess()
      : lowerL1BodyToSsaProps(
          ir1Block(prelude.letStmts as [IR1Stmt, ...IR1Stmt[]]),
          [],
          {
            applyConst: (e) => applyOpaqueAliases(e, supply),
          },
        );

  const pushed: IR1Expr[] = [];
  for (const push of builderPushes) {
    if (expressionHasSideEffects(push.valueExpr, checker)) {
      return {
        error: "list builder push argument has side effects",
      };
    }
    const value = buildPureL1OrNull(push.valueExpr, {
      checker,
      strategy,
      paramNames: prelude.scopedParams,
      state: undefined,
      supply,
      env,
    });
    if (value === null) {
      return {
        error: "unsupported pure expression in list builder push argument",
      };
    }
    pushed.push(value);
  }
  if (pushed.length === 0) {
    return {
      error: "list builder must push at least one value",
    };
  }

  return {
    returnedAccumulatorName: accName,
    pushed,
    prelude,
    loweredLets,
    diagnostics: loweredLets.diagnostics,
  };
}

function tryBuildLocalSetBuilderReturn(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  extracted: ExtractedBody,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: ReadonlyMap<string, string>,
  supply: UniqueSupply,
  env: AssumptionEnv,
): LocalSetBuilderResult | { error: string } | null {
  if (
    !ts.isExpression(extracted.returnExpr) ||
    !ts.isIdentifier(extracted.returnExpr)
  ) {
    return null;
  }
  const accName = extracted.returnExpr.text;
  const accDecls = extracted.bindings.filter(
    (binding): binding is Extract<PreludeStmt, { kind: "const" }> =>
      binding.kind === "const" &&
      binding.tsName === accName &&
      isEmptySetConstructor(binding.initializer),
  );
  if (accDecls.length === 0) {
    const setAdds = extracted.bindings.filter(
      (binding): binding is Extract<PreludeStmt, { kind: "builderSetAdd" }> =>
        binding.kind === "builderSetAdd",
    );
    return setAdds.length === 0
      ? null
      : {
          error:
            "Set builder must return the same local empty Set accumulator it adds to",
        };
  }
  if (accDecls.length !== 1) {
    return {
      error: "Set builder must have exactly one returned empty Set accumulator",
    };
  }

  const setAdds = extracted.bindings.filter(
    (binding): binding is Extract<PreludeStmt, { kind: "builderSetAdd" }> =>
      binding.kind === "builderSetAdd",
  );
  if (setAdds.some((add) => add.accName !== accName)) {
    return {
      error: "Set builder may only add to the returned local accumulator",
    };
  }

  const sig = checker.getSignatureFromDeclaration(node);
  const returnType =
    sig === undefined
      ? checker.getTypeAtLocation(extracted.returnExpr)
      : checker.getReturnTypeOfSignature(sig);
  const elemType = setElementSort(
    returnType,
    checker,
    strategy,
    supply.synthCell,
  );
  if (elemType === null) {
    return {
      error:
        "Set builder return type must be Set<T> or ReadonlySet<T> with a supported element type",
    };
  }

  const accNames = new Set([accName]);
  let seenAdd = false;
  const preludeBindings: PreludeStmt[] = [];
  for (const binding of extracted.bindings) {
    if (binding === accDecls[0]) {
      continue;
    }
    if (binding.kind === "builderSetAdd") {
      seenAdd = true;
      if (expressionReferencesNames(binding.valueExpr, accNames)) {
        return {
          error: "Set builder add argument may not read the accumulator",
        };
      }
      continue;
    }
    if (binding.kind === "const") {
      if (seenAdd) {
        return {
          error:
            "Set builder const bindings must appear before added expressions",
        };
      }
      if (expressionReferencesNames(binding.initializer, accNames)) {
        return {
          error: "Set builder accumulator alias or escape is not supported",
        };
      }
      preludeBindings.push(binding);
      continue;
    }
    return {
      error:
        "Set builder only supports const bindings and direct accumulator.add calls before return",
    };
  }

  const prelude = lowerPreludeBindings(
    preludeBindings,
    checker,
    strategy,
    paramNames,
    supply,
    env,
    undefined,
  );
  if ("error" in prelude) {
    return prelude;
  }
  if (prelude.arms.length > 0) {
    return {
      error: "Set builder with early returns is not supported",
    };
  }
  const loweredLets =
    prelude.letStmts.length === 0
      ? ir1SsaBodyLowerSuccess()
      : lowerL1BodyToSsaProps(
          ir1Block(prelude.letStmts as [IR1Stmt, ...IR1Stmt[]]),
          [],
          {
            applyConst: (e) => applyOpaqueAliases(e, supply),
          },
        );

  const added: IR1Expr[] = [];
  for (const add of setAdds) {
    if (expressionHasSideEffects(add.valueExpr, checker)) {
      return {
        error: "Set builder add argument has side effects",
      };
    }
    const value = buildPureL1OrNull(add.valueExpr, {
      checker,
      strategy,
      paramNames: prelude.scopedParams,
      state: undefined,
      supply,
      env,
    });
    if (value === null) {
      return {
        error: "unsupported pure expression in Set builder add argument",
      };
    }
    added.push(value);
  }

  return {
    returnedAccumulatorName: accName,
    elemType,
    added,
    prelude,
    loweredLets,
    diagnostics: loweredLets.diagnostics,
  };
}

function allocSetMembershipBinder(
  supply: UniqueSupply,
  params: Array<{ name: string; type: string }>,
  builder: LocalSetBuilderResult,
): string {
  const usedNames = new Set<string>(params.map((p) => p.name));
  for (const name of builder.prelude.scopedParams.values()) {
    usedNames.add(name);
  }
  for (const expr of builder.added) {
    for (const name of freeVarsIR1Expr(expr)) {
      usedNames.add(name);
    }
  }

  let binder = allocComprehensionBinder(supply, "x");
  while (usedNames.has(binder)) {
    binder = allocComprehensionBinder(supply, "x");
  }
  return binder;
}

function describeUnsupportedLocalCollectionConstructorReturn(
  extracted: ExtractedBody,
): string | null {
  if (
    !ts.isExpression(extracted.returnExpr) ||
    !ts.isIdentifier(extracted.returnExpr)
  ) {
    return null;
  }
  const accName = extracted.returnExpr.text;
  const accDecl = extracted.bindings.find(
    (binding): binding is Extract<PreludeStmt, { kind: "const" }> =>
      binding.kind === "const" && binding.tsName === accName,
  );
  if (accDecl === undefined) {
    return null;
  }
  if (isSetConstructorWithIterable(accDecl.initializer)) {
    return "Set builder from iterable is not supported";
  }
  if (isMapConstructor(accDecl.initializer)) {
    return "Map builder construction is not supported in this milestone";
  }
  return null;
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
    `property access .${fieldName}: ` +
    UNSUPPORTED_NON_DISCRIMINATED_UNION_FIELD_ACCESS_REASON
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
 * const bindings for IR1Let lowering.
 * Handles:
 *   - Single return statement
 *   - Leading const bindings + return statement
 *   - if/else with returns in both branches (produces a synthetic conditional)
 * Returns null if the body contains unsupported prelude statements.
 */
/**
 * Recognize the Kleene μ-minimization pattern as a `let counter = init;`
 * statement followed by `while (P(counter)) counter++;` (with or without
 * braces around the while body).
 *
 * Returns `{ counterName, initTsExpr, predicateTsExpr }` if the pair at
 * `stmts[idx]` and `stmts[idx + 1]` matches; null otherwise. Shape-only
 * match: counter must be a single identifier with any initializer, and the
 * loop body must be exactly one expression statement. The L1 builder
 * validates the canonical `+1` counter step. Purity of the
 * initializer and predicate is screened in `lowerPreludeBindings`'s TDZ
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
 * block thereof). μ-search semantics are applied by the L1 builder called
 * from `lowerPreludeBindings`; this recognizer consumes the pair
 * structurally so `extractReturnExpression` can keep walking.
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
function recognizeEarlyReturnArm(stmt: ts.Statement): {
  predicateExpr: ts.Expression;
  valueExpr?: ts.Expression;
  blockBindings: readonly ExtractedBlockConstBinding[];
  nestedBlock?: ts.Block;
} | null {
  if (!ts.isIfStatement(stmt)) {
    return null;
  }
  if (stmt.elseStatement) {
    return null;
  }
  const body = stmt.thenStatement;
  if (ts.isReturnStatement(body)) {
    if (body.expression === undefined) {
      return null;
    }
    return {
      predicateExpr: stmt.expression,
      valueExpr: body.expression,
      blockBindings: [],
    };
  }
  if (!ts.isBlock(body)) {
    return null;
  }
  const extracted = extractBlockReturn(body);
  if (extracted === null) {
    if (!blockHasReturnTerminal(body)) {
      return null;
    }
    return {
      predicateExpr: stmt.expression,
      blockBindings: [],
      nestedBlock: body,
    };
  }
  return {
    predicateExpr: stmt.expression,
    valueExpr: extracted.returnExpr,
    blockBindings: extracted.bindings,
  };
}

function blockHasReturnTerminal(block: ts.Block): boolean {
  const last = [...block.statements].at(-1);
  return last !== undefined && statementHasReturnTerminal(last);
}

function statementHasReturnTerminal(stmt: ts.Statement): boolean {
  if (ts.isReturnStatement(stmt)) {
    return stmt.expression !== undefined;
  }
  if (ts.isBlock(stmt)) {
    return blockHasReturnTerminal(stmt);
  }
  if (ts.isIfStatement(stmt)) {
    return (
      stmt.elseStatement !== undefined &&
      statementHasReturnTerminal(stmt.thenStatement) &&
      statementHasReturnTerminal(stmt.elseStatement)
    );
  }
  return ts.isSwitchStatement(stmt);
}

/**
 * Recognize the pure build-list loop:
 *
 *   for (const x of xs) { [if (P)] acc.push(f(x)); }
 *
 * The recognizer is intentionally not wired into extraction in Patch 2.
 * It is a conservative AST matcher plus pure-L1 lowering probe that returns
 * the exact pieces Patch 3 will wrap in `ir1Each`.
 */
export function recognizeForOfPush(
  stmt: ts.ForOfStatement,
  accNames: ReadonlySet<string>,
  ctx: L1BuildContext,
): RecognizedForOfPush | null {
  const init = stmt.initializer;
  if (
    !ts.isVariableDeclarationList(init) ||
    !(init.flags & ts.NodeFlags.Const) ||
    init.declarations.length !== 1
  ) {
    return null;
  }
  const decl = init.declarations[0]!;
  if (!ts.isIdentifier(decl.name)) {
    return null;
  }

  const binderTsName = decl.name.text;
  const binder = toPantTermName(binderTsName);
  const scopedParams = new Map(ctx.paramNames);
  scopedParams.set(binderTsName, binder);
  const scopedCtx: L1BuildContext = { ...ctx, paramNames: scopedParams };

  if (!forOfSourceIsExpressible(stmt, decl, ctx)) {
    return null;
  }

  const src = buildPureL1OrNull(stmt.expression, ctx);
  if (src === null) {
    return null;
  }

  const push = recognizeForOfPushBody(stmt.statement, accNames);
  if (push === null) {
    return null;
  }
  if (expressionHasSideEffects(push.projExpr, ctx.checker)) {
    return null;
  }
  const proj = buildPureL1OrNull(push.projExpr, scopedCtx);
  if (proj === null) {
    return null;
  }

  const guards: IR1Expr[] = [];
  for (const guardExpr of push.guardExprs) {
    if (!isStaticallyBoolTyped(guardExpr, ctx.checker)) {
      return null;
    }
    if (
      !isEffectFree(guardExpr, ctx.checker, {
        admitForeignBoolPredicates: true,
      })
    ) {
      return null;
    }
    const guard = buildPureL1OrNull(guardExpr, scopedCtx);
    if (guard === null) {
      return null;
    }
    guards.push(guard);
  }

  return {
    binder,
    src,
    proj,
    guards,
    accName: push.accName,
  };
}

function forOfSourceIsExpressible(
  stmt: ts.ForOfStatement,
  decl: ts.VariableDeclaration,
  ctx: L1BuildContext,
): boolean {
  const sourceType = mapTsType(
    ctx.checker.getTypeAtLocation(stmt.expression),
    ctx.checker,
    ctx.strategy,
    ctx.supply.synthCell,
  );
  if (!sourceType.ok) {
    return false;
  }

  const binderType = mapTsType(
    ctx.checker.getTypeAtLocation(decl.name),
    ctx.checker,
    ctx.strategy,
    ctx.supply.synthCell,
  );
  if (!binderType.ok) {
    return false;
  }
  return sourceType.sort === `[${binderType.sort}]`;
}

function buildPureL1OrNull(
  expr: ts.Expression,
  ctx: L1BuildContext,
): IR1Expr | null {
  const built = tryBuildL1PureSubExpression(expr, ctx);
  if (built === null || isL1Unsupported(built)) {
    return null;
  }
  return built;
}

interface RecognizedPushBody {
  accName: string;
  projExpr: ts.Expression;
  guardExprs: ts.Expression[];
}

function recognizeForOfPushBody(
  stmt: ts.Statement,
  accNames: ReadonlySet<string>,
): RecognizedPushBody | null {
  if (ts.isBlock(stmt) && stmt.statements.length === 2) {
    const guard = recognizeIfContinue(stmt.statements[0]!);
    if (guard === null) {
      return null;
    }
    const push = recognizePushStatement(stmt.statements[1]!, accNames);
    if (push === null) {
      return null;
    }
    return {
      ...push,
      guardExprs: [guard],
    };
  }

  const bodyStmt = unwrapSingleStatementBlock(stmt);
  if (bodyStmt === null) {
    return null;
  }

  const direct = recognizePushStatement(bodyStmt, accNames);
  if (direct !== null) {
    return { ...direct, guardExprs: [] };
  }

  if (ts.isIfStatement(bodyStmt)) {
    if (bodyStmt.elseStatement !== undefined) {
      return null;
    }
    const guardedPush = recognizeForOfPushBody(
      bodyStmt.thenStatement,
      accNames,
    );
    if (guardedPush === null || guardedPush.guardExprs.length !== 0) {
      return null;
    }
    return {
      ...guardedPush,
      guardExprs: [bodyStmt.expression],
    };
  }

  return null;
}

function unwrapSingleStatementBlock(stmt: ts.Statement): ts.Statement | null {
  if (!ts.isBlock(stmt)) {
    return stmt;
  }
  return stmt.statements.length === 1 ? stmt.statements[0]! : null;
}

function recognizePushStatement(
  stmt: ts.Statement,
  accNames: ReadonlySet<string>,
): Omit<RecognizedPushBody, "guardExprs"> | null {
  if (!ts.isExpressionStatement(stmt)) {
    return null;
  }
  const expr = stmt.expression;
  if (
    !ts.isCallExpression(expr) ||
    expr.arguments.length !== 1 ||
    !ts.isPropertyAccessExpression(expr.expression) ||
    expr.expression.name.text !== "push" ||
    !ts.isIdentifier(expr.expression.expression)
  ) {
    return null;
  }
  const accName = expr.expression.expression.text;
  if (!accNames.has(accName)) {
    return null;
  }
  return { accName, projExpr: expr.arguments[0]! };
}

function recognizeSetAddStatement(
  stmt: ts.Statement,
  accNames: ReadonlySet<string>,
): { accName: string; valueExpr: ts.Expression } | null {
  if (!ts.isExpressionStatement(stmt)) {
    return null;
  }
  const expr = stmt.expression;
  if (
    !ts.isCallExpression(expr) ||
    expr.arguments.length !== 1 ||
    !ts.isPropertyAccessExpression(expr.expression) ||
    expr.expression.name.text !== "add" ||
    !ts.isIdentifier(expr.expression.expression)
  ) {
    return null;
  }
  const accName = expr.expression.expression.text;
  if (!accNames.has(accName)) {
    return null;
  }
  return { accName, valueExpr: expr.arguments[0]! };
}

function describeLocalListBuilderMutationRejection(
  stmt: ts.ExpressionStatement,
  accNames: ReadonlySet<string>,
  aliases: ReadonlySet<string>,
): string | null {
  const expr = unwrapExpression(stmt.expression);
  if (ts.isCallExpression(expr)) {
    for (const arg of expr.arguments) {
      if (expressionReferencesNames(arg, new Set([...accNames, ...aliases]))) {
        return "list builder accumulator alias or escape is not supported";
      }
    }
  }
  if (
    !ts.isCallExpression(expr) ||
    !ts.isPropertyAccessExpression(expr.expression) ||
    !ts.isIdentifier(expr.expression.expression)
  ) {
    return null;
  }
  const receiver = expr.expression.expression.text;
  const method = expr.expression.name.text;
  if (aliases.has(receiver)) {
    return "list builder accumulator alias or escape is not supported";
  }
  if (accNames.has(receiver) && method !== "push") {
    return `list builder unknown mutation .${method} is not supported`;
  }
  return null;
}

function describeLocalSetBuilderMutationRejection(
  stmt: ts.ExpressionStatement,
  accNames: ReadonlySet<string>,
  aliases: ReadonlySet<string>,
): string | null {
  const expr = unwrapExpression(stmt.expression);
  if (ts.isCallExpression(expr)) {
    for (const arg of expr.arguments) {
      if (expressionReferencesNames(arg, new Set([...accNames, ...aliases]))) {
        return "Set builder accumulator alias or escape is not supported";
      }
    }
  }
  if (
    !ts.isCallExpression(expr) ||
    !ts.isPropertyAccessExpression(expr.expression) ||
    !ts.isIdentifier(expr.expression.expression)
  ) {
    return null;
  }
  const receiver = expr.expression.expression.text;
  const method = expr.expression.name.text;
  if (aliases.has(receiver)) {
    return "Set builder accumulator alias or escape is not supported";
  }
  if (accNames.has(receiver) && method !== "add") {
    return `Set builder mutation .${method} is not supported`;
  }
  return null;
}

function describeLocalMapBuilderMutationRejection(
  stmt: ts.ExpressionStatement,
  accNames: ReadonlySet<string>,
  aliases: ReadonlySet<string>,
): string | null {
  const expr = unwrapExpression(stmt.expression);
  if (ts.isCallExpression(expr)) {
    for (const arg of expr.arguments) {
      if (expressionReferencesNames(arg, new Set([...accNames, ...aliases]))) {
        return "Map builder accumulator alias or escape is not supported";
      }
    }
  }
  if (
    !ts.isCallExpression(expr) ||
    !ts.isPropertyAccessExpression(expr.expression) ||
    !ts.isIdentifier(expr.expression.expression)
  ) {
    return null;
  }
  const receiver = expr.expression.expression.text;
  if (aliases.has(receiver)) {
    return "Map builder accumulator alias or escape is not supported";
  }
  if (accNames.has(receiver)) {
    return "Map builder construction is not supported in this milestone";
  }
  return null;
}

function describeLocalMapBuilderBodyRejection(
  body: ts.Block,
  checker: ts.TypeChecker,
): string | null {
  const stmts = body.statements.filter((s) => !isGuardStatement(s, checker));
  if (stmts.length < 2) {
    return null;
  }

  const mapAccNames = new Set<string>();
  const aliases = new Set<string>();
  const last = stmts[stmts.length - 1]!;
  const scanStmts = ts.isReturnStatement(last) ? stmts.slice(0, -1) : stmts;
  for (const stmt of scanStmts) {
    if (ts.isVariableStatement(stmt)) {
      const bindings = constLikeBindingsFromVariableStatement(stmt, body);
      if (bindings === null) {
        continue;
      }
      for (const binding of bindings) {
        if (binding.kind !== "const") {
          continue;
        }
        if (isMapConstructor(binding.initializer)) {
          mapAccNames.add(binding.tsName);
        } else if (
          expressionReferencesNames(binding.initializer, mapAccNames)
        ) {
          aliases.add(binding.tsName);
        }
      }
      continue;
    }
    if (!ts.isExpressionStatement(stmt)) {
      continue;
    }
    const rejection = describeLocalMapBuilderMutationRejection(
      stmt,
      mapAccNames,
      aliases,
    );
    if (rejection !== null) {
      return rejection;
    }
  }
  return null;
}

function recognizeIfContinue(stmt: ts.Statement): ts.Expression | null {
  const ifStmt = unwrapSingleStatementBlock(stmt);
  if (
    ifStmt === null ||
    !ts.isIfStatement(ifStmt) ||
    ifStmt.elseStatement !== undefined ||
    !isContinueOnly(ifStmt.thenStatement)
  ) {
    return null;
  }
  return unwrapNegatedCondition(ifStmt.expression);
}

function isContinueOnly(stmt: ts.Statement): boolean {
  const inner = unwrapSingleStatementBlock(stmt);
  return inner !== null && ts.isContinueStatement(inner);
}

function unwrapNegatedCondition(expr: ts.Expression): ts.Expression | null {
  if (
    ts.isPrefixUnaryExpression(expr) &&
    expr.operator === ts.SyntaxKind.ExclamationToken
  ) {
    return expr.operand;
  }
  return null;
}

function extractReturnExpression(
  body: ts.Block,
  ctx: L1BuildContext,
): ExtractedBody | null {
  const { checker } = ctx;
  // Skip guard statements (if-throw patterns and assertion calls)
  const guardStmts = body.statements.filter((s) =>
    isGuardStatement(s, checker),
  );
  const stmts = body.statements.filter((s) => !isGuardStatement(s, checker));

  if (stmts.length === 0) {
    return null;
  }

  const bindings: PreludeStmt[] = [];
  const letNames = new Set<string>();
  const emptyArrayAccNames = new Set<string>();
  const emptySetAccNames = new Set<string>();
  let scopedParams = ctx.paramNames;
  let lastLetDeclNames = new Set<string>();

  // Every statement before the last must be either a const binding or a
  // recognized μ-search pair (`let counter = init; while (P) counter++`).
  // Any other shape rejects the whole body. The last statement is the
  // return / if-else-return.
  const lastIdx = stmts.length - 1;
  let i = 0;
  while (i < lastIdx) {
    const mu = recognizeLetWhilePair(stmts, i, checker);
    if (mu) {
      const localName = allocLocalBindingName(
        ctx.supply,
        toPantTermName(mu.counterName),
        scopedParams,
      );
      bindings.push({
        kind: "muSearch",
        tsName: mu.counterName,
        mu,
        localName,
      });
      scopedParams = withParam(scopedParams, mu.counterName, localName);
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
    if (ts.isExpressionStatement(stmt)) {
      const push = recognizePushStatement(stmt, emptyArrayAccNames);
      if (push !== null) {
        bindings.push({
          kind: "builderPush",
          accName: push.accName,
          valueExpr: push.projExpr,
        });
        i += 1;
        lastLetDeclNames = new Set();
        continue;
      }
      const setAdd = recognizeSetAddStatement(stmt, emptySetAccNames);
      if (setAdd !== null) {
        bindings.push({
          kind: "builderSetAdd",
          accName: setAdd.accName,
          valueExpr: setAdd.valueExpr,
        });
        i += 1;
        lastLetDeclNames = new Set();
        continue;
      }
      const reassigned = reassignmentsFromExpressionStatement(stmt, letNames);
      if (reassigned === null) {
        return null;
      }
      bindings.push(...reassigned);
      i += 1;
      lastLetDeclNames = new Set();
      continue;
    }
    if (ts.isForOfStatement(stmt)) {
      const recognized = recognizeForOfPush(stmt, emptyArrayAccNames, {
        ...ctx,
        paramNames: scopedParams,
      });
      if (recognized === null) {
        return null;
      }
      bindings.push({ kind: "forOf", ...recognized });
      i += 1;
      lastLetDeclNames = new Set();
      continue;
    }
    if (
      ts.isIfStatement(stmt) ||
      ts.isForStatement(stmt) ||
      ts.isWhileStatement(stmt)
    ) {
      const writtenLetNames = writtenLetNamesInStatement(stmt, letNames);
      if (writtenLetNames.size === 0) {
        return null;
      }
      if (
        ts.isWhileStatement(stmt) &&
        [...writtenLetNames].every((name) => lastLetDeclNames.has(name))
      ) {
        return null;
      }
      bindings.push({ kind: "stmt", stmt });
      i += 1;
      lastLetDeclNames = new Set();
      continue;
    }
    if (!ts.isVariableStatement(stmt)) {
      return null;
    }
    if (
      stmt.declarationList.flags & ts.NodeFlags.Let &&
      i + 1 < lastIdx &&
      ts.isWhileStatement(stmts[i + 1]!)
    ) {
      const counterFor = recognizeLocalAccumulatorWhileAsFor(
        stmt,
        stmts[i + 1]! as ts.WhileStatement,
        letNames,
      );
      if (counterFor !== null) {
        bindings.push({ kind: "stmt", stmt: counterFor });
        i += 2;
        lastLetDeclNames = new Set();
        continue;
      }
    }
    const loweredBindings = constLikeBindingsFromVariableStatement(stmt, body);
    if (loweredBindings === null) {
      return null;
    }
    const scopedBindings: PreludeStmt[] = [];
    for (const binding of loweredBindings) {
      if (
        binding.kind === "const" &&
        isEmptyArrayLiteral(binding.initializer)
      ) {
        emptyArrayAccNames.add(binding.tsName);
      }
      if (
        binding.kind === "const" &&
        isEmptySetConstructor(binding.initializer)
      ) {
        emptySetAccNames.add(binding.tsName);
      }
      if (binding.kind === "const") {
        const localName = allocLocalBindingName(
          ctx.supply,
          toPantTermName(binding.tsName),
          scopedParams,
        );
        scopedBindings.push({ ...binding, localName });
        scopedParams = withParam(scopedParams, binding.tsName, localName);
      } else {
        scopedBindings.push(binding);
      }
    }
    if (stmt.declarationList.flags & ts.NodeFlags.Let) {
      lastLetDeclNames = new Set();
      for (const name of bindingNamesFromDeclarationList(
        stmt.declarationList,
      )) {
        letNames.add(name);
        lastLetDeclNames.add(name);
      }
    } else {
      lastLetDeclNames = new Set();
    }
    bindings.push(...scopedBindings);
    i += 1;
  }

  const last = stmts[lastIdx]!;
  if (ts.isReturnStatement(last) && last.expression) {
    return { bindings, returnExpr: last.expression, guardStmts };
  }
  if (ts.isIfStatement(last) && last.elseStatement) {
    return { bindings, returnExpr: last, guardStmts };
  }
  // Switch as a terminal — handled by the L1 conditional builder
  // (workstream M1). The L1 path requires a default that's last, every
  // case ending in `return EXPR`, and only literal case labels;
  // anything else surfaces as UNSUPPORTED at build time.
  if (ts.isSwitchStatement(last)) {
    return { bindings, returnExpr: last, guardStmts };
  }

  return null;
}

function isEmptyArrayLiteral(expr: ts.Expression): boolean {
  let current = expr;
  while (
    ts.isParenthesizedExpression(current) ||
    ts.isAsExpression(current) ||
    ts.isTypeAssertionExpression(current) ||
    ts.isSatisfiesExpression(current) ||
    ts.isNonNullExpression(current)
  ) {
    current = current.expression;
  }
  return ts.isArrayLiteralExpression(current) && current.elements.length === 0;
}

function isEmptySetConstructor(expr: ts.Expression): boolean {
  const current = unwrapExpression(expr);
  return (
    ts.isNewExpression(current) &&
    ts.isIdentifier(current.expression) &&
    current.expression.text === "Set" &&
    (current.arguments === undefined || current.arguments.length === 0)
  );
}

function isSetConstructorWithIterable(expr: ts.Expression): boolean {
  const current = unwrapExpression(expr);
  return (
    ts.isNewExpression(current) &&
    ts.isIdentifier(current.expression) &&
    current.expression.text === "Set" &&
    current.arguments !== undefined &&
    current.arguments.length > 0
  );
}

function isEmptyMapConstructor(expr: ts.Expression): boolean {
  const current = unwrapExpression(expr);
  return (
    ts.isNewExpression(current) &&
    ts.isIdentifier(current.expression) &&
    current.expression.text === "Map" &&
    (current.arguments === undefined || current.arguments.length === 0)
  );
}

function isMapConstructor(expr: ts.Expression): boolean {
  const current = unwrapExpression(expr);
  return (
    ts.isNewExpression(current) &&
    ts.isIdentifier(current.expression) &&
    current.expression.text === "Map"
  );
}

function setElementSort(
  type: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell?: SynthCell,
): string | null {
  if (!isSetType(type)) {
    return null;
  }
  const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
  if (typeArgs.length !== 1) {
    return null;
  }
  const mapped = mapTsType(typeArgs[0]!, checker, strategy, synthCell);
  return mapped.ok ? mapped.sort : null;
}

function nodeWritesName(node: ts.Node, name: string): boolean {
  let found = false;
  const visit = (current: ts.Node): void => {
    if (found) {
      return;
    }
    if (
      ts.isBinaryExpression(current) &&
      ts.isIdentifier(current.left) &&
      current.left.text === name &&
      isAssignmentOperator(current.operatorToken.kind)
    ) {
      found = true;
      return;
    }
    if (
      (ts.isPrefixUnaryExpression(current) ||
        ts.isPostfixUnaryExpression(current)) &&
      ts.isIdentifier(current.operand) &&
      current.operand.text === name &&
      (current.operator === ts.SyntaxKind.PlusPlusToken ||
        current.operator === ts.SyntaxKind.MinusMinusToken)
    ) {
      found = true;
      return;
    }
    ts.forEachChild(current, visit);
  };
  visit(node);
  return found;
}

function guardStatementsReadName(
  guardStmts: readonly ts.Statement[],
  name: string,
): boolean {
  const names = new Set([name]);
  return guardStmts.some((stmt) => nodeReferencesNames(stmt, names));
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
    const emptyArrayAccNames = new Set<string>();
    const emptySetAccNames = new Set<string>();
    const emptyMapAccNames = new Set<string>();
    const listBuilderAliases = new Set<string>();
    const setBuilderAliases = new Set<string>();
    const mapBuilderAliases = new Set<string>();
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
      if (ts.isForOfStatement(stmt)) {
        return "for-of loop is not a recognized build-list comprehension";
      }
      // An if-shaped statement that didn't match recognizeEarlyReturnArm:
      // diagnose precisely.
      if (ts.isIfStatement(stmt)) {
        if (stmt.elseStatement) {
          return "if-with-else only supported as the final statement";
        }
        return "if-with-return block must contain only const bindings followed by a return";
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
        if (declList.flags & ts.NodeFlags.Let) {
          const declaredNames = declaredIdentifierNames(declList);
          if (
            declaredNames === null ||
            findClosureCapturedReassignedNamesInBody(body, declaredNames).size >
              0
          ) {
            return LET_CLOSURE_REASSIGNMENT_UNSUPPORTED;
          }
        } else if (!(declList.flags & ts.NodeFlags.Const)) {
          return VAR_BINDINGS_UNSUPPORTED;
        }
        if (constLikeBindingsFromVariableStatement(stmt, body) !== null) {
          for (const binding of constLikeBindingsFromVariableStatement(
            stmt,
            body,
          ) ?? []) {
            if (
              binding.kind === "const" &&
              isEmptyArrayLiteral(binding.initializer)
            ) {
              emptyArrayAccNames.add(binding.tsName);
            } else if (
              binding.kind === "const" &&
              isEmptySetConstructor(binding.initializer)
            ) {
              emptySetAccNames.add(binding.tsName);
            } else if (
              binding.kind === "const" &&
              isSetConstructorWithIterable(binding.initializer)
            ) {
              return "Set builder from iterable is not supported";
            } else if (
              binding.kind === "const" &&
              isEmptyMapConstructor(binding.initializer)
            ) {
              emptyMapAccNames.add(binding.tsName);
            } else if (
              binding.kind === "const" &&
              isMapConstructor(binding.initializer)
            ) {
              return "Map builder construction is not supported in this milestone";
            } else if (
              binding.kind === "const" &&
              expressionReferencesNames(binding.initializer, emptyArrayAccNames)
            ) {
              listBuilderAliases.add(binding.tsName);
            } else if (
              binding.kind === "const" &&
              expressionReferencesNames(binding.initializer, emptySetAccNames)
            ) {
              setBuilderAliases.add(binding.tsName);
            } else if (
              binding.kind === "const" &&
              expressionReferencesNames(binding.initializer, emptyMapAccNames)
            ) {
              mapBuilderAliases.add(binding.tsName);
            }
          }
          i += 1;
          continue;
        }
      }
      if (ts.isExpressionStatement(stmt)) {
        if (recognizePushStatement(stmt, emptyArrayAccNames) !== null) {
          i += 1;
          continue;
        }
        if (recognizeSetAddStatement(stmt, emptySetAccNames) !== null) {
          i += 1;
          continue;
        }
        const mutation = describeLocalListBuilderMutationRejection(
          stmt,
          emptyArrayAccNames,
          listBuilderAliases,
        );
        if (mutation !== null) {
          return mutation;
        }
        const setMutation = describeLocalSetBuilderMutationRejection(
          stmt,
          emptySetAccNames,
          setBuilderAliases,
        );
        if (setMutation !== null) {
          return setMutation;
        }
        const mapMutation = describeLocalMapBuilderMutationRejection(
          stmt,
          emptyMapAccNames,
          mapBuilderAliases,
        );
        if (mapMutation !== null) {
          return mapMutation;
        }
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

function constLikeBindingsFromVariableStatement(
  stmt: ts.VariableStatement,
  body: ts.Block,
): PreludeStmt[] | null {
  const declList = stmt.declarationList;
  if (declList.flags & ts.NodeFlags.Const) {
    return constBindingsFromDeclarationList(declList);
  }
  if (declList.flags & ts.NodeFlags.Let) {
    const declaredNames = declaredIdentifierNames(declList);
    if (
      declaredNames === null ||
      findClosureCapturedReassignedNamesInBody(body, declaredNames).size > 0
    ) {
      return null;
    }
    return constBindingsFromDeclarationList(declList);
  }
  return null;
}

function constBindingsFromDeclarationList(
  declList: ts.VariableDeclarationList,
): PreludeStmt[] | null {
  const bindings: PreludeStmt[] = [];
  for (const decl of declList.declarations) {
    if (!ts.isIdentifier(decl.name) || !decl.initializer) {
      return null;
    }
    bindings.push({
      kind: "const",
      tsName: decl.name.text,
      initializer: decl.initializer,
    });
  }
  return bindings;
}

/**
 * @pant all dl: ForeignVariableDeclarationList | binding-names-from-declaration-list dl = (each d in declarations dl, is-identifier (name d) | identifier-text (name d)).
 */
function bindingNamesFromDeclarationList(
  declList: ts.VariableDeclarationList,
): readonly string[] {
  const names: string[] = [];
  for (const decl of declList.declarations) {
    if (ts.isIdentifier(decl.name)) {
      names.push(decl.name.text);
    }
  }
  return names;
}

const COMPOUND_ASSIGN_TO_BINARY = new Map<ts.SyntaxKind, ts.BinaryOperator>([
  [ts.SyntaxKind.PlusEqualsToken, ts.SyntaxKind.PlusToken],
  [ts.SyntaxKind.MinusEqualsToken, ts.SyntaxKind.MinusToken],
  [ts.SyntaxKind.AsteriskEqualsToken, ts.SyntaxKind.AsteriskToken],
  [ts.SyntaxKind.SlashEqualsToken, ts.SyntaxKind.SlashToken],
  [ts.SyntaxKind.PercentEqualsToken, ts.SyntaxKind.PercentToken],
  [
    ts.SyntaxKind.AsteriskAsteriskEqualsToken,
    ts.SyntaxKind.AsteriskAsteriskToken,
  ],
  [
    ts.SyntaxKind.LessThanLessThanEqualsToken,
    ts.SyntaxKind.LessThanLessThanToken,
  ],
  [
    ts.SyntaxKind.GreaterThanGreaterThanEqualsToken,
    ts.SyntaxKind.GreaterThanGreaterThanToken,
  ],
  [
    ts.SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken,
    ts.SyntaxKind.GreaterThanGreaterThanGreaterThanToken,
  ],
  [ts.SyntaxKind.AmpersandEqualsToken, ts.SyntaxKind.AmpersandToken],
  [ts.SyntaxKind.BarEqualsToken, ts.SyntaxKind.BarToken],
  [ts.SyntaxKind.CaretEqualsToken, ts.SyntaxKind.CaretToken],
  [ts.SyntaxKind.BarBarEqualsToken, ts.SyntaxKind.BarBarToken],
  [
    ts.SyntaxKind.AmpersandAmpersandEqualsToken,
    ts.SyntaxKind.AmpersandAmpersandToken,
  ],
  [
    ts.SyntaxKind.QuestionQuestionEqualsToken,
    ts.SyntaxKind.QuestionQuestionToken,
  ],
]);

function reassignmentsFromExpressionStatement(
  stmt: ts.ExpressionStatement,
  letNames: ReadonlySet<string>,
): PreludeStmt[] | null {
  const expr = unwrapExpression(stmt.expression);
  if (ts.isBinaryExpression(expr)) {
    if (expr.operatorToken.kind === ts.SyntaxKind.EqualsToken) {
      return simpleReassignmentsFromAssignment(expr.left, expr.right, letNames);
    }
    const op = COMPOUND_ASSIGN_TO_BINARY.get(expr.operatorToken.kind);
    if (
      op !== undefined &&
      ts.isIdentifier(expr.left) &&
      letNames.has(expr.left.text)
    ) {
      return [
        {
          kind: "reassign",
          tsName: expr.left.text,
          // Anchor the synthetic binop to its real originating node so
          // `sourceRefForNode` can recover a source position for it (a bare
          // factory node has no source file). See ir1-build.ts:sourceRefForNode.
          valueExpr: ts.setOriginalNode(
            ts.factory.createBinaryExpression(expr.left, op, expr.right),
            expr,
          ),
        },
      ];
    }
    return null;
  }
  if (ts.isPrefixUnaryExpression(expr) || ts.isPostfixUnaryExpression(expr)) {
    if (
      (expr.operator !== ts.SyntaxKind.PlusPlusToken &&
        expr.operator !== ts.SyntaxKind.MinusMinusToken) ||
      !ts.isIdentifier(expr.operand) ||
      !letNames.has(expr.operand.text)
    ) {
      return null;
    }
    return [
      {
        kind: "reassign",
        tsName: expr.operand.text,
        // Anchor the synthetic binop to its real originating node (see above).
        valueExpr: ts.setOriginalNode(
          ts.factory.createBinaryExpression(
            expr.operand,
            expr.operator === ts.SyntaxKind.PlusPlusToken
              ? ts.SyntaxKind.PlusToken
              : ts.SyntaxKind.MinusToken,
            ts.factory.createNumericLiteral(1),
          ),
          expr,
        ),
      },
    ];
  }
  return null;
}

// The returned ForStatement is factory-created and has no parent pointers.
// That is safe because buildSupportedSsaStatement consumes its child structure
// only; if that changes, this synthetic node needs parent wiring.
function recognizeLocalAccumulatorWhileAsFor(
  counterDeclStmt: ts.VariableStatement,
  whileStmt: ts.WhileStatement,
  letNames: ReadonlySet<string>,
): ts.ForStatement | null {
  const declList = counterDeclStmt.declarationList;
  if (
    declList.declarations.length !== 1 ||
    !ts.isIdentifier(declList.declarations[0]!.name) ||
    declList.declarations[0]!.initializer === undefined
  ) {
    return null;
  }
  const counterName = declList.declarations[0]!.name.text;
  const body = ts.isBlock(whileStmt.statement)
    ? [...whileStmt.statement.statements]
    : [whileStmt.statement];
  if (body.length !== 2 || !ts.isExpressionStatement(body[0]!)) {
    return null;
  }
  const stepStmt = body[1]!;
  if (!ts.isExpressionStatement(stepStmt)) {
    return null;
  }
  const step = unwrapExpression(stepStmt.expression);
  if (
    !(
      (ts.isPostfixUnaryExpression(step) || ts.isPrefixUnaryExpression(step)) &&
      step.operator === ts.SyntaxKind.PlusPlusToken &&
      ts.isIdentifier(step.operand) &&
      step.operand.text === counterName
    )
  ) {
    return null;
  }
  const assignment = unwrapExpression(body[0]!.expression);
  if (
    !ts.isBinaryExpression(assignment) ||
    assignment.operatorToken.kind !== ts.SyntaxKind.PlusEqualsToken ||
    !ts.isIdentifier(assignment.left) ||
    !letNames.has(assignment.left.text)
  ) {
    return null;
  }
  const forBody = ts.factory.createBlock([body[0]!], true);
  return ts.factory.createForStatement(
    declList,
    whileStmt.expression,
    stepStmt.expression,
    forBody,
  );
}

function simpleReassignmentsFromAssignment(
  lhs: ts.Expression,
  rhs: ts.Expression,
  letNames: ReadonlySet<string>,
): PreludeStmt[] | null {
  const target = unwrapExpression(lhs);
  if (ts.isIdentifier(target)) {
    return letNames.has(target.text)
      ? [{ kind: "reassign", tsName: target.text, valueExpr: rhs }]
      : null;
  }
  if (ts.isArrayLiteralExpression(target)) {
    const temps: PreludeStmt[] = [];
    const assigns: PreludeStmt[] = [];
    for (let i = 0; i < target.elements.length; i++) {
      const element = target.elements[i]!;
      if (!ts.isIdentifier(element) || !letNames.has(element.text)) {
        return null;
      }
      // toPantTermName turns these ASCII temp names into Pant identifiers like
      // `ts2pant-destructure-0-x`; snapshots pin that emitted form.
      const tempName = `__ts2pant_destructure_${i}_${element.text}`;
      temps.push({
        kind: "const",
        tsName: tempName,
        initializer: ts.isArrayLiteralExpression(rhs)
          ? (rhs.elements[i] as ts.Expression)
          : ts.factory.createElementAccessExpression(
              rhs,
              ts.factory.createNumericLiteral(i),
            ),
      });
      assigns.push({
        kind: "reassign",
        tsName: element.text,
        valueExpr: ts.factory.createIdentifier(tempName),
      });
    }
    return [...temps, ...assigns];
  }
  if (ts.isObjectLiteralExpression(target)) {
    const temps: PreludeStmt[] = [];
    const assigns: PreludeStmt[] = [];
    let i = 0;
    for (const prop of target.properties) {
      if (ts.isShorthandPropertyAssignment(prop)) {
        if (!letNames.has(prop.name.text)) {
          return null;
        }
        // toPantTermName turns these ASCII temp names into Pant identifiers like
        // `ts2pant-destructure-0-x`; snapshots pin that emitted form.
        const tempName = `__ts2pant_destructure_${i}_${prop.name.text}`;
        temps.push({
          kind: "const",
          tsName: tempName,
          initializer:
            objectLiteralPropertyValue(rhs, prop.name.text) ??
            ts.factory.createPropertyAccessExpression(rhs, prop.name),
        });
        assigns.push({
          kind: "reassign",
          tsName: prop.name.text,
          valueExpr: ts.factory.createIdentifier(tempName),
        });
        i += 1;
        continue;
      }
      if (
        ts.isPropertyAssignment(prop) &&
        ts.isIdentifier(prop.initializer) &&
        letNames.has(prop.initializer.text) &&
        (ts.isIdentifier(prop.name) || ts.isStringLiteral(prop.name))
      ) {
        // toPantTermName turns these ASCII temp names into Pant identifiers like
        // `ts2pant-destructure-0-x`; snapshots pin that emitted form.
        const tempName = `__ts2pant_destructure_${i}_${prop.initializer.text}`;
        temps.push({
          kind: "const",
          tsName: tempName,
          initializer:
            objectLiteralPropertyValue(rhs, prop.name.text) ??
            ts.factory.createPropertyAccessExpression(rhs, prop.name.text),
        });
        assigns.push({
          kind: "reassign",
          tsName: prop.initializer.text,
          valueExpr: ts.factory.createIdentifier(tempName),
        });
        i += 1;
        continue;
      }
      return null;
    }
    return [...temps, ...assigns];
  }
  return null;
}

function objectLiteralPropertyValue(
  expr: ts.Expression,
  name: string,
): ts.Expression | null {
  if (!ts.isObjectLiteralExpression(expr)) {
    return null;
  }
  for (const prop of expr.properties) {
    if (ts.isShorthandPropertyAssignment(prop) && prop.name.text === name) {
      return prop.name;
    }
    if (
      ts.isPropertyAssignment(prop) &&
      ((ts.isIdentifier(prop.name) && prop.name.text === name) ||
        (ts.isStringLiteral(prop.name) && prop.name.text === name)) &&
      ts.isExpression(prop.initializer)
    ) {
      return prop.initializer;
    }
  }
  return null;
}

function writtenLetNamesInStatement(
  stmt: ts.Statement,
  letNames: ReadonlySet<string>,
): Set<string> {
  const found = new Set<string>();
  const visitTarget = (node: ts.Node): void => {
    node = ts.isExpression(node) ? unwrapExpression(node) : node;
    if (ts.isIdentifier(node) && letNames.has(node.text)) {
      found.add(node.text);
      return;
    }
    if (ts.isArrayLiteralExpression(node)) {
      for (const element of node.elements) {
        visitTarget(ts.isSpreadElement(element) ? element.expression : element);
      }
      return;
    }
    if (ts.isObjectLiteralExpression(node)) {
      for (const prop of node.properties) {
        if (ts.isShorthandPropertyAssignment(prop)) {
          if (letNames.has(prop.name.text)) {
            found.add(prop.name.text);
          }
        } else if (ts.isPropertyAssignment(prop)) {
          visitTarget(prop.initializer);
        }
      }
    }
  };
  const walk = (node: ts.Node): void => {
    if (
      ts.isBinaryExpression(node) &&
      isAssignmentOperator(node.operatorToken.kind)
    ) {
      visitTarget(node.left);
      walk(node.right);
      return;
    }
    if (ts.isPrefixUnaryExpression(node) || ts.isPostfixUnaryExpression(node)) {
      if (
        node.operator === ts.SyntaxKind.PlusPlusToken ||
        node.operator === ts.SyntaxKind.MinusMinusToken
      ) {
        visitTarget(node.operand);
        return;
      }
    }
    ts.forEachChild(node, walk);
  };
  walk(stmt);
  return found;
}

function declaredIdentifierNames(
  declList: ts.VariableDeclarationList,
): ReadonlySet<string> | null {
  const names = new Set<string>();
  for (const decl of declList.declarations) {
    if (!ts.isIdentifier(decl.name) || !decl.initializer) {
      return null;
    }
    names.add(decl.name.text);
  }
  return names;
}

interface LoweredPreludeBindings {
  letStmts: IR1Stmt[];
  ruleDecls: PropResult[];
  scopedParams: ReadonlyMap<string, string>;
  arms: ReadonlyArray<readonly [OpaqueExpr, OpaqueExpr]>;
  fallthroughFacts: Fact[];
}

function mappedFunctionReturnSort(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell?: SynthCell,
): string | undefined {
  const sig = checker.getSignatureFromDeclaration(node);
  if (sig === undefined) {
    return undefined;
  }
  const mapped = mapTsType(sig.getReturnType(), checker, strategy, synthCell);
  return mapped.ok ? mapped.sort : undefined;
}

function lowerPreludeBindings(
  bindings: PreludeStmt[],
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  baseParams: ReadonlyMap<string, string>,
  supply: UniqueSupply,
  env: AssumptionEnv,
  state?: SymbolicState,
  expectedReturnSort?: string,
): LoweredPreludeBindings | { error: string } {
  const validation = validatePreludeBindings(bindings, checker);
  if (validation !== null) {
    return validation;
  }

  // Phase 2: translate initializers as a left fold, threading scopedParams
  // through the accumulator. Const and μ-search bindings become IR1Let
  // statements; early-return arms are accumulated alongside.
  type Acc =
    | {
        tag: "ok";
        scopedParams: ReadonlyMap<string, string>;
        letStmts: IR1Stmt[];
        ruleDecls: PropResult[];
        arms: ReadonlyArray<readonly [OpaqueExpr, OpaqueExpr]>;
        fallthroughFacts: Fact[];
      }
    | { tag: "error"; error: string };

  const folded = bindings.reduce<Acc>(
    (acc, binding) => {
      if (acc.tag === "error") {
        return acc;
      }
      if (binding.kind === "earlyReturn") {
        const currentFact = recognizeBranchFact(binding.predicateExpr, checker);
        const predRes = translateBindingInit(
          binding.predicateExpr,
          checker,
          strategy,
          acc.scopedParams,
          state,
          supply,
          env,
        );
        if ("error" in predRes) {
          return { tag: "error", error: predRes.error };
        }
        enterFrame(env);
        let valRes: BindingInitResult;
        try {
          for (const fact of [...acc.fallthroughFacts, currentFact]) {
            if (fact !== null) {
              pushFact(env, fact);
            }
          }
          if (binding.nestedBlock !== undefined) {
            const value = lowerNestedPureBlockReturnToL1(binding.nestedBlock, {
              checker,
              strategy,
              paramNames: acc.scopedParams,
              state: state ?? makeSymbolicState(),
              supply,
              env,
              ...(expectedReturnSort === undefined
                ? {}
                : { expectedReturnSort }),
            });
            valRes =
              value === null
                ? {
                    error:
                      "if-with-return block must contain only const bindings followed by a return",
                  }
                : {
                    value: applyOpaqueAliases(lowerL1ToOpaque(value), supply),
                  };
          } else if (
            binding.valueExpr !== undefined &&
            binding.blockBindings.length === 0
          ) {
            const definednessSnapshot = supply.synthCell
              ? [...supply.synthCell.definednessObligations]
              : null;
            const l1Value =
              currentFact?.kind === "non-null" ||
              (currentFact?.kind === "predicate" &&
                currentFact.typePredicate !== undefined)
                ? buildL1SubExpr(binding.valueExpr, {
                    checker,
                    strategy,
                    paramNames: acc.scopedParams,
                    state: state ?? makeSymbolicState(),
                    supply,
                    env,
                    ...(expectedReturnSort === undefined
                      ? {}
                      : { expectedSort: expectedReturnSort }),
                  })
                : null;
            valRes =
              l1Value !== null &&
              !isUnsupported(l1Value) &&
              !isL1Unsupported(l1Value)
                ? {
                    value: applyOpaqueAliases(lowerL1ToOpaque(l1Value), supply),
                  }
                : (() => {
                    if (supply.synthCell && definednessSnapshot !== null) {
                      supply.synthCell.definednessObligations =
                        definednessSnapshot;
                    }
                    return translateBindingInit(
                      binding.valueExpr,
                      checker,
                      strategy,
                      acc.scopedParams,
                      state,
                      supply,
                      env,
                    );
                  })();
          } else if (binding.valueExpr !== undefined) {
            valRes = translateEarlyReturnBlockValue(
              binding.blockBindings,
              binding.valueExpr,
              {
                checker,
                strategy,
                scopedParams: acc.scopedParams,
                state: state ?? makeSymbolicState(),
                supply,
                env,
                ...(expectedReturnSort === undefined
                  ? {}
                  : { expectedReturnSort }),
              },
            );
          } else {
            valRes = {
              error:
                "if-with-return block must contain only const bindings followed by a return",
            };
          }
        } finally {
          exitFrame(env);
        }
        if ("error" in valRes) {
          return { tag: "error", error: valRes.error };
        }
        return {
          tag: "ok",
          scopedParams: acc.scopedParams,
          letStmts: acc.letStmts,
          ruleDecls: acc.ruleDecls,
          arms: [...acc.arms, [predRes.value, valRes.value] as const],
          fallthroughFacts: [
            ...acc.fallthroughFacts,
            ...(currentFact === null ? [] : [negateFact(currentFact)]),
          ],
        };
      }
      if (binding.kind === "reassign") {
        const value = buildL1SubExpr(binding.valueExpr, {
          checker,
          strategy,
          paramNames: acc.scopedParams,
          state: state ?? makeSymbolicState(),
          supply,
          env,
        });
        if (isUnsupported(value)) {
          return { tag: "error", error: value.unsupported };
        }
        const localName = acc.scopedParams.get(binding.tsName);
        if (localName === undefined) {
          return {
            tag: "error",
            error: `assignment to unknown local binding ${binding.tsName}`,
          };
        }
        return {
          tag: "ok",
          scopedParams: acc.scopedParams,
          letStmts: [...acc.letStmts, ir1Assign(ir1Var(localName), value)],
          ruleDecls: acc.ruleDecls,
          arms: acc.arms,
          fallthroughFacts: acc.fallthroughFacts,
        };
      }
      if (binding.kind === "stmt") {
        const scopedParams = new Map(acc.scopedParams);
        const built = buildSupportedSsaStatement(binding.stmt, {
          checker,
          strategy,
          paramNames: scopedParams,
          state: state ?? makeSymbolicState(),
          supply,
          env,
        });
        if (isUnsupported(built)) {
          return { tag: "error", error: built.unsupported };
        }
        return {
          tag: "ok",
          scopedParams,
          letStmts: [...acc.letStmts, built],
          ruleDecls: acc.ruleDecls,
          arms: acc.arms,
          fallthroughFacts: acc.fallthroughFacts,
        };
      }
      if (binding.kind === "forOf") {
        return {
          tag: "error",
          error:
            "for-of loop is only supported as a build-list comprehension returned directly",
        };
      }
      if (binding.kind === "builderPush") {
        return {
          tag: "error",
          error:
            "list builder push is only supported when returning its accumulator directly",
        };
      }
      if (binding.kind === "builderSetAdd") {
        return {
          tag: "error",
          error:
            "Set builder add is only supported when returning its accumulator directly",
        };
      }
      const localName =
        binding.localName ??
        allocLocalBindingName(
          supply,
          toPantTermName(binding.tsName),
          acc.scopedParams,
        );
      const returnTypeResult =
        binding.kind === "const"
          ? mapTsType(
              checker.getTypeAtLocation(binding.initializer),
              checker,
              strategy,
              supply.synthCell,
            )
          : okSort(strategy.mapNumber());
      if (!returnTypeResult.ok) {
        return {
          tag: "error",
          error: `${binding.tsName}: ${returnTypeResult.reason}`,
        };
      }
      const returnType = returnTypeResult.sort;
      const initExpr =
        binding.kind === "const"
          ? buildL1SubExpr(binding.initializer, {
              checker,
              strategy,
              paramNames: acc.scopedParams,
              state: state ?? makeSymbolicState(),
              supply,
              env,
            })
          : buildL1MuSearchCombTyped(binding.mu, {
              checker,
              strategy,
              paramNames: acc.scopedParams,
              state,
              supply,
              env,
            });
      if (isUnsupported(initExpr) || isL1Unsupported(initExpr)) {
        if (
          binding.kind === "const" &&
          state === undefined &&
          initExpr.unsupported.startsWith(
            "unsupported mutating-body sub-expression:",
          )
        ) {
          return {
            tag: "error",
            error: "unsupported pure expression in const initializer",
          };
        }
        return { tag: "error", error: initExpr.unsupported };
      }
      return {
        tag: "ok",
        scopedParams: withParam(acc.scopedParams, binding.tsName, localName),
        ruleDecls: [
          ...acc.ruleDecls,
          {
            kind: "rule-decl",
            ruleName: localName,
            params: [],
            returnType: getAst().tName(returnType),
          },
        ],
        letStmts: [...acc.letStmts, ir1Let(localName, initExpr)],
        arms: acc.arms,
        fallthroughFacts: acc.fallthroughFacts,
      };
    },
    {
      tag: "ok",
      scopedParams: baseParams,
      letStmts: [],
      ruleDecls: [],
      arms: [],
      fallthroughFacts: [],
    },
  );

  if (folded.tag === "error") {
    return { error: folded.error };
  }
  return {
    letStmts: folded.letStmts,
    ruleDecls: folded.ruleDecls,
    scopedParams: folded.scopedParams,
    arms: folded.arms,
    fallthroughFacts: folded.fallthroughFacts,
  };
}

function validatePreludeBindings(
  bindings: readonly PreludeStmt[],
  checker: ts.TypeChecker,
): { error: string } | null {
  // Phase 1: TDZ validation — reject forward/self references on TS AST.
  // For μ-search bindings, validate both the init and the predicate; the
  // predicate may reference its own counter (that's the loop), so the
  // counter is removed from the blocked set when checking the predicate.
  // Side-effectful μ-search init/predicate and early-return predicate/value
  // are also rejected here: translateBodyExpr has no explicit
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
    } else if (binding.kind === "reassign") {
      if (expressionReferencesNames(binding.valueExpr, blockedNames)) {
        return { error: "reassignment value references a later binding" };
      }
    } else if (binding.kind === "stmt") {
      if (nodeReferencesNames(binding.stmt, blockedNames)) {
        return { error: "statement before return references a later binding" };
      }
    } else if (binding.kind === "muSearch") {
      if (expressionHasSideEffects(binding.mu.initTsExpr, checker)) {
        return { error: "while-loop init has side effects" };
      }
      if (
        expressionHasSideEffects(binding.mu.predicateTsExpr, checker, {
          admitForeignBoolPredicates: true,
        })
      ) {
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
    } else if (binding.kind === "forOf") {
    } else if (binding.kind === "builderPush") {
      if (expressionReferencesNames(binding.valueExpr, blockedNames)) {
        return { error: "list builder push references a later binding" };
      }
    } else if (binding.kind === "builderSetAdd") {
      if (expressionReferencesNames(binding.valueExpr, blockedNames)) {
        return { error: "Set builder add references a later binding" };
      }
    } else {
      // earlyReturn arm — predicate and value must be pure and may not refer
      // to bindings declared after this point. The arm itself binds nothing,
      // so `blockedNames` here just contains the names of later const /
      // μ-search bindings (earlyReturn entries contribute nothing).
      if (
        expressionHasSideEffects(binding.predicateExpr, checker, {
          admitForeignBoolPredicates: true,
        })
      ) {
        return { error: "early-return predicate has side effects" };
      }
      for (const [blockIdx, blockBinding] of binding.blockBindings.entries()) {
        if (expressionHasSideEffects(blockBinding.initializer, checker)) {
          return { error: "early-return block const has side effects" };
        }
        const blockBlockedNames = new Set(
          binding.blockBindings.slice(blockIdx).map((b) => b.tsName),
        );
        if (
          expressionReferencesNames(blockBinding.initializer, blockBlockedNames)
        ) {
          return {
            error:
              "early-return block const initializer references a later binding",
          };
        }
        if (expressionReferencesNames(blockBinding.initializer, blockedNames)) {
          return {
            error:
              "early-return block const initializer references a later binding",
          };
        }
      }
      if (expressionReferencesNames(binding.predicateExpr, blockedNames)) {
        return { error: "early-return predicate references a later binding" };
      }
      if (binding.valueExpr !== undefined) {
        if (
          expressionHasSideEffects(binding.valueExpr, checker, {
            admitForeignBoolPredicates: true,
          })
        ) {
          return { error: "early-return value has side effects" };
        }
        if (expressionReferencesNames(binding.valueExpr, blockedNames)) {
          return { error: "early-return value references a later binding" };
        }
      }
      if (
        binding.nestedBlock !== undefined &&
        nodeReferencesNames(binding.nestedBlock, blockedNames)
      ) {
        return { error: "early-return value references a later binding" };
      }
    }
  }
  return null;
}

type BindingInitResult = { value: OpaqueExpr } | { error: string };

function translateEarlyReturnBlockValue(
  bindings: readonly ExtractedBlockConstBinding[],
  valueExpr: ts.Expression,
  ctx: {
    checker: ts.TypeChecker;
    strategy: NumericStrategy;
    scopedParams: ReadonlyMap<string, string>;
    state: SymbolicState;
    supply: UniqueSupply;
    env: AssumptionEnv;
  } & { expectedReturnSort?: string },
): BindingInitResult {
  let scopedParams: ReadonlyMap<string, string> = new Map(ctx.scopedParams);
  const substitutions: Array<{
    localName: string;
    value: IR1Expr;
  }> = [];

  for (const binding of bindings) {
    const localName = freshHygienicBinder(ctx.supply);
    const initExpr = buildL1SubExpr(binding.initializer, {
      checker: ctx.checker,
      strategy: ctx.strategy,
      paramNames: scopedParams,
      state: ctx.state,
      supply: ctx.supply,
      env: ctx.env,
    });
    if (isUnsupported(initExpr) || isL1Unsupported(initExpr)) {
      return { error: initExpr.unsupported };
    }
    substitutions.push({ localName, value: initExpr });
    scopedParams = withParam(scopedParams, binding.tsName, localName);
  }

  const value = buildL1SubExpr(valueExpr, {
    checker: ctx.checker,
    strategy: ctx.strategy,
    paramNames: scopedParams,
    state: ctx.state,
    supply: ctx.supply,
    env: ctx.env,
    ...(ctx.expectedReturnSort === undefined
      ? {}
      : { expectedSort: ctx.expectedReturnSort }),
  });
  if (isUnsupported(value) || isL1Unsupported(value)) {
    return { error: value.unsupported };
  }

  const inlined = substitutions.reduceRight(
    (acc, binding) =>
      substituteIR1ExprSubtree(acc, ir1Var(binding.localName), binding.value),
    value,
  );
  return { value: applyOpaqueAliases(lowerL1ToOpaque(inlined), ctx.supply) };
}

export interface NestedPureBlockReturnContext {
  checker: ts.TypeChecker;
  strategy: NumericStrategy;
  paramNames: ReadonlyMap<string, string>;
  state?: SymbolicState;
  supply: UniqueSupply;
  env: AssumptionEnv;
  expectedReturnSort?: string;
}

/**
 * Lower a TS block as one nested pure return value.
 *
 * This is the recursive block-return primitive used by later branch,
 * switch, and callback consumers. It shares `extractReturnExpression` for
 * shape recognition, `validatePreludeBindings` for TDZ/side-effect refusal,
 * L1 if-conversion for terminal control flow, and IR1 capture-avoiding
 * substitution for local const/μ let-elimination. Unsupported statement
 * sequencing returns `null` rather than leaking a partially lowered value.
 */
export function lowerNestedPureBlockReturn(
  block: ts.Block,
  ctx: NestedPureBlockReturnContext,
): BodyResult | null {
  const lowered = lowerNestedPureBlockReturnToL1(block, ctx);
  if (lowered === null) {
    return null;
  }
  return { expr: applyOpaqueAliases(lowerL1ToOpaque(lowered), ctx.supply) };
}

export function lowerNestedPureBlockReturnToL1(
  block: ts.Block,
  ctx: NestedPureBlockReturnContext,
): IR1Expr | null {
  const extracted = extractReturnExpression(block, {
    checker: ctx.checker,
    strategy: ctx.strategy,
    paramNames: ctx.paramNames,
    state: ctx.state,
    supply: ctx.supply,
    env: ctx.env,
    ...(ctx.expectedReturnSort === undefined
      ? {}
      : { expectedSort: ctx.expectedReturnSort }),
  });
  if (extracted === null) {
    return null;
  }
  if (
    extracted.bindings.some(
      (binding) => binding.kind === "reassign" || binding.kind === "stmt",
    )
  ) {
    return null;
  }
  if (
    extracted.bindings.some(
      (binding) =>
        binding.kind === "builderPush" || binding.kind === "builderSetAdd",
    )
  ) {
    return null;
  }
  const validation = validatePreludeBindings(extracted.bindings, ctx.checker);
  if (validation !== null) {
    return null;
  }
  if (
    extracted.bindings.some(
      (binding) =>
        binding.kind === "const" &&
        expressionHasSideEffects(binding.initializer, ctx.checker),
    )
  ) {
    return null;
  }

  let scopedParams: ReadonlyMap<string, string> = ctx.paramNames;
  const substitutions: Array<{ localName: string; value: IR1Expr }> = [];
  const arms: Array<readonly [IR1Expr, IR1Expr]> = [];
  const fallthroughFacts: Fact[] = [];

  for (const binding of extracted.bindings) {
    if (
      binding.kind === "forOf" ||
      binding.kind === "builderPush" ||
      binding.kind === "builderSetAdd" ||
      binding.kind === "reassign" ||
      binding.kind === "stmt"
    ) {
      return null;
    }
    if (binding.kind === "earlyReturn") {
      const currentFact = recognizeBranchFact(
        binding.predicateExpr,
        ctx.checker,
      );
      const guard = withNestedFactFrame(ctx.env, fallthroughFacts, () =>
        buildL1SubExpr(binding.predicateExpr, {
          checker: ctx.checker,
          strategy: ctx.strategy,
          paramNames: scopedParams,
          state: ctx.state ?? makeSymbolicState(),
          supply: ctx.supply,
          env: ctx.env,
        }),
      );
      if (isUnsupported(guard) || isL1Unsupported(guard)) {
        return null;
      }
      const value = withNestedFactFrame(
        ctx.env,
        [...fallthroughFacts, currentFact],
        () => {
          if (binding.nestedBlock !== undefined) {
            return lowerNestedPureBlockReturnToL1(binding.nestedBlock, {
              checker: ctx.checker,
              strategy: ctx.strategy,
              paramNames: scopedParams,
              state: ctx.state ?? makeSymbolicState(),
              supply: ctx.supply,
              env: ctx.env,
              ...(ctx.expectedReturnSort === undefined
                ? {}
                : { expectedReturnSort: ctx.expectedReturnSort }),
            });
          }
          if (binding.valueExpr !== undefined) {
            return lowerEarlyReturnArmValueToL1(
              binding.blockBindings,
              binding.valueExpr,
              {
                checker: ctx.checker,
                strategy: ctx.strategy,
                paramNames: scopedParams,
                state: ctx.state ?? makeSymbolicState(),
                supply: ctx.supply,
                env: ctx.env,
                ...(ctx.expectedReturnSort === undefined
                  ? {}
                  : { expectedReturnSort: ctx.expectedReturnSort }),
              },
            );
          }
          return null;
        },
      );
      if (value === null) {
        return null;
      }
      arms.push([guard, value] as const);
      if (currentFact !== null) {
        fallthroughFacts.push(negateFact(currentFact));
      }
      continue;
    }

    const localName = freshHygienicBinder(ctx.supply);
    const value = withNestedFactFrame(ctx.env, fallthroughFacts, () =>
      binding.kind === "const"
        ? buildL1SubExpr(binding.initializer, {
            checker: ctx.checker,
            strategy: ctx.strategy,
            paramNames: scopedParams,
            state: ctx.state ?? makeSymbolicState(),
            supply: ctx.supply,
            env: ctx.env,
          })
        : buildL1MuSearchCombTyped(binding.mu, {
            checker: ctx.checker,
            strategy: ctx.strategy,
            paramNames: scopedParams,
            state: ctx.state ?? makeSymbolicState(),
            supply: ctx.supply,
            env: ctx.env,
          }),
    );
    if (isUnsupported(value) || isL1Unsupported(value)) {
      return null;
    }
    substitutions.push({ localName, value });
    scopedParams = withParam(scopedParams, binding.tsName, localName);
  }

  const terminal = withNestedFactFrame(ctx.env, fallthroughFacts, () =>
    lowerNestedPureTerminalToL1(extracted.returnExpr, ctx, scopedParams),
  );
  if (terminal === null) {
    return null;
  }
  const withArms =
    arms.length === 0
      ? terminal
      : ir1Cond(
          arms as [readonly [IR1Expr, IR1Expr], ...typeof arms],
          terminal,
        );
  return substitutions.reduceRight(
    (acc, binding) =>
      substituteIR1ExprSubtree(acc, ir1Var(binding.localName), binding.value),
    withArms,
  );
}

function withNestedFactFrame<T>(
  env: AssumptionEnv,
  facts: ReadonlyArray<Fact | null>,
  fn: () => T,
): T {
  enterFrame(env);
  try {
    for (const fact of facts) {
      if (fact !== null) {
        pushFact(env, fact);
      }
    }
    return fn();
  } finally {
    exitFrame(env);
  }
}

function lowerEarlyReturnArmValueToL1(
  bindings: readonly ExtractedBlockConstBinding[],
  valueExpr: ts.Expression,
  ctx: {
    checker: ts.TypeChecker;
    strategy: NumericStrategy;
    paramNames: ReadonlyMap<string, string>;
    state: SymbolicState;
    supply: UniqueSupply;
    env: AssumptionEnv;
  } & { expectedReturnSort?: string },
): IR1Expr | null {
  let scopedParams: ReadonlyMap<string, string> = new Map(ctx.paramNames);
  const substitutions: Array<{ localName: string; value: IR1Expr }> = [];
  for (const binding of bindings) {
    const localName = freshHygienicBinder(ctx.supply);
    const value = buildL1SubExpr(binding.initializer, {
      checker: ctx.checker,
      strategy: ctx.strategy,
      paramNames: scopedParams,
      state: ctx.state,
      supply: ctx.supply,
      env: ctx.env,
    });
    if (isUnsupported(value) || isL1Unsupported(value)) {
      return null;
    }
    substitutions.push({ localName, value });
    scopedParams = withParam(scopedParams, binding.tsName, localName);
  }
  const value = buildL1SubExpr(valueExpr, {
    checker: ctx.checker,
    strategy: ctx.strategy,
    paramNames: scopedParams,
    state: ctx.state,
    supply: ctx.supply,
    env: ctx.env,
    ...(ctx.expectedReturnSort === undefined
      ? {}
      : { expectedSort: ctx.expectedReturnSort }),
  });
  if (isUnsupported(value) || isL1Unsupported(value)) {
    return null;
  }
  return substitutions.reduceRight(
    (acc, binding) =>
      substituteIR1ExprSubtree(acc, ir1Var(binding.localName), binding.value),
    value,
  );
}

function lowerNestedPureTerminalToL1(
  terminal: ts.Expression | ts.IfStatement | ts.SwitchStatement,
  ctx: NestedPureBlockReturnContext,
  scopedParams: ReadonlyMap<string, string>,
): IR1Expr | null {
  const l1Ctx = {
    checker: ctx.checker,
    strategy: ctx.strategy,
    paramNames: scopedParams,
    state: ctx.state ?? makeSymbolicState(),
    supply: ctx.supply,
    env: ctx.env,
    ...(ctx.expectedReturnSort === undefined
      ? {}
      : { expectedSort: ctx.expectedReturnSort }),
  };
  if (ts.isIfStatement(terminal)) {
    return lowerNestedPureIfStatementToL1(terminal, ctx, scopedParams);
  }
  if (ts.isSwitchStatement(terminal)) {
    const value = buildL1Conditional(terminal, l1Ctx);
    return isL1Unsupported(value) ? null : value;
  }
  const value = buildL1SubExpr(terminal, l1Ctx);
  return isUnsupported(value) || isL1Unsupported(value) ? null : value;
}

function lowerNestedPureIfStatementToL1(
  stmt: ts.IfStatement,
  ctx: NestedPureBlockReturnContext,
  scopedParams: ReadonlyMap<string, string>,
): IR1Expr | null {
  const arms: Array<readonly [IR1Expr, IR1Expr]> = [];
  let current: ts.IfStatement = stmt;
  const elsePathFacts: Fact[] = [];

  while (true) {
    if (!current.elseStatement) {
      return null;
    }
    const currentFact = recognizeBranchFact(current.expression, ctx.checker);
    const guard = lowerWithFactsFrame(ctx.env, elsePathFacts, () =>
      buildL1SubExpr(current.expression, {
        checker: ctx.checker,
        strategy: ctx.strategy,
        paramNames: scopedParams,
        state: ctx.state ?? makeSymbolicState(),
        supply: ctx.supply,
        env: ctx.env,
      }),
    );
    if (isUnsupported(guard) || isL1Unsupported(guard)) {
      return null;
    }

    const thenValue = lowerWithFactsFrame(
      ctx.env,
      [...elsePathFacts, currentFact],
      () =>
        lowerNestedPureBranchStatementToL1(
          current.thenStatement,
          ctx,
          scopedParams,
        ),
    );
    if (thenValue === null) {
      return null;
    }
    arms.push([guard, thenValue] as const);

    const elseFact = currentFact === null ? null : negateFact(currentFact);
    if (ts.isIfStatement(current.elseStatement)) {
      if (elseFact !== null) {
        elsePathFacts.push(elseFact);
      }
      current = current.elseStatement;
      continue;
    }

    const elseValue = lowerWithFactsFrame(
      ctx.env,
      [...elsePathFacts, elseFact],
      () =>
        lowerNestedPureBranchStatementToL1(
          current.elseStatement!,
          ctx,
          scopedParams,
        ),
    );
    if (elseValue === null || arms.length === 0) {
      return null;
    }
    return ir1Cond(
      arms as [readonly [IR1Expr, IR1Expr], ...typeof arms],
      elseValue,
    );
  }
}

function lowerNestedPureBranchStatementToL1(
  branch: ts.Statement,
  ctx: NestedPureBlockReturnContext,
  scopedParams: ReadonlyMap<string, string>,
): IR1Expr | null {
  if (ts.isReturnStatement(branch) && branch.expression !== undefined) {
    const value = buildL1SubExpr(branch.expression, {
      checker: ctx.checker,
      strategy: ctx.strategy,
      paramNames: scopedParams,
      state: ctx.state ?? makeSymbolicState(),
      supply: ctx.supply,
      env: ctx.env,
      ...(ctx.expectedReturnSort === undefined
        ? {}
        : { expectedSort: ctx.expectedReturnSort }),
    });
    return isUnsupported(value) || isL1Unsupported(value) ? null : value;
  }
  if (ts.isBlock(branch)) {
    return lowerNestedPureBlockReturnToL1(branch, {
      checker: ctx.checker,
      strategy: ctx.strategy,
      paramNames: scopedParams,
      state: ctx.state ?? makeSymbolicState(),
      supply: ctx.supply,
      env: ctx.env,
      ...(ctx.expectedReturnSort === undefined
        ? {}
        : { expectedReturnSort: ctx.expectedReturnSort }),
    });
  }
  if (ts.isIfStatement(branch)) {
    return lowerNestedPureIfStatementToL1(branch, ctx, scopedParams);
  }
  return null;
}

function lowerWithFactsFrame<T>(
  env: AssumptionEnv,
  facts: ReadonlyArray<Fact | null>,
  lower: () => T,
): T {
  if (facts.every((fact) => fact === null)) {
    return lower();
  }
  enterFrame(env);
  try {
    for (const fact of facts) {
      if (fact !== null) {
        pushFact(env, fact);
      }
    }
    return lower();
  } finally {
    exitFrame(env);
  }
}

function translateBindingInit(
  initializer: ts.Expression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  scopedParams: ReadonlyMap<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
  env: AssumptionEnv,
): BindingInitResult {
  const result = translateBodyExpr(
    initializer,
    checker,
    strategy,
    scopedParams,
    state,
    supply,
    env,
  );
  if (isBodyUnsupported(result)) {
    return { error: result.unsupported };
  }
  return { value: bodyExpr(result) };
}

export function isGuardStatement(
  stmt: ts.Statement,
  checker: ts.TypeChecker,
): boolean {
  // Assertion call or followable guard call — must match the same purity
  // checks used by scanBodyForGuards in translate-signature.ts
  if (ts.isExpressionStatement(stmt) && ts.isCallExpression(stmt.expression)) {
    const call = stmt.expression;
    if (!isEffectFree(call.expression, checker)) {
      return false;
    }
    if (!call.arguments.every((arg) => isEffectFree(arg, checker))) {
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
  // Condition must be effect-free, aligned with classifyGuardIf.
  if (!isEffectFree(stmt.expression, checker)) {
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

const ASSIGNMENT_OPERATORS = new Set<ts.SyntaxKind>([
  ts.SyntaxKind.EqualsToken,
  ts.SyntaxKind.PlusEqualsToken,
  ts.SyntaxKind.MinusEqualsToken,
  ts.SyntaxKind.AsteriskEqualsToken,
  ts.SyntaxKind.AsteriskAsteriskEqualsToken,
  ts.SyntaxKind.SlashEqualsToken,
  ts.SyntaxKind.PercentEqualsToken,
  ts.SyntaxKind.LessThanLessThanEqualsToken,
  ts.SyntaxKind.GreaterThanGreaterThanEqualsToken,
  ts.SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken,
  ts.SyntaxKind.AmpersandEqualsToken,
  ts.SyntaxKind.BarEqualsToken,
  ts.SyntaxKind.CaretEqualsToken,
  ts.SyntaxKind.BarBarEqualsToken,
  ts.SyntaxKind.AmpersandAmpersandEqualsToken,
  ts.SyntaxKind.QuestionQuestionEqualsToken,
]);

function isAssignmentOperator(kind: ts.SyntaxKind): boolean {
  return ASSIGNMENT_OPERATORS.has(kind);
}

function findClosureCapturedReassignedNamesInBody(
  body: ts.Block,
  declaredNames: ReadonlySet<string>,
): ReadonlySet<string> {
  const captured = new Set<string>();
  const targetNames = new Set(declaredNames);

  const addIfTarget = (
    name: string,
    shadowed: ReadonlySet<string>,
    inClosure: boolean,
  ): void => {
    if (inClosure && targetNames.has(name) && !shadowed.has(name)) {
      captured.add(name);
    }
  };

  const collectAssignmentTarget = (
    node: ts.Node,
    shadowed: ReadonlySet<string>,
    inClosure: boolean,
  ): void => {
    node = ts.isExpression(node) ? unwrapExpression(node) : node;
    if (ts.isIdentifier(node)) {
      addIfTarget(node.text, shadowed, inClosure);
      return;
    }
    if (ts.isArrayLiteralExpression(node)) {
      for (const element of node.elements) {
        collectAssignmentTarget(
          ts.isSpreadElement(element) ? element.expression : element,
          shadowed,
          inClosure,
        );
      }
      return;
    }
    if (ts.isObjectLiteralExpression(node)) {
      for (const prop of node.properties) {
        if (ts.isShorthandPropertyAssignment(prop)) {
          addIfTarget(prop.name.text, shadowed, inClosure);
        } else if (ts.isPropertyAssignment(prop)) {
          collectAssignmentTarget(prop.initializer, shadowed, inClosure);
        } else if (ts.isSpreadAssignment(prop)) {
          collectAssignmentTarget(prop.expression, shadowed, inClosure);
        }
      }
    }
  };

  const walk = (
    node: ts.Node,
    shadowed: ReadonlySet<string>,
    inClosure: boolean,
  ): void => {
    if (captured.size === targetNames.size) {
      return;
    }
    if (
      ts.isArrowFunction(node) ||
      ts.isFunctionExpression(node) ||
      ts.isFunctionDeclaration(node) ||
      ts.isMethodDeclaration(node) ||
      ts.isGetAccessorDeclaration(node) ||
      ts.isSetAccessorDeclaration(node) ||
      ts.isConstructorDeclaration(node)
    ) {
      for (const param of node.parameters) {
        if (param.initializer) {
          walk(param.initializer, shadowed, inClosure);
        }
      }
      const innerShadowed = new Set(shadowed);
      for (const param of node.parameters) {
        collectBindingNames(param.name, innerShadowed);
      }
      if (
        (ts.isFunctionExpression(node) || ts.isFunctionDeclaration(node)) &&
        node.name
      ) {
        innerShadowed.add(node.name.text);
      }
      if (node.body) {
        walk(node.body, innerShadowed, true);
      }
      return;
    }
    if (ts.isBlock(node)) {
      const blockShadowed = new Set(shadowed);
      for (const stmt of node.statements) {
        walk(stmt, blockShadowed, inClosure);
        if (ts.isVariableStatement(stmt)) {
          const flags = stmt.declarationList.flags;
          if (flags & ts.NodeFlags.Let || flags & ts.NodeFlags.Const) {
            for (const decl of stmt.declarationList.declarations) {
              collectBindingNames(decl.name, blockShadowed);
            }
          }
        }
      }
      return;
    }
    if (ts.isVariableStatement(node)) {
      for (const decl of node.declarationList.declarations) {
        if (decl.initializer) {
          walk(decl.initializer, shadowed, inClosure);
        }
      }
      return;
    }
    if (ts.isVariableDeclaration(node)) {
      if (node.initializer) {
        walk(node.initializer, shadowed, inClosure);
      }
      return;
    }
    if (
      ts.isBinaryExpression(node) &&
      isAssignmentOperator(node.operatorToken.kind)
    ) {
      collectAssignmentTarget(node.left, shadowed, inClosure);
      walk(node.right, shadowed, inClosure);
      return;
    }
    if (ts.isPrefixUnaryExpression(node) || ts.isPostfixUnaryExpression(node)) {
      if (
        node.operator === ts.SyntaxKind.PlusPlusToken ||
        node.operator === ts.SyntaxKind.MinusMinusToken
      ) {
        collectAssignmentTarget(node.operand, shadowed, inClosure);
        return;
      }
    }
    ts.forEachChild(node, (child) => walk(child, shadowed, inClosure));
  };

  for (const stmt of body.statements) {
    walk(stmt, new Set(), false);
  }
  return captured;
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
  env: AssumptionEnv = createL1AssumptionEnv(),
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
        env,
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
      env,
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

  if (ts.isExpression(expr)) {
    const l1 = tryBuildL1PureSubExpression(expr, {
      checker,
      strategy,
      paramNames,
      state,
      supply,
      env,
    });
    if (l1 !== null) {
      if (isL1Unsupported(l1)) {
        return { unsupported: l1.unsupported };
      }
      return { expr: lowerL1ToOpaque(l1) };
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
      { checker, strategy, paramNames, state, supply, env },
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
          env,
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
      env,
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
      env,
    };
    const member = buildL1MemberAccess(expr, l1Ctx);
    if ("unsupported" in member) {
      return member;
    }
    return { expr: lowerL1ToOpaque(member) };
  }

  // Call expression: handle .includes(), .filter().map(), etc.
  // A Map.set/delete call returns a `{ effect }` BodyResult which only
  // the SSA mutating-body builder consumes as a statement; in any expression context
  // (ternary arm, binary operand, arg, etc.) turn it into `{ unsupported }`
  // so downstream `bodyExpr()` calls see a narrow result and never throw.
  if (ts.isCallExpression(expr)) {
    return rejectEffect(
      translateCallExpr(
        expr,
        checker,
        strategy,
        paramNames,
        state,
        supply,
        env,
      ),
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
      env,
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
    // (Dafny Reference Manual §"Nullable Types"). See AGENTS.md "Option-
    // Type Elimination" for the broader encoding.
    if (expr.operatorToken.kind === ts.SyntaxKind.QuestionQuestionToken) {
      // Use the operand's *declared* type (narrowing-free) for the
      // nullability classifications. Inside a flow-narrowed branch
      // — `if (x !== null) return x ?? y;` — `getTypeAtLocation`
      // would return the narrowed `T` and silently emit a bare `x`,
      // but the Pant symbol for `x` is still the list-lifted `[T]`
      // parameter (ts2pant doesn't track flow narrowing on the Pant
      // side), so the typecheck would fail. Same fix the
      // `NonNullExpression` branch and the L1 `??` lowering use; see
      // `getOperandDeclaredType` in nullish-recognizer.ts.
      const leftTsType = getOperandDeclaredType(expr.left, checker);
      const leftResult = translateBodyExpr(
        expr.left,
        checker,
        strategy,
        paramNames,
        state,
        supply,
        env,
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
        env,
      );
      if (isBodyUnsupported(rightResult)) {
        return rightResult;
      }
      const xExpr = bodyExpr(leftResult);
      const yExpr = bodyExpr(rightResult);
      const rightTsType = getOperandDeclaredType(expr.right, checker);
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
    // `BinOp(eq | neq, ...)`. Sub-expressions reach L1 natively
    // (post-M5 / M6 — no escape-hatch wrap remains).
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
        env,
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
        env,
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
      env,
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
      env,
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
      supply.program,
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

function collectPreludeRecordArms(
  bindings: readonly PreludeStmt[],
  loweredArms: ReadonlyArray<readonly [OpaqueExpr, OpaqueExpr]>,
):
  | { arms: Array<readonly [OpaqueExpr, ts.ObjectLiteralExpression]> }
  | { error: string } {
  const earlyReturns = bindings.filter(
    (b): b is Extract<PreludeStmt, { kind: "earlyReturn" }> =>
      b.kind === "earlyReturn",
  );
  if (earlyReturns.length !== loweredArms.length) {
    return {
      error:
        "record return branch collection lost an early-return guard during lowering",
    };
  }
  const arms: Array<readonly [OpaqueExpr, ts.ObjectLiteralExpression]> = [];
  for (const [idx, binding] of earlyReturns.entries()) {
    if (
      binding.valueExpr === undefined ||
      !ts.isObjectLiteralExpression(binding.valueExpr)
    ) {
      return {
        error:
          "record return branches must all return object literals with the same field set",
      };
    }
    arms.push([loweredArms[idx]![0], binding.valueExpr] as const);
  }
  return { arms };
}

function collectIfRecordConditionalArms(
  stmt: ts.IfStatement,
  ctx: L1BuildContext,
):
  | {
      arms: Array<readonly [OpaqueExpr, ts.ObjectLiteralExpression]>;
      otherwise: ts.ObjectLiteralExpression;
    }
  | { error: string } {
  const arms: Array<readonly [OpaqueExpr, ts.ObjectLiteralExpression]> = [];
  let current: ts.IfStatement = stmt;
  while (true) {
    const guard = buildL1SubExpr(current.expression, {
      ...ctx,
      state: ctx.state ?? makeSymbolicState(),
    });
    if (isUnsupported(guard) || isL1Unsupported(guard)) {
      return { error: guard.unsupported };
    }
    const thenExpr = extractReturnFromBranch(
      current.thenStatement,
      ctx.checker,
    );
    if (thenExpr === null || !ts.isObjectLiteralExpression(thenExpr)) {
      return {
        error:
          "record return branches must all return object literals with the same field set",
      };
    }
    arms.push([lowerL1ToOpaque(guard), thenExpr] as const);
    if (!current.elseStatement) {
      return {
        error: "record return if/else chain must have a final else branch",
      };
    }
    if (ts.isIfStatement(current.elseStatement)) {
      current = current.elseStatement;
      continue;
    }
    const elseExpr = extractReturnFromBranch(
      current.elseStatement,
      ctx.checker,
    );
    if (elseExpr === null || !ts.isObjectLiteralExpression(elseExpr)) {
      return {
        error:
          "record return branches must all return object literals with the same field set",
      };
    }
    return { arms, otherwise: elseExpr };
  }
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
  if (typeArgs.length !== 1) {
    return "?";
  }
  const mapped = mapTsType(typeArgs[0]!, checker, strategy);
  return mapped.ok ? mapped.sort : null;
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
  env: AssumptionEnv,
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
    env,
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
    env,
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
  env: AssumptionEnv,
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
    env,
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
    env,
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
    env,
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
  env: AssumptionEnv = createL1AssumptionEnv(),
): BodyResult {
  const ast = getAst();
  const builtin = tryBuildBuiltinCall(expr, {
    checker,
    strategy,
    paramNames,
    state,
    supply,
    env,
  });
  if (builtin !== null) {
    if (isL1Unsupported(builtin)) {
      return { unsupported: builtin.unsupported };
    }
    return { expr: lowerL1ToOpaque(builtin) };
  }

  // Method calls: obj.method(args)
  if (ts.isPropertyAccessExpression(expr.expression)) {
    const methodName = expr.expression.name.text;
    const tsReceiver = expr.expression.expression;

    // .add(x) / .delete(x) / .clear() on a Set<T> receiver -> emit a
    // `{ effect: SetMutation }` for the IR1 SSA builder. Stage A only:
    // the receiver must be a
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
      // `requireMember` suppresses the property-write alias substitution
      // so collection effects keep the canonical Member shape that the
      // SSA builder needs.
      const memberR = buildL1MemberAccess(
        normalizedReceiver,
        {
          checker,
          strategy,
          paramNames,
          state,
          supply,
          env,
        },
        { requireMember: true },
      );
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
      if (!ownerType.ok) {
        return { unsupported: `Set mutation owner type: ${ownerType.reason}` };
      }
      if (!elemType.ok) {
        return { unsupported: `Set mutation element type: ${elemType.reason}` };
      }
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
          env,
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
          ownerType: ownerType.sort,
          elemType: elemType.sort,
          objExpr: objOpaque,
          elemExpr: elemOpaque,
        },
      };
    }

    // .set(k, v) / .delete(k) on a Map<K,V> receiver -> emit a `{ effect: ... }`
    // BodyResult that the IR1 SSA builder lowers as collection writes.
    // Stage A / Stage B disambiguation mirrors the .get/.has branch below.
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
        env,
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
          env,
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
        const memberR = buildL1MemberAccess(
          normalizedReceiver,
          {
            checker,
            strategy,
            paramNames,
            state,
            supply,
            env,
          },
          { requireMember: true },
        );
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
        if (!ownerType.ok) {
          return {
            unsupported: `Map mutation owner type: ${ownerType.reason}`,
          };
        }
        if (!keyType.ok) {
          return { unsupported: `Map mutation key type: ${keyType.reason}` };
        }
        return {
          effect: {
            op: methodName as "set" | "delete",
            ruleName: fieldName,
            keyPredName: `${fieldName}-key`,
            ownerType: ownerType.sort,
            keyType: keyType.sort,
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
      const kTypeResult = mapTsType(
        typeArgs[0]!,
        checker,
        strategy,
        supply.synthCell,
      );
      if (!kTypeResult.ok) {
        return { unsupported: `Map key type: ${kTypeResult.reason}` };
      }
      const vTypeResult = mapTsType(
        typeArgs[1]!,
        checker,
        strategy,
        supply.synthCell,
      );
      if (!vTypeResult.ok) {
        return { unsupported: `Map value type: ${vTypeResult.reason}` };
      }
      const kType = kTypeResult.sort;
      const vType = vTypeResult.sort;
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
        env,
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
        // Stage A receiver-property qualification through the L1 Member
        // helper. `requireMember` suppresses property-write alias
        // substitution so the canonical Member shape is always returned
        // for collection SSA reads.
        const memberR = buildL1MemberAccess(
          normalizedReceiver,
          {
            checker,
            strategy,
            paramNames,
            state,
            supply,
            env,
          },
          { requireMember: true },
        );
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
          env,
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
        if (!ownerType.ok) {
          return { unsupported: `Map read owner type: ${ownerType.reason}` };
        }
        if (!keyType.ok) {
          return { unsupported: `Map read key type: ${keyType.reason}` };
        }
        return {
          expr: readMapThroughWrites(
            state,
            methodName as "get" | "has",
            fieldName,
            `${fieldName}-key`,
            ownerType.sort,
            keyType.sort,
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
      const kTypeResult = mapTsType(
        typeArgs[0]!,
        checker,
        strategy,
        supply.synthCell,
      );
      if (!kTypeResult.ok) {
        return { unsupported: `Map key type: ${kTypeResult.reason}` };
      }
      const vTypeResult = mapTsType(
        typeArgs[1]!,
        checker,
        strategy,
        supply.synthCell,
      );
      if (!vTypeResult.ok) {
        return { unsupported: `Map value type: ${vTypeResult.reason}` };
      }
      const kType = kTypeResult.sort;
      const vType = vTypeResult.sort;
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
        env,
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
        env,
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
        env,
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
        env,
      );
      const objExpr = rejectEffect(objRaw);
      if (isBodyUnsupported(objExpr)) {
        return objExpr;
      }
      // Stage A Set: receiver is a declared interface field. In
      // state-bearing contexts, route through the compatibility read helper
      // so prior `.add`/`.delete`/`.clear` in the same body are observed.
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
          // `requireMember` suppresses property-write alias substitution so
          // collection reads keep the canonical Member shape.
          const memberR = buildL1MemberAccess(
            normalizedReceiver,
            {
              checker,
              strategy,
              paramNames,
              state,
              supply,
              env,
            },
            { requireMember: true },
          );
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
            if (ownerType.ok && elemType?.ok) {
              return {
                expr: readSetThroughWrites(
                  state,
                  fieldName,
                  ownerType.sort,
                  elemType.sort,
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
        env,
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
        env,
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
      env,
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
        env,
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
        env,
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
  env: AssumptionEnv,
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
    const lowered = lowerNestedPureBlockReturn(expr.body, {
      checker,
      strategy,
      paramNames: arrowParams,
      supply,
      env,
    });
    return (
      lowered ??
      lowerNestedCallbackBlockReturn(expr.body, {
        checker,
        strategy,
        paramNames: arrowParams,
        supply,
        env,
      })
    );
  }

  // Expression body
  return translateBodyExpr(
    expr.body,
    checker,
    strategy,
    arrowParams,
    undefined,
    supply,
    env,
  );
}

function lowerNestedCallbackBlockReturn(
  block: ts.Block,
  ctx: NestedPureBlockReturnContext,
): BodyResult | null {
  const lowered = lowerNestedCallbackBlockReturnToL1(block, ctx);
  if (lowered === null) {
    return null;
  }
  return { expr: applyOpaqueAliases(lowerL1ToOpaque(lowered), ctx.supply) };
}

function lowerNestedCallbackBlockReturnToL1(
  block: ts.Block,
  ctx: NestedPureBlockReturnContext,
): IR1Expr | null {
  const stmts = block.statements.filter(
    (s) => !isGuardStatement(s, ctx.checker),
  );
  if (stmts.length === 0) {
    return null;
  }

  let scopedParams: ReadonlyMap<string, string> = ctx.paramNames;
  const substitutions: Array<{ localName: string; value: IR1Expr }> = [];
  const arms: Array<readonly [IR1Expr, IR1Expr]> = [];
  const lastIdx = stmts.length - 1;

  for (let i = 0; i < lastIdx; i++) {
    const stmt = stmts[i]!;
    if (ts.isVariableStatement(stmt)) {
      if (!(stmt.declarationList.flags & ts.NodeFlags.Const)) {
        return null;
      }
      const bindings = constBindingsFromDeclarationList(stmt.declarationList);
      if (bindings === null) {
        return null;
      }
      for (const binding of bindings) {
        if (binding.kind !== "const") {
          return null;
        }
        const blockedNames = callbackConstNamesFrom(stmts, i);
        if (expressionHasSideEffects(binding.initializer, ctx.checker)) {
          return null;
        }
        if (expressionReferencesNames(binding.initializer, blockedNames)) {
          return null;
        }
        const localName = freshHygienicBinder(ctx.supply);
        const value = buildL1SubExpr(binding.initializer, {
          checker: ctx.checker,
          strategy: ctx.strategy,
          paramNames: scopedParams,
          state: ctx.state ?? makeSymbolicState(),
          supply: ctx.supply,
          env: ctx.env,
        });
        if (isUnsupported(value) || isL1Unsupported(value)) {
          return null;
        }
        substitutions.push({ localName, value });
        scopedParams = withParam(scopedParams, binding.tsName, localName);
      }
      continue;
    }

    if (!ts.isIfStatement(stmt) || stmt.elseStatement) {
      return null;
    }
    const blockedNames = callbackConstNamesFrom(stmts, i + 1);
    if (
      expressionReferencesNames(stmt.expression, blockedNames) ||
      nodeReferencesNames(stmt.thenStatement, blockedNames)
    ) {
      return null;
    }
    const guard = buildL1SubExpr(stmt.expression, {
      checker: ctx.checker,
      strategy: ctx.strategy,
      paramNames: scopedParams,
      state: ctx.state ?? makeSymbolicState(),
      supply: ctx.supply,
      env: ctx.env,
    });
    if (isUnsupported(guard) || isL1Unsupported(guard)) {
      return null;
    }
    const value = lowerCallbackReturnArmToL1(stmt.thenStatement, {
      ...ctx,
      paramNames: scopedParams,
    });
    if (value === null) {
      return null;
    }
    arms.push([guard, value] as const);
  }

  const terminal = lowerCallbackTerminalStatementToL1(stmts[lastIdx]!, {
    ...ctx,
    paramNames: scopedParams,
  });
  if (terminal === null) {
    return null;
  }
  const withArms =
    arms.length === 0
      ? terminal
      : ir1Cond(
          arms as [readonly [IR1Expr, IR1Expr], ...typeof arms],
          terminal,
        );
  return substitutions.reduceRight(
    (acc, binding) =>
      substituteIR1ExprSubtree(acc, ir1Var(binding.localName), binding.value),
    withArms,
  );
}

function lowerCallbackReturnArmToL1(
  stmt: ts.Statement,
  ctx: NestedPureBlockReturnContext,
): IR1Expr | null {
  if (ts.isReturnStatement(stmt) && stmt.expression !== undefined) {
    const value = buildL1SubExpr(stmt.expression, {
      checker: ctx.checker,
      strategy: ctx.strategy,
      paramNames: ctx.paramNames,
      state: ctx.state ?? makeSymbolicState(),
      supply: ctx.supply,
      env: ctx.env,
    });
    return isUnsupported(value) || isL1Unsupported(value) ? null : value;
  }
  return ts.isBlock(stmt)
    ? lowerNestedCallbackBlockReturnToL1(stmt, ctx)
    : null;
}

function lowerCallbackTerminalStatementToL1(
  stmt: ts.Statement,
  ctx: NestedPureBlockReturnContext,
): IR1Expr | null {
  if (ts.isReturnStatement(stmt) && stmt.expression !== undefined) {
    return lowerNestedPureTerminalToL1(stmt.expression, ctx, ctx.paramNames);
  }
  if (ts.isIfStatement(stmt) && stmt.elseStatement !== undefined) {
    return lowerNestedPureTerminalToL1(stmt, ctx, ctx.paramNames);
  }
  if (ts.isSwitchStatement(stmt)) {
    return lowerNestedPureTerminalToL1(stmt, ctx, ctx.paramNames);
  }
  return null;
}

function callbackConstNamesFrom(
  stmts: readonly ts.Statement[],
  startIdx: number,
): Set<string> {
  const names = new Set<string>();
  for (const stmt of stmts.slice(startIdx)) {
    if (!ts.isVariableStatement(stmt)) {
      continue;
    }
    for (const name of bindingNamesFromDeclarationList(stmt.declarationList)) {
      names.add(name);
    }
  }
  return names;
}

// --- Mutating function body translation ---

function translateMutatingBody(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  functionName: string,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  declarations: PantDeclaration[],
  synthCell?: SynthCell,
  program?: ts.Program,
  suppliedEnv?: AssumptionEnv,
  recognitionHook?: (env: AssumptionEnv, location: string) => void,
): PropResult[] {
  if (!node.body) {
    return [];
  }
  const localMapBuilderRejection = describeLocalMapBuilderBodyRejection(
    node.body,
    checker,
  );
  if (localMapBuilderRejection !== null) {
    return [
      {
        kind: "unsupported",
        reason: `${functionName} — ${localMapBuilderRejection}`,
      },
    ];
  }
  const localRuleDecls: PropResult[] = [];
  const env = suppliedEnv ?? createL1AssumptionEnv();

  const lowered = appendFramesForUnmodifiedRules(
    lowerSupportedSsaMutatingStatements(Array.from(node.body.statements), {
      checker,
      strategy,
      paramNames,
      state: makeSymbolicState(),
      supply: makeUniqueSupply(synthCell, program),
      initialPropertyValues: new Map(),
      helperNameBase: functionName,
      declarations,
      localRuleDecls,
      env,
      ...(recognitionHook === undefined ? {} : { recognitionHook }),
    }),
    declarations,
  );
  if (lowered.diagnostics.length > 0) {
    return lowered.diagnostics;
  }
  return [...localRuleDecls, ...lowered.propositions];
}

function lowerSupportedSsaMutatingStatements(
  stmts: readonly ts.Statement[],
  ctx: {
    checker: ts.TypeChecker;
    strategy: NumericStrategy;
    paramNames: Map<string, string>;
    state: SymbolicState;
    supply: UniqueSupply;
    initialPropertyValues: ReadonlyMap<string, OpaqueExpr>;
    helperNameBase: string;
    declarations: readonly PantDeclaration[];
    localRuleDecls: PropResult[];
    env: AssumptionEnv;
    recognitionHook?: (env: AssumptionEnv, location: string) => void;
  },
): IR1SsaBodyLowerResult {
  const tailReturnValue = extractTailReturnValue(stmts, ctx.checker);
  if (tailReturnValue !== null) {
    const prefix = lowerSupportedSsaMutatingBlock(
      tailReturnValue.prefixStmts,
      ctx,
    );
    if (prefix.diagnostics.length > 0) {
      return prefix;
    }
    const returnExpr = translateBodyExpr(
      tailReturnValue.expression,
      ctx.checker,
      ctx.strategy,
      ctx.paramNames,
      ctx.state,
      ctx.supply,
      ctx.env,
    );
    if (isBodyUnsupported(returnExpr)) {
      return ir1SsaBodyLowerUnsupported(returnExpr.unsupported);
    }
    return {
      ...prefix,
      returnValue: {
        ruleName: ctx.helperNameBase,
        expression: applyOpaqueAliases(bodyExpr(returnExpr), ctx.supply),
      },
    };
  }

  const firstEarlyExit = stmts.findIndex(
    (stmt) => detectEarlyExit(stmt) !== null,
  );
  if (firstEarlyExit === -1) {
    return lowerSupportedSsaMutatingBlock(stmts, ctx);
  }

  const exitStmt = stmts[firstEarlyExit]!;
  const exit = detectEarlyExit(exitStmt);
  if (exit === null) {
    return ir1SsaBodyLowerUnsupported("internal early-exit detection mismatch");
  }
  if (expressionHasSideEffects(exit.condition, ctx.checker)) {
    return ir1SsaBodyLowerUnsupported("impure if-condition in mutating body");
  }

  const prefix = lowerSupportedSsaMutatingBlock(
    stmts.slice(0, firstEarlyExit),
    {
      ...ctx,
    },
  );
  if (prefix.diagnostics.length > 0) {
    return prefix;
  }
  if (hasDirectNonFinalEmission(prefix)) {
    return ir1SsaBodyLowerUnsupported(
      "early-exit prefixes must lower to final SSA properties only",
    );
  }

  const propertyValues = new Map(ctx.initialPropertyValues);
  for (const entry of prefix.finalProperties ?? []) {
    const property = finalPropertyParts(entry);
    if (property === null) {
      return ir1SsaBodyLowerUnsupported(
        "early-exit prefixes only support scalar property writes",
      );
    }
    propertyValues.set(
      symbolicKey(property.prop, property.objExpr),
      property.rhs,
    );
    ctx.state.writes = putWrite(
      ctx.state.writes,
      symbolicKey(property.prop, property.objExpr),
      {
        kind: "property",
        prop: property.prop,
        objExpr: property.objExpr,
        value: property.rhs,
      },
    );
    ctx.state.writtenKeys = addWrittenKey(
      ctx.state.writtenKeys,
      symbolicKey(property.prop, property.objExpr),
    );
  }

  const gResult = translateBodyExpr(
    exit.condition,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
    ctx.env,
  );
  if (isBodyUnsupported(gResult)) {
    return ir1SsaBodyLowerUnsupported(gResult.unsupported);
  }
  const guardExpr = applyOpaqueAliases(bodyExpr(gResult), ctx.supply);

  const continuation = [
    ...exit.continuationPrefix,
    ...stmts.slice(firstEarlyExit + 1),
  ];
  const continuationResult = lowerSupportedSsaMutatingStatements(continuation, {
    ...ctx,
    initialPropertyValues: propertyValues,
    helperNameBase: ctx.helperNameBase,
  });
  if (continuationResult.diagnostics.length > 0) {
    return continuationResult;
  }
  if (hasDirectNonFinalEmission(continuationResult)) {
    return ir1SsaBodyLowerUnsupported(
      "loop with per-iteration writes cannot appear after an early-exit guard",
    );
  }

  const continuationKeys = new Set<string>();
  const inputs: ScalarSsaEarlyExitPropertyInput[] = [];
  for (const entry of continuationResult.finalProperties ?? []) {
    const property = finalPropertyParts(entry);
    if (property === null) {
      return ir1SsaBodyLowerUnsupported(
        "early-exit continuations only support scalar property writes",
      );
    }
    const key = symbolicKey(property.prop, property.objExpr);
    continuationKeys.add(key);
    inputs.push({
      key,
      prop: property.prop,
      objExpr: property.objExpr,
      priorValue: propertyValues.get(key),
      continuationValue: property.rhs,
    });
  }

  const merged = lowerScalarSsaEarlyExitMerge(inputs, {
    guardExpr,
    earlyExitWhenTrue: exit.earlyExitWhenTrue,
    canonicalize: (e) => applyOpaqueAliases(e, ctx.supply),
  });
  const prefixOnly = (prefix.finalProperties ?? []).filter((entry) => {
    const property = finalPropertyParts(entry);
    return property === null
      ? false
      : !continuationKeys.has(symbolicKey(property.prop, property.objExpr));
  });

  return ir1SsaBodyLowerSuccess({
    propositions: [
      ...prefixOnly.map((entry) => finalPropertyEquation(entry)),
      ...merged.propositions,
    ],
    modifiedRules: [
      ...prefixOnly.map((entry) => finalPropertyRuleName(entry)),
      ...merged.modifiedRules,
    ],
    programs: [
      ...prefix.programs,
      ...continuationResult.programs,
      merged.program,
    ],
    finalProperties: [...prefixOnly, ...merged.finalProperties],
  });
}

function extractTailReturnValue(
  stmts: readonly ts.Statement[],
  checker: ts.TypeChecker,
): { expression: ts.Expression; prefixStmts: readonly ts.Statement[] } | null {
  const relevant = stmts.filter((stmt) => !isGuardStatement(stmt, checker));
  const last = relevant[relevant.length - 1];
  if (
    last === undefined ||
    !ts.isReturnStatement(last) ||
    last.expression === undefined
  ) {
    return null;
  }
  const prefixStmts = relevant.slice(0, -1);
  if (prefixStmts.some((stmt) => containsEarlyExitStatement(stmt))) {
    return null;
  }
  return { expression: last.expression, prefixStmts };
}

function containsEarlyExitStatement(node: ts.Node): boolean {
  let found = false;
  function visit(current: ts.Node): void {
    if (found) {
      return;
    }
    if (ts.isFunctionLike(current) && current !== node) {
      return;
    }
    if (ts.isClassDeclaration(current) || ts.isClassExpression(current)) {
      return;
    }
    if (
      ts.isReturnStatement(current) ||
      ts.isBreakStatement(current) ||
      ts.isContinueStatement(current) ||
      ts.isThrowStatement(current)
    ) {
      found = true;
      return;
    }
    ts.forEachChild(current, visit);
  }
  visit(node);
  return found;
}

function isInsideLoopBody(node: ts.Node): boolean {
  let current: ts.Node | undefined = node.parent;
  while (current !== undefined) {
    if (
      ts.isForStatement(current) ||
      ts.isWhileStatement(current) ||
      ts.isDoStatement(current)
    ) {
      return true;
    }
    if (ts.isFunctionLike(current)) {
      return false;
    }
    current = current.parent;
  }
  return false;
}

function lowerSupportedSsaMutatingBlock(
  stmts: readonly ts.Statement[],
  ctx: {
    checker: ts.TypeChecker;
    strategy: NumericStrategy;
    paramNames: Map<string, string>;
    state: SymbolicState;
    supply: UniqueSupply;
    initialPropertyValues: ReadonlyMap<string, OpaqueExpr>;
    helperNameBase: string;
    declarations: readonly PantDeclaration[];
    localRuleDecls: PropResult[];
    env: AssumptionEnv;
    recognitionHook?: (env: AssumptionEnv, location: string) => void;
  },
): IR1SsaBodyLowerResult {
  const body = ts.factory.createBlock(stmts, true);
  const ssaBody = buildSupportedSsaMutatingBody(
    body,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
    ctx.env,
    ctx.localRuleDecls,
    ctx.recognitionHook,
  );

  if (isUnsupported(ssaBody)) {
    return ssaBody.unsupported === "empty mutating body"
      ? ir1SsaBodyLowerSuccess()
      : ir1SsaBodyLowerUnsupported(ssaBody.unsupported);
  }

  return lowerL1BodyToSsaProps(ssaBody, [], {
    applyConst: (e) => applyOpaqueAliases(e, ctx.supply),
    initialPropertyValues: ctx.initialPropertyValues,
    strategy: ctx.strategy,
    declarations: ctx.declarations,
    returnRuleName: ctx.helperNameBase,
    ...(ctx.supply.synthCell === undefined
      ? {}
      : { synthCell: ctx.supply.synthCell }),
  });
}

function hasDirectNonFinalEmission(result: IR1SsaBodyLowerResult): boolean {
  return (
    result.returnValue !== null ||
    result.propositions.length !== (result.finalProperties?.length ?? 0)
  );
}

function finalPropertyParts(
  entry: IR1SsaFinalProperty,
): { prop: string; objExpr: OpaqueExpr; rhs: OpaqueExpr } | null {
  if ("kind" in entry && entry.kind === "property") {
    return { prop: entry.prop, objExpr: entry.objExpr, rhs: entry.rhs };
  }
  if ("location" in entry && entry.location.kind === "property") {
    return {
      prop: entry.location.ruleName,
      objExpr: entry.objExpr,
      rhs: entry.rhs,
    };
  }
  return null;
}

function finalPropertyRuleName(entry: IR1SsaFinalProperty): string {
  const property = finalPropertyParts(entry);
  if (property === null) {
    throw new Error("expected scalar property final entry");
  }
  return property.prop;
}

function finalPropertyEquation(entry: IR1SsaFinalProperty): PropResult {
  const property = finalPropertyParts(entry);
  if (property === null) {
    throw new Error("expected scalar property final entry");
  }
  return {
    kind: "equation",
    quantifiers: [] as OpaqueParam[],
    lhs: getAst().app(getAst().primed(property.prop), [property.objExpr]),
    rhs: property.rhs,
  };
}

function buildSupportedSsaMutatingBody(
  body: ts.Block,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  state: SymbolicState,
  supply: UniqueSupply,
  env: AssumptionEnv,
  localRuleDecls?: PropResult[],
  recognitionHook?: (env: AssumptionEnv, location: string) => void,
): IR1Stmt | { unsupported: string } {
  const stmts: IR1Stmt[] = [];
  const scopedParamNames = new Map(paramNames);
  const letNames = new Set<string>();
  const canonicalizeAliases = (e: OpaqueExpr): OpaqueExpr =>
    applyOpaqueAliases(e, supply);
  setCanonicalize(state, canonicalizeAliases);
  for (let i = 0; i < body.statements.length; i++) {
    const stmt = body.statements[i]!;
    if (isGuardStatement(stmt, checker) && !isInsideLoopBody(stmt)) {
      continue;
    }
    if (
      ts.isReturnStatement(stmt) &&
      !stmt.expression &&
      !isInsideLoopBody(stmt)
    ) {
      continue;
    }
    if (
      ts.isVariableStatement(stmt) &&
      stmt.declarationList.flags & ts.NodeFlags.Let &&
      stmt.declarationList.declarations.length === 1 &&
      ts.isIdentifier(stmt.declarationList.declarations[0]!.name) &&
      stmt.declarationList.declarations[0]!.initializer !== undefined &&
      i + 1 < body.statements.length &&
      ts.isWhileStatement(body.statements[i + 1]!)
    ) {
      const built = buildL1LetWhileMutation(
        stmt,
        body.statements[i + 1]! as ts.WhileStatement,
        {
          checker,
          strategy,
          paramNames: scopedParamNames,
          state,
          supply,
          env,
          ...(recognitionHook === undefined ? {} : { recognitionHook }),
        },
      );
      if (built !== null) {
        stmts.push(built);
        i += 1;
        continue;
      }
      const fixedPointBuilt = buildL1LetWhileFixedPointMutation(
        stmt,
        body.statements[i + 1]! as ts.WhileStatement,
        {
          checker,
          strategy,
          paramNames: scopedParamNames,
          state,
          supply,
          env,
          ...(recognitionHook === undefined ? {} : { recognitionHook }),
        },
      );
      if (isUnsupported(fixedPointBuilt)) {
        return fixedPointBuilt;
      }
      if (fixedPointBuilt !== null) {
        stmts.push(fixedPointBuilt);
        i += 1;
        continue;
      }
    }
    if (ts.isVariableStatement(stmt)) {
      const declList = stmt.declarationList;
      if (
        declList.flags & ts.NodeFlags.Const ||
        declList.flags & ts.NodeFlags.Let
      ) {
        const bindings = constLikeBindingsFromVariableStatement(stmt, body);
        if (bindings !== null) {
          const lowered = lowerPreludeBindings(
            bindings,
            checker,
            strategy,
            scopedParamNames,
            supply,
            env,
            state,
          );
          if ("error" in lowered) {
            return { unsupported: lowered.error };
          }
          if (localRuleDecls !== undefined) {
            localRuleDecls.push(...lowered.ruleDecls);
          }
          stmts.push(...lowered.letStmts);
          for (const [key, value] of lowered.scopedParams) {
            scopedParamNames.set(key, value);
          }
          if (declList.flags & ts.NodeFlags.Let) {
            for (const name of bindingNamesFromDeclarationList(declList)) {
              letNames.add(name);
            }
          }
          setCanonicalize(state, canonicalizeAliases);
          continue;
        }
        if (declList.flags & ts.NodeFlags.Let) {
          return { unsupported: LET_CLOSURE_REASSIGNMENT_UNSUPPORTED };
        }
      }
      if (
        !(declList.flags & ts.NodeFlags.Const) &&
        !(declList.flags & ts.NodeFlags.Let)
      ) {
        return { unsupported: VAR_BINDINGS_UNSUPPORTED };
      }
      return {
        unsupported: "local variable declaration (effectful const)",
      };
    }
    if (ts.isExpressionStatement(stmt)) {
      const reassignments = reassignmentsFromExpressionStatement(
        stmt,
        letNames,
      );
      if (reassignments !== null) {
        const lowered = lowerPreludeBindings(
          reassignments,
          checker,
          strategy,
          scopedParamNames,
          supply,
          env,
          state,
        );
        if ("error" in lowered) {
          return { unsupported: lowered.error };
        }
        stmts.push(...lowered.letStmts);
        continue;
      }
    }
    const built = buildSupportedSsaStatement(stmt, {
      checker,
      strategy,
      paramNames: scopedParamNames,
      state,
      supply,
      env,
      ...(localRuleDecls === undefined ? {} : { localRuleDecls }),
      ...(recognitionHook === undefined ? {} : { recognitionHook }),
    });
    if (isUnsupported(built)) {
      return built;
    }
    stmts.push(built);
  }
  if (stmts.length === 0) {
    return { unsupported: "empty mutating body" };
  }
  if (stmts.length === 1) {
    return stmts[0]!;
  }
  return ir1Block(stmts as [IR1Stmt, ...IR1Stmt[]]);
}

function buildSupportedSsaStatement(
  stmt: ts.Statement,
  ctx: {
    checker: ts.TypeChecker;
    strategy: NumericStrategy;
    paramNames: Map<string, string>;
    state: SymbolicState;
    supply: UniqueSupply;
    env: AssumptionEnv;
    localRuleDecls?: PropResult[];
    recognitionHook?: (env: AssumptionEnv, location: string) => void;
  },
): IR1Stmt | { unsupported: string } {
  if (
    (ts.isBreakStatement(stmt) || ts.isContinueStatement(stmt)) &&
    stmt.label !== undefined
  ) {
    return {
      unsupported:
        "labeled loop early-exit is M7 future work; remove the label or restructure",
    };
  }
  if (ts.isBreakStatement(stmt)) {
    return ir1Break();
  }
  if (ts.isContinueStatement(stmt)) {
    return ir1Continue();
  }
  if (ts.isLabeledStatement(stmt)) {
    return buildSupportedSsaStatement(stmt.statement, ctx);
  }
  if (ts.isBlock(stmt)) {
    const body = buildSupportedSsaMutatingBody(
      stmt,
      ctx.checker,
      ctx.strategy,
      ctx.paramNames,
      ctx.state,
      ctx.supply,
      ctx.env,
      ctx.localRuleDecls,
      ctx.recognitionHook,
    );
    return body;
  }
  if (ts.isIfStatement(stmt)) {
    return buildL1IfMutation(stmt, ctx);
  }
  if (ts.isReturnStatement(stmt)) {
    if (stmt.expression === undefined) {
      return ir1Return(null);
    }
    const expr = buildL1SubExpr(stmt.expression, ctx);
    if (isUnsupported(expr)) {
      return expr;
    }
    return ir1Return(expr);
  }
  if (ts.isThrowStatement(stmt)) {
    const expr = buildL1SubExpr(stmt.expression, ctx);
    if (isUnsupported(expr)) {
      return expr;
    }
    return ir1Throw(expr);
  }
  if (ts.isForOfStatement(stmt)) {
    return buildL1ForOfMutation(stmt, ctx);
  }
  if (ts.isForStatement(stmt)) {
    return buildL1ForCounterMutation(stmt, ctx);
  }
  if (ts.isExpressionStatement(stmt)) {
    const expr = unwrapExpression(stmt.expression);
    if (
      ts.isCallExpression(expr) &&
      ts.isPropertyAccessExpression(expr.expression) &&
      expr.expression.name.text === "forEach"
    ) {
      return buildL1ForEachCall(expr, ctx);
    }
    if (ts.isCallExpression(expr)) {
      return buildL1EffectCall(expr, ctx);
    }
    if (
      ts.isBinaryExpression(expr) ||
      ts.isPrefixUnaryExpression(expr) ||
      ts.isPostfixUnaryExpression(expr)
    ) {
      return buildL1AssignStmt(stmt, ctx);
    }
  }
  if (ts.isWhileStatement(stmt)) {
    return buildL1BareWhileMutation(stmt, ctx);
  }
  if (ts.isForInStatement(stmt) || ts.isDoStatement(stmt)) {
    return { unsupported: "loop assignment" };
  }
  return {
    unsupported: `statement is not supported by unified SSA body lowering: ${ts.SyntaxKind[stmt.kind]}`,
  };
}

function buildL1BareWhileMutation(
  stmt: ts.WhileStatement,
  ctx: BuildBodyCtx & {
    paramNames: Map<string, string>;
    localRuleDecls?: PropResult[];
  },
): IR1Stmt | { unsupported: string } {
  const expr = unwrapExpression(stmt.expression);
  if (
    expr.kind === ts.SyntaxKind.TrueKeyword &&
    !containsReachableLoopExit(stmt.statement)
  ) {
    return {
      unsupported:
        "while loop has no observable termination condition (literal-true guard); rewrite with a guard that depends on mutated state",
    };
  }
  const cond = buildL1SubExpr(stmt.expression, ctx);
  if (isUnsupported(cond)) {
    return cond;
  }
  const bodyStmt = ts.isBlock(stmt.statement)
    ? buildSupportedSsaMutatingBody(
        stmt.statement,
        ctx.checker,
        ctx.strategy,
        ctx.paramNames,
        ctx.state,
        ctx.supply,
        ctx.env,
        ctx.localRuleDecls,
        ctx.recognitionHook,
      )
    : buildSupportedSsaStatement(stmt.statement, ctx);
  if (isUnsupported(bodyStmt)) {
    return bodyStmt;
  }
  return ir1While(cond, bodyStmt);
}

function buildL1ForCounterMutation(
  stmt: ts.ForStatement,
  ctx: {
    checker: ts.TypeChecker;
    strategy: NumericStrategy;
    paramNames: Map<string, string>;
    state: SymbolicState;
    supply: UniqueSupply;
    env: AssumptionEnv;
    localRuleDecls?: PropResult[];
    recognitionHook?: (env: AssumptionEnv, location: string) => void;
  },
): IR1Stmt | { unsupported: string } {
  const init = buildL1ForCounterInit(stmt.initializer);
  if (isUnsupported(init)) {
    return init;
  }

  if (stmt.condition === undefined) {
    return { unsupported: "counter loop condition is missing" };
  }
  const cond = buildL1ForCounterCondition(
    stmt.condition,
    init.counterName,
    ctx,
  );
  if (isUnsupported(cond)) {
    return cond;
  }

  if (stmt.incrementor === undefined) {
    return { unsupported: "counter loop step is missing" };
  }
  if (counterStepHasEffectfulOperand(stmt.incrementor, init.counterName, ctx)) {
    return {
      unsupported: "counter loop step expression has side effects",
    };
  }
  const step = buildL1IncrementStep(stmt.incrementor, init.counterName, {
    checker: ctx.checker,
    strategy: ctx.strategy,
    paramNames: ctx.paramNames,
    state: ctx.state,
    supply: ctx.supply,
    env: ctx.env,
  });
  if (isL1StmtUnsupported(step)) {
    return { unsupported: step.unsupported };
  }

  const bodyStmt = ts.isBlock(stmt.statement)
    ? buildSupportedSsaMutatingBody(
        stmt.statement,
        ctx.checker,
        ctx.strategy,
        ctx.paramNames,
        ctx.state,
        ctx.supply,
        ctx.env,
        ctx.localRuleDecls,
        ctx.recognitionHook,
      )
    : buildSupportedSsaStatement(stmt.statement, ctx);
  if (isUnsupported(bodyStmt)) {
    return bodyStmt;
  }

  return ir1For(init.initStmt, cond, step, bodyStmt);
}

function buildL1LetWhileFixedPointMutation(
  letStmt: ts.VariableStatement,
  whileStmt: ts.WhileStatement,
  ctx: BuildBodyCtx & {
    paramNames: Map<string, string>;
    localRuleDecls?: PropResult[];
  },
): IR1Stmt | { unsupported: string } | null {
  const declList = letStmt.declarationList;
  if (
    !(declList.flags & ts.NodeFlags.Let) ||
    declList.declarations.length !== 1
  ) {
    return null;
  }

  const decl = declList.declarations[0]!;
  if (!ts.isIdentifier(decl.name) || decl.initializer === undefined) {
    return null;
  }

  if (!ts.isBlock(whileStmt.statement)) {
    return null;
  }

  const initExpr = buildL1SubExpr(decl.initializer, ctx);
  if (isUnsupported(initExpr)) {
    return initExpr;
  }
  const expr = unwrapExpression(whileStmt.expression);
  if (
    expr.kind === ts.SyntaxKind.TrueKeyword &&
    !containsReachableLoopExit(whileStmt.statement)
  ) {
    return {
      unsupported:
        "while loop has no observable termination condition (literal-true guard); rewrite with a guard that depends on mutated state",
    };
  }
  const cond = buildL1SubExpr(whileStmt.expression, ctx);
  if (isUnsupported(cond)) {
    return cond;
  }
  const bodyStmt = buildSupportedSsaMutatingBody(
    whileStmt.statement,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
    ctx.env,
    ctx.localRuleDecls,
    ctx.recognitionHook,
  );
  if (isUnsupported(bodyStmt)) {
    return bodyStmt;
  }
  return ir1Block([ir1Let(decl.name.text, initExpr), ir1While(cond, bodyStmt)]);
}

function containsReachableLoopExit(stmt: ts.Statement): boolean {
  let found = false;
  function visit(node: ts.Node): void {
    if (found) {
      return;
    }
    if (node !== stmt && (ts.isFunctionLike(node) || ts.isClassLike(node))) {
      return;
    }
    if (
      node !== stmt &&
      (ts.isForStatement(node) || ts.isWhileStatement(node))
    ) {
      return;
    }
    if (ts.isBreakStatement(node) || ts.isReturnStatement(node)) {
      found = true;
      return;
    }
    ts.forEachChild(node, visit);
  }
  visit(stmt);
  return found;
}

function buildL1LetWhileMutation(
  letStmt: ts.VariableStatement,
  whileStmt: ts.WhileStatement,
  ctx: BuildBodyCtx & {
    paramNames: Map<string, string>;
    localRuleDecls?: PropResult[];
  },
): IR1Stmt | null {
  const declList = letStmt.declarationList;
  if (
    !(declList.flags & ts.NodeFlags.Let) ||
    declList.declarations.length !== 1
  ) {
    return null;
  }

  const decl = declList.declarations[0]!;
  if (!ts.isIdentifier(decl.name) || decl.initializer === undefined) {
    return null;
  }

  const counterName = decl.name.text;
  const initExpr = buildL1SubExpr(decl.initializer, ctx);
  if (isUnsupported(initExpr)) {
    return null;
  }

  const cond = buildL1LetWhileCounterCondition(
    whileStmt.expression,
    counterName,
    ctx,
  );
  if (cond === null) {
    return null;
  }

  if (!ts.isBlock(whileStmt.statement)) {
    return null;
  }
  const bodyStatements = [...whileStmt.statement.statements];
  const trailingStep = bodyStatements.at(-1);
  if (
    trailingStep === undefined ||
    !ts.isExpressionStatement(trailingStep) ||
    counterStepHasEffectfulOperand(trailingStep.expression, counterName, ctx)
  ) {
    return null;
  }

  const step = buildL1IncrementStep(trailingStep.expression, counterName, ctx);
  if (isL1StmtUnsupported(step)) {
    return null;
  }
  if (!isCanonicalUnitCounterStep(step, counterName)) {
    return null;
  }

  const bodyWithoutStep = ts.factory.updateBlock(
    whileStmt.statement,
    bodyStatements.slice(0, -1),
  );
  const bodyStmt = buildSupportedSsaMutatingBody(
    bodyWithoutStep,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
    ctx.env,
    ctx.localRuleDecls,
    ctx.recognitionHook,
  );
  if (isUnsupported(bodyStmt)) {
    return null;
  }

  return ir1For(ir1Let(counterName, initExpr), cond, step, bodyStmt);
}

function isCanonicalUnitCounterStep(
  step: IR1Stmt,
  counterName: string,
): boolean {
  return (
    step.kind === "assign" &&
    step.target.kind === "var" &&
    step.target.name === counterName &&
    step.value.kind === "binop" &&
    (step.value.op === "add" || step.value.op === "sub") &&
    step.value.lhs.kind === "var" &&
    step.value.lhs.name === counterName &&
    step.value.rhs.kind === "lit" &&
    step.value.rhs.value.kind === "nat" &&
    step.value.rhs.value.value === 1
  );
}

function buildL1LetWhileCounterCondition(
  condition: ts.Expression,
  counterName: string,
  ctx: BuildBodyCtx,
): IR1Expr | null {
  const cond = unwrapExpression(condition);
  if (
    !ts.isBinaryExpression(cond) ||
    (cond.operatorToken.kind !== ts.SyntaxKind.LessThanToken &&
      cond.operatorToken.kind !== ts.SyntaxKind.LessThanEqualsToken &&
      cond.operatorToken.kind !== ts.SyntaxKind.GreaterThanToken &&
      cond.operatorToken.kind !== ts.SyntaxKind.GreaterThanEqualsToken) ||
    !ts.isIdentifier(cond.left) ||
    cond.left.text !== counterName
  ) {
    return null;
  }
  if (
    expressionReferencesNames(cond.right, new Set([counterName])) ||
    expressionHasSideEffects(cond.right, ctx.checker)
  ) {
    return null;
  }
  const bound = buildL1SubExpr(cond.right, ctx);
  if (isUnsupported(bound)) {
    return null;
  }

  const op =
    cond.operatorToken.kind === ts.SyntaxKind.LessThanToken
      ? "lt"
      : cond.operatorToken.kind === ts.SyntaxKind.LessThanEqualsToken
        ? "le"
        : cond.operatorToken.kind === ts.SyntaxKind.GreaterThanToken
          ? "gt"
          : "ge";
  return ir1Binop(op, ir1Var(counterName), bound);
}

function buildL1ForCounterInit(
  initializer: ts.ForInitializer | undefined,
): { counterName: string; initStmt: IR1Stmt } | { unsupported: string } {
  if (
    initializer === undefined ||
    !ts.isVariableDeclarationList(initializer) ||
    !(initializer.flags & ts.NodeFlags.Let) ||
    initializer.declarations.length !== 1
  ) {
    return {
      unsupported: "counter loop initializer must be a single let binding",
    };
  }
  const decl = initializer.declarations[0]!;
  if (!ts.isIdentifier(decl.name)) {
    return { unsupported: "counter loop initializer must bind an identifier" };
  }
  if (decl.initializer === undefined) {
    return { unsupported: "counter loop initializer is missing" };
  }
  const initExpr = unwrapExpression(decl.initializer);
  if (!ts.isNumericLiteral(initExpr)) {
    return {
      unsupported: "counter loop initializer must be a numeric literal",
    };
  }
  const value = Number(initExpr.text);
  if (!Number.isFinite(value) || !Number.isInteger(value) || value < 0) {
    return {
      unsupported:
        "counter loop initializer must be a non-negative integer literal",
    };
  }
  return {
    counterName: decl.name.text,
    initStmt: ir1Let(decl.name.text, ir1LitNat(value)),
  };
}

function buildL1ForCounterCondition(
  condition: ts.Expression,
  counterName: string,
  ctx: {
    checker: ts.TypeChecker;
    strategy: NumericStrategy;
    paramNames: Map<string, string>;
    state: SymbolicState;
    supply: UniqueSupply;
    env: AssumptionEnv;
  },
): IR1Expr | { unsupported: string } {
  const cond = unwrapExpression(condition);
  if (
    !ts.isBinaryExpression(cond) ||
    (cond.operatorToken.kind !== ts.SyntaxKind.LessThanToken &&
      cond.operatorToken.kind !== ts.SyntaxKind.LessThanEqualsToken) ||
    !ts.isIdentifier(cond.left) ||
    cond.left.text !== counterName
  ) {
    return {
      unsupported:
        "counter loop condition must be `counter < bound` or `counter <= bound`",
    };
  }
  if (expressionReferencesNames(cond.right, new Set([counterName]))) {
    return { unsupported: "counter loop bound must not reference the counter" };
  }
  if (expressionHasSideEffects(cond.right, ctx.checker)) {
    return { unsupported: "counter loop bound expression has side effects" };
  }
  const bound = buildL1SubExpr(cond.right, ctx);
  if (isUnsupported(bound)) {
    return bound;
  }
  return ir1Binop(
    cond.operatorToken.kind === ts.SyntaxKind.LessThanToken ? "lt" : "le",
    ir1Var(counterName),
    bound,
  );
}

function counterStepHasEffectfulOperand(
  step: ts.Expression,
  counterName: string,
  ctx: { checker: ts.TypeChecker },
): boolean {
  const expr = unwrapExpression(step);
  if (ts.isPostfixUnaryExpression(expr) || ts.isPrefixUnaryExpression(expr)) {
    return false;
  }
  if (!ts.isBinaryExpression(expr)) {
    return expressionHasSideEffects(expr, ctx.checker);
  }
  if (
    expr.operatorToken.kind === ts.SyntaxKind.PlusEqualsToken ||
    expr.operatorToken.kind === ts.SyntaxKind.MinusEqualsToken ||
    expr.operatorToken.kind === ts.SyntaxKind.AsteriskEqualsToken ||
    expr.operatorToken.kind === ts.SyntaxKind.SlashEqualsToken
  ) {
    return expressionHasSideEffects(expr.right, ctx.checker);
  }
  if (expr.operatorToken.kind !== ts.SyntaxKind.EqualsToken) {
    return expressionHasSideEffects(expr, ctx.checker);
  }
  if (!ts.isBinaryExpression(expr.right)) {
    return expressionHasSideEffects(expr.right, ctx.checker);
  }
  const { left, right } = expr.right;
  if (ts.isIdentifier(left) && left.text === counterName) {
    return expressionHasSideEffects(right, ctx.checker);
  }
  if (ts.isIdentifier(right) && right.text === counterName) {
    return expressionHasSideEffects(left, ctx.checker);
  }
  return expressionHasSideEffects(expr.right, ctx.checker);
}
