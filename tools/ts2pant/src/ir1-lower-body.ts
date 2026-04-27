/**
 * L1 statement → mutating-body Pantagruel propositions.
 *
 * Single-fold lowering: walks an `IR1Stmt`, threads `SymbolicState`
 * from `translate-body.ts`, and pushes `PropResult[]`. Reuses the
 * existing mutation primitives (`putWrite`, `mergeOverrides`,
 * `installMapWrite`, `installSetWrite`) so frame-condition synthesis
 * is identical to the pre-L1 path.
 *
 * Dispatches by `stmt.kind`:
 * - `block` — sequential composition.
 * - `assign(member, …)` — property write into the symbolic state.
 * - `cond-stmt` — single-arm if-with-mutation; forks two state clones,
 *   recurses, then merges per write-key (property writes use
 *   cond-fallback; Map/Set merge their override lists).
 * - `map-effect` / `set-effect` — Map/Set mutation effects.
 * - `foreach` — Shape A body lowered through a sub-state and emitted
 *   as `all binder in src | prop' obj = value` equations; Shape B
 *   `foldLeaves` emit `prop' target = prior outerOp (combOP over each
 *   binder in src[, guard] | rhs)` equations into the outer state.
 *
 * Other L1 forms (`let`, `return`, `throw`, `expr-stmt`, `while`,
 * `for`) reject — they're either out of scope for mutating-body
 * lowering or handled at higher layers (μ-search via `lowerL1MuSearch`).
 */

import { lowerBinop, lowerExpr } from "./ir-emit.js";
import type { IR1Expr, IR1FoldLeaf, IR1Stmt } from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import {
  addModifiedProp,
  addWrittenKey,
  cloneSymbolicState,
  installMapWrite,
  installSetWrite,
  type MapOverride,
  type MapRuleWriteEntry,
  makeSymbolicState,
  mergeOverrides,
  type PropertyWriteEntry,
  putWrite,
  type SetOverride,
  type SetRuleWriteEntry,
  type SymbolicState,
  symbolicKey,
} from "./translate-body.js";
import type { PropResult } from "./types.js";

export interface LowerBodyCtx {
  applyConst: (e: OpaqueExpr) => OpaqueExpr;
}

/** Lower an L1 expression all the way to OpaqueExpr (L1 → L2 → Opaque). */
function lowerL1ExprToOpaque(e: IR1Expr): OpaqueExpr {
  return lowerExpr(lowerL1Expr(e));
}

/**
 * Lower an L1 statement into the symbolic state and propositions.
 * Returns `false` if any sub-statement was unsupported (in which case
 * `propositions` already contains an `unsupported` marker).
 */
export function lowerL1Body(
  stmt: IR1Stmt,
  state: SymbolicState,
  propositions: PropResult[],
  ctx: LowerBodyCtx,
): boolean {
  switch (stmt.kind) {
    case "block": {
      let ok = true;
      for (const child of stmt.stmts) {
        if (!lowerL1Body(child, state, propositions, ctx)) {
          ok = false;
        }
      }
      return ok;
    }
    case "assign":
      return lowerAssign(stmt, state, propositions, ctx);
    case "cond-stmt":
      return lowerCondStmt(stmt, state, propositions, ctx);
    case "map-effect":
      return lowerMapEffect(stmt, state, ctx);
    case "set-effect":
      return lowerSetEffect(stmt, state, ctx);
    case "foreach":
      return lowerForeach(stmt, state, propositions, ctx);
    case "let":
    case "return":
    case "throw":
    case "expr-stmt":
    case "while":
    case "for":
      propositions.push({
        kind: "unsupported",
        reason: `lowerL1Body: '${stmt.kind}' out of scope for this slice`,
      });
      return false;
    default: {
      const _exhaustive: never = stmt;
      void _exhaustive;
      propositions.push({
        kind: "unsupported",
        reason: "lowerL1Body: unknown form",
      });
      return false;
    }
  }
}

function lowerAssign(
  stmt: Extract<IR1Stmt, { kind: "assign" }>,
  state: SymbolicState,
  propositions: PropResult[],
  ctx: LowerBodyCtx,
): boolean {
  if (stmt.target.kind !== "member") {
    propositions.push({
      kind: "unsupported",
      reason:
        "var-target assign is μ-search counter-only and not handled by lowerL1Body",
    });
    return false;
  }
  const objExpr = ctx.applyConst(lowerL1ExprToOpaque(stmt.target.receiver));
  const value = ctx.applyConst(lowerL1ExprToOpaque(stmt.value));
  const prop = stmt.target.name;
  const key = symbolicKey(prop, objExpr);
  state.writes = putWrite(state.writes, key, {
    kind: "property",
    prop,
    objExpr,
    value,
  });
  state.writtenKeys = addWrittenKey(state.writtenKeys, key);
  return true;
}

function lowerMapEffect(
  stmt: Extract<IR1Stmt, { kind: "map-effect" }>,
  state: SymbolicState,
  ctx: LowerBodyCtx,
): boolean {
  const objExpr = lowerL1ExprToOpaque(stmt.objExpr);
  const keyExpr = lowerL1ExprToOpaque(stmt.keyExpr);
  const valueExpr =
    stmt.valueExpr !== null ? lowerL1ExprToOpaque(stmt.valueExpr) : null;
  installMapWrite(
    state,
    {
      op: stmt.op,
      ruleName: stmt.ruleName,
      keyPredName: stmt.keyPredName,
      ownerType: stmt.ownerType,
      keyType: stmt.keyType,
      objExpr,
      keyExpr,
      valueExpr,
    },
    ctx.applyConst,
  );
  return true;
}

function lowerSetEffect(
  stmt: Extract<IR1Stmt, { kind: "set-effect" }>,
  state: SymbolicState,
  ctx: LowerBodyCtx,
): boolean {
  const objExpr = lowerL1ExprToOpaque(stmt.objExpr);
  const elemExpr =
    stmt.elemExpr !== null ? lowerL1ExprToOpaque(stmt.elemExpr) : null;
  installSetWrite(
    state,
    {
      op: stmt.op,
      ruleName: stmt.ruleName,
      ownerType: stmt.ownerType,
      elemType: stmt.elemType,
      objExpr,
      elemExpr,
    },
    ctx.applyConst,
  );
  return true;
}

function lowerForeach(
  stmt: Extract<IR1Stmt, { kind: "foreach" }>,
  state: SymbolicState,
  propositions: PropResult[],
  ctx: LowerBodyCtx,
): boolean {
  const ast = getAst();
  const arrExpr = ctx.applyConst(lowerL1ExprToOpaque(stmt.source));

  // Shape A: Sub-state captures the body's per-iteration writes so they
  // don't leak into the outer state. Skipped when body is null (pure
  // Shape B foreach).
  if (stmt.body !== null) {
    const subState = makeSymbolicState(ctx.applyConst);
    if (!lowerL1Body(stmt.body, subState, propositions, ctx)) {
      return false;
    }
    for (const [, entry] of subState.writes) {
      if (entry.kind !== "property") {
        propositions.push({
          kind: "unsupported",
          reason: `${entry.kind === "map" ? "Map" : "Set"} mutation inside foreach body is out of scope`,
        });
        return false;
      }
      // Universal-quantifier proposition:
      //   `all binder in src | prop' obj = value`
      propositions.push({
        kind: "equation",
        quantifiers: [],
        guards: [ast.gIn(stmt.binder, arrExpr)],
        lhs: ast.app(ast.primed(entry.prop), [entry.objExpr]),
        rhs: entry.value,
      });
      state.modifiedProps = addModifiedProp(state.modifiedProps, entry.prop);
    }
  }

  // Shape B: Each fold leaf becomes one accumulator equation
  //   prop' target = prior outerOp (combOP over each binder in src[, guard] | rhs)
  // The leaf carries pre-translated `target`, `rhs`, `guard` (with `rhs`/
  // `guard` already observing in-iter Shape A writes via the build-time
  // subState).
  for (const leaf of stmt.foldLeaves) {
    if (!lowerFoldLeaf(leaf, stmt.binder, arrExpr, state, propositions, ctx)) {
      return false;
    }
  }
  return true;
}

function lowerFoldLeaf(
  leaf: IR1FoldLeaf,
  binder: string,
  arrExpr: OpaqueExpr,
  state: SymbolicState,
  propositions: PropResult[],
  ctx: LowerBodyCtx,
): boolean {
  const ast = getAst();
  const target = ctx.applyConst(lowerL1ExprToOpaque(leaf.target));
  const rhs = ctx.applyConst(lowerL1ExprToOpaque(leaf.rhs));
  const guards = [ast.gIn(binder, arrExpr)];
  if (leaf.guard !== null) {
    guards.push(ast.gExpr(ctx.applyConst(lowerL1ExprToOpaque(leaf.guard))));
  }
  const comb =
    leaf.combiner === "add"
      ? ast.combAdd()
      : leaf.combiner === "mul"
        ? ast.combMul()
        : leaf.combiner === "and"
          ? ast.combAnd()
          : ast.combOr();
  const folded = ast.eachComb([], guards, comb, rhs);

  const outerOp = lowerBinop(leaf.outerOp);
  const key = symbolicKey(leaf.prop, target);
  const priorEntry = state.writes.get(key);
  if (priorEntry !== undefined && priorEntry.kind !== "property") {
    propositions.push({
      kind: "unsupported",
      reason: "loop-fold accumulator over a non-property prior write",
    });
    return false;
  }
  const priorVal = priorEntry?.value ?? ast.app(ast.var(leaf.prop), [target]);
  const newVal = ast.binop(outerOp, priorVal, folded);

  state.writes = putWrite(state.writes, key, {
    kind: "property",
    prop: leaf.prop,
    objExpr: target,
    value: newVal,
  });
  state.writtenKeys = addWrittenKey(state.writtenKeys, key);
  return true;
}

function lowerCondStmt(
  stmt: Extract<IR1Stmt, { kind: "cond-stmt" }>,
  state: SymbolicState,
  propositions: PropResult[],
  ctx: LowerBodyCtx,
): boolean {
  // Slice 1: single-arm cond-stmt with optional otherwise. Multi-armed
  // folds right-to-left (consistent with M1's expression-position cond
  // arm-folding) — deferred to a future slice when a test demands it.
  if (stmt.arms.length !== 1) {
    propositions.push({
      kind: "unsupported",
      reason: "multi-armed cond-stmt deferred",
    });
    return false;
  }
  const ast = getAst();
  const [guard, thenBody] = stmt.arms[0]!;
  const gExpr = ctx.applyConst(lowerL1ExprToOpaque(guard));

  const sT = cloneSymbolicState(state);
  if (!lowerL1Body(thenBody, sT, propositions, ctx)) {
    return false;
  }

  const sE = cloneSymbolicState(state);
  if (stmt.otherwise !== null) {
    if (!lowerL1Body(stmt.otherwise, sE, propositions, ctx)) {
      return false;
    }
  }

  // Merge per write-key. Property writes use cond-fallback to the
  // pre-state read; Map/Set writes merge their override lists via
  // mergeOverrides.
  const touched = new Set<string>([...sT.writtenKeys, ...sE.writtenKeys]);
  for (const key of touched) {
    const entryT = sT.writes.get(key);
    const entryE = sE.writes.get(key);
    const pick = (entryT ?? entryE)!;
    if (entryT && entryE && entryT.kind !== entryE.kind) {
      propositions.push({
        kind: "unsupported",
        reason:
          "branches wrote the same key with different kinds (property / map / set mismatch)",
      });
      return false;
    }
    if (pick.kind === "property") {
      const tP = entryT as PropertyWriteEntry | undefined;
      const eP = entryE as PropertyWriteEntry | undefined;
      const objExpr = pick.objExpr;
      const prop = pick.prop;
      // Identity fallback — pre-state read at this key. Used only when
      // *neither* the outer state nor the branch's clone had any prior
      // write for this key. If the outer had a prior write, the clone
      // copied it so `entryT?.value`/`entryE?.value` will hold that
      // outer value (and the merge will see it instead of the
      // pre-state).
      const identity = ast.app(ast.var(prop), [objExpr]);
      const vT = tP?.value ?? identity;
      const vE = eP?.value ?? identity;
      state.writes = putWrite(state.writes, key, {
        kind: "property",
        prop,
        objExpr,
        value: ast.cond([
          [gExpr, vT],
          [ast.litBool(true), vE],
        ]),
      });
      state.writtenKeys = addWrittenKey(state.writtenKeys, key);
      continue;
    }

    const combineCond = (vA: OpaqueExpr, vB: OpaqueExpr): OpaqueExpr =>
      ast.cond([
        [gExpr, vA],
        [ast.litBool(true), vB],
      ]);

    if (pick.kind === "map") {
      const tM = entryT as MapRuleWriteEntry | undefined;
      const eM = entryE as MapRuleWriteEntry | undefined;
      const base = tM ?? eM!;
      const ruleVar = ast.var(base.ruleName);
      const keyVar = ast.var(base.keyPredName);
      const valueFallback = (o: MapOverride): OpaqueExpr =>
        ast.app(ruleVar, [o.objExpr, o.keyExpr]);
      const memberFallback = (o: MapOverride): OpaqueExpr =>
        ast.app(keyVar, [o.objExpr, o.keyExpr]);
      const mergedValue = mergeOverrides(
        tM?.valueOverrides ?? [],
        eM?.valueOverrides ?? [],
        (o) => o.keyTuple,
        valueFallback,
        combineCond,
      );
      const mergedMember = mergeOverrides(
        tM?.membershipOverrides ?? [],
        eM?.membershipOverrides ?? [],
        (o) => o.keyTuple,
        memberFallback,
        combineCond,
      );
      state.writes = putWrite(state.writes, key, {
        kind: "map",
        ruleName: base.ruleName,
        keyPredName: base.keyPredName,
        ownerType: base.ownerType,
        keyType: base.keyType,
        valueOverrides: mergedValue,
        membershipOverrides: mergedMember,
      });
      state.writtenKeys = addWrittenKey(state.writtenKeys, key);
      continue;
    }

    // pick.kind === "set"
    const tS = entryT as SetRuleWriteEntry | undefined;
    const eS = entryE as SetRuleWriteEntry | undefined;
    const baseS = tS ?? eS!;
    const setRuleVar = ast.var(baseS.ruleName);
    const setFallback = (o: SetOverride): OpaqueExpr =>
      ast.binop(ast.opIn(), o.elemExpr, ast.app(setRuleVar, [baseS.objExpr]));
    const mergedSet = mergeOverrides(
      tS?.memberOverrides ?? [],
      eS?.memberOverrides ?? [],
      (o) => o.elemExpr,
      setFallback,
      combineCond,
    );
    state.writes = putWrite(state.writes, key, {
      kind: "set",
      ruleName: baseS.ruleName,
      ownerType: baseS.ownerType,
      elemType: baseS.elemType,
      objExpr: baseS.objExpr,
      memberOverrides: mergedSet,
      cleared: (tS?.cleared ?? false) || (eS?.cleared ?? false),
    });
    state.writtenKeys = addWrittenKey(state.writtenKeys, key);
  }

  return true;
}
