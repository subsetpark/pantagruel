/**
 * L1 statement → mutating-body Pantagruel propositions.
 *
 * Single-fold lowering: walks an `IR1Stmt`, threads the existing legacy
 * `SymbolicState` from `translate-body.ts`, and pushes `PropResult[]`.
 * Mirrors the shape of legacy `symbolicExecute` but operates on
 * canonicalized L1 input.
 *
 * Slice 1: handles `block`, `assign(member, …)`, `cond-stmt` (single-arm,
 * property writes only). Other forms reject with a specific reason —
 * subsequent slices grow the dispatch as needed.
 */

import { lowerExpr } from "./ir-emit.js";
import type { IR1Expr, IR1Stmt } from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import {
  addModifiedProp,
  addWrittenKey,
  cloneSymbolicState,
  installMapWrite,
  installSetWrite,
  makeSymbolicState,
  type MapOverride,
  type MapRuleWriteEntry,
  mergeOverrides,
  type PropertyWriteEntry,
  putWrite,
  type SetOverride,
  type SetRuleWriteEntry,
  symbolicKey,
  type SymbolicState,
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
  // Sub-state captures Shape A's per-iteration writes so they don't
  // leak into the outer state. Mirrors legacy `subState` discipline at
  // translate-body.ts:3383 (deleted in rip).
  const subState = makeSymbolicState(ctx.applyConst);
  if (!lowerL1Body(stmt.body, subState, propositions, ctx)) {
    return false;
  }
  // Iterate sub-state writes and emit per-iteration equations
  // (Shape A). Matches legacy emission shape exactly:
  //   `quantifiers: [], guards: [gIn(iter, src)], lhs: primed-rule, rhs`
  // (translate-body.ts:3415-3421, deleted in rip).
  //
  // Slice 4 = Shape A only: the write's receiver must mention the
  // iter binder as a free identifier. Writes whose receiver doesn't
  // reference the iter (Shape B accumulator folds, Map/Set mutations)
  // are deferred to later slices.
  const iterRefRe = new RegExp(`\\b${escapeRegex(stmt.binder)}\\b`);
  for (const [, entry] of subState.writes) {
    if (entry.kind !== "property") {
      propositions.push({
        kind: "unsupported",
        reason: `${entry.kind === "map" ? "Map" : "Set"} mutation inside foreach body is out of scope for this slice`,
      });
      return false;
    }
    if (!iterRefRe.test(ast.strExpr(entry.objExpr))) {
      propositions.push({
        kind: "unsupported",
        reason:
          "Shape B (accumulator fold inside foreach) is out of scope for this slice",
      });
      return false;
    }
    propositions.push({
      kind: "equation",
      quantifiers: [],
      guards: [ast.gIn(stmt.binder, arrExpr)],
      lhs: ast.app(ast.primed(entry.prop), [entry.objExpr]),
      rhs: entry.value,
    });
    state.modifiedProps = addModifiedProp(state.modifiedProps, entry.prop);
  }
  return true;
}

function escapeRegex(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
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

  // Merge per write-key — mirror what legacy `symbolicExecute`'s
  // if-statement arm did at translate-body.ts:5054-5170 (deleted in
  // the rip). Slice 1 supports property writes only; Map/Set merge
  // is Slice 2.
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
