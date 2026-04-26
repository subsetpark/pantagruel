/**
 * L1 statement → mutating-body Pantagruel propositions.
 *
 * Single-fold lowering: walks an `IR1Stmt`, threads the existing legacy
 * `SymbolicState` from `translate-body.ts`, and pushes `PropResult[]`.
 * Mirrors the shape of legacy `symbolicExecute` but operates on
 * canonicalized L1 input rather than raw TS AST. The benefit of L1 is
 * canonicalization — `if`-with-mutation, `for-of`, `forEach`,
 * compound-assign, etc. all collapse to one L1 shape so the fold has
 * one case per construct, not many.
 *
 * **Architectural commitment**: no parallel L2 statement vocabulary.
 * `lowerL1Body` calls into the existing legacy mutation primitives
 * (`putWrite`, `mergeOverrides`, etc.) — those are now the only
 * mutation-side infrastructure, owned jointly with `symbolicExecute`
 * during the cutover and inherited solely by `lowerL1Body` once
 * `symbolicExecute`'s mutation arms are deleted (Patches R4–R6).
 *
 * **R2 scope**: `block`, `assign(member, …)`, `cond-stmt`, `foreach`
 * (Shape A: per-iteration property writes). Other forms (`let`,
 * `return`, `throw`, `while`, `for`, `expr-stmt`, `assign(var, …)`,
 * Map/Set inside foreach) are out of scope and reject with a specific
 * reason. R3+ adds the build pass and Map/Set effect handling.
 */

import { lowerExpr } from "./ir-emit.js";
import type { IR1Expr, IR1Stmt } from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import type { OpaqueExpr, OpaqueParam } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import {
  addModifiedProp,
  addWrittenKey,
  cloneSymbolicState,
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

/**
 * Threading context for `lowerL1Body`. The const-binding closure
 * (`applyConst`) is the only piece that varies across recursion sites
 * today; iteration adds the iter binder name implicitly via the L1
 * `foreach` form, not through the context.
 */
export interface LowerBodyCtx {
  applyConst: (e: OpaqueExpr) => OpaqueExpr;
}

/**
 * Lower an L1 statement into the symbolic state and propositions.
 * Returns `false` if any sub-statement was unsupported (in which case
 * the propositions array already contains the `kind: "unsupported"`
 * markers).
 *
 * Mirrors legacy `symbolicExecute`'s contract: the state is mutated in
 * place; propositions are pushed; a returned `false` short-circuits
 * frame-condition emission at the caller.
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
        const inner = lowerL1Body(child, state, propositions, ctx);
        if (!inner) {
          ok = false;
        }
      }
      return ok;
    }

    case "assign":
      return lowerAssign(stmt, state, propositions, ctx);

    case "cond-stmt":
      return lowerCondStmt(stmt, state, propositions, ctx);

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
        reason: `lowerL1Body: L1 form '${stmt.kind}' is out of R2 scope (R3+ territory)`,
      });
      return false;

    default: {
      const _exhaustive: never = stmt;
      void _exhaustive;
      propositions.push({
        kind: "unsupported",
        reason: "lowerL1Body: unknown L1 form (unreachable)",
      });
      return false;
    }
  }
}

// ---------------------------------------------------------------------------
// assign
// ---------------------------------------------------------------------------

function lowerAssign(
  stmt: Extract<IR1Stmt, { kind: "assign" }>,
  state: SymbolicState,
  propositions: PropResult[],
  ctx: LowerBodyCtx,
): boolean {
  if (stmt.target.kind !== "member") {
    // `Var`-target assigns are reserved for μ-search counter steps and
    // are handled by `lowerL1MuSearch` at the expression layer, not by
    // general statement lowering.
    propositions.push({
      kind: "unsupported",
      reason: `lowerL1Body: assign with target kind '${stmt.target.kind}' is μ-search-only via lowerL1MuSearch`,
    });
    return false;
  }
  const objExpr = ctx.applyConst(lowerExpr(lowerL1Expr(stmt.target.receiver)));
  const value = ctx.applyConst(lowerExpr(lowerL1Expr(stmt.value)));
  const prop = stmt.target.name;
  const key = symbolicKey(prop, objExpr);
  putWrite(state, key, { kind: "property", prop, objExpr, value });
  addWrittenKey(state, key);
  return true;
}

// ---------------------------------------------------------------------------
// cond-stmt — fork sub-states, merge via cond per write-key
// ---------------------------------------------------------------------------

function lowerCondStmt(
  stmt: Extract<IR1Stmt, { kind: "cond-stmt" }>,
  state: SymbolicState,
  propositions: PropResult[],
  ctx: LowerBodyCtx,
): boolean {
  // Multi-armed cond-stmt right-folds into nested if/else: arm[0]'s
  // else branch is a synthetic cond-stmt for arms[1..] + otherwise.
  // The base case is a single arm (with the otherwise as its else
  // body) — mirroring M1's expression-position cond folding into
  // nested L2 cond arms.
  const [firstArm, ...restArms] = stmt.arms;
  const [guard, thenBody] = firstArm;
  let elseBody: IR1Stmt | null;
  if (restArms.length > 0) {
    const restHead = restArms[0]!;
    const restTail = restArms.slice(1);
    elseBody = {
      kind: "cond-stmt",
      arms: [restHead, ...restTail],
      otherwise: stmt.otherwise,
    };
  } else {
    elseBody = stmt.otherwise;
  }
  return lowerSimpleIfElse(guard, thenBody, elseBody, state, propositions, ctx);
}

function lowerSimpleIfElse(
  guard: IR1Expr,
  thenBody: IR1Stmt,
  elseBody: IR1Stmt | null,
  state: SymbolicState,
  propositions: PropResult[],
  ctx: LowerBodyCtx,
): boolean {
  const ast = getAst();
  const gExpr = ctx.applyConst(lowerExpr(lowerL1Expr(guard)));

  // Fork sub-states for then and else branches.
  const sT = cloneSymbolicState(state);
  const okT = lowerL1Body(thenBody, sT, propositions, ctx);
  if (!okT) {
    return false;
  }

  const sE = cloneSymbolicState(state);
  if (elseBody !== null) {
    const okE = lowerL1Body(elseBody, sE, propositions, ctx);
    if (!okE) {
      return false;
    }
  }

  // Merge per write-key, ported from legacy `symbolicExecute`'s
  // if-statement merge at translate-body.ts:5057-5170. Reuses the
  // existing `mergeOverrides` helper for Map/Set per-override merge;
  // property merge is a single cond per write-key with pre-state
  // identity fallback.
  const touched = new Set<string>([...sT.writtenKeys, ...sE.writtenKeys]);
  const combineCond = (vA: OpaqueExpr, vB: OpaqueExpr): OpaqueExpr =>
    ast.cond([
      [gExpr, vA],
      [ast.litBool(true), vB],
    ]);

  for (const key of touched) {
    const entryT = sT.writes.get(key);
    const entryE = sE.writes.get(key);
    const pick = (entryT ?? entryE)!;

    if (entryT && entryE && entryT.kind !== entryE.kind) {
      propositions.push({
        kind: "unsupported",
        reason: "branches wrote the same key with different kinds",
      });
      return false;
    }

    if (pick.kind === "property") {
      const tP = entryT as PropertyWriteEntry | undefined;
      const eP = entryE as PropertyWriteEntry | undefined;
      const objExpr = pick.objExpr;
      const prop = pick.prop;
      const identity = ast.app(ast.var(prop), [objExpr]);
      const vT = tP?.value ?? identity;
      const vE = eP?.value ?? identity;
      putWrite(state, key, {
        kind: "property",
        prop,
        objExpr,
        value: combineCond(vT, vE),
      });
      addWrittenKey(state, key);
      continue;
    }

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

    // Set
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
    putWrite(state, key, {
      kind: "set",
      ruleName: baseS.ruleName,
      ownerType: baseS.ownerType,
      elemType: baseS.elemType,
      objExpr: baseS.objExpr,
      memberOverrides: mergedSet,
      cleared: (tS?.cleared ?? false) || (eS?.cleared ?? false),
    });
    addWrittenKey(state, key);
  }

  return true;
}

// ---------------------------------------------------------------------------
// foreach (Shape A) — per-iteration property writes via gIn envelope
// ---------------------------------------------------------------------------

function lowerForeach(
  stmt: Extract<IR1Stmt, { kind: "foreach" }>,
  state: SymbolicState,
  propositions: PropResult[],
  ctx: LowerBodyCtx,
): boolean {
  const ast = getAst();
  const arrExpr = ctx.applyConst(lowerExpr(lowerL1Expr(stmt.source)));

  // Per-iteration sub-state — captures Shape A writes so they don't
  // leak into the outer state. Mirrors legacy
  // translate-body.ts:3383's `subState`.
  const subState = makeSymbolicState(ctx.applyConst);
  const okBody = lowerL1Body(stmt.body, subState, propositions, ctx);
  if (!okBody) {
    return false;
  }

  // Iterate sub-state writes. For Shape A each is a property write
  // emitted as a quantified equation `all x in arr | p' x = v.` —
  // empty quantifiers + gIn guard (matches legacy emission shape at
  // translate-body.ts:3415-3421).
  //
  // R2 scope: only property writes inside foreach. Map/Set mutation
  // inside loop bodies is rejected here for now (R5 territory).
  for (const [, entry] of subState.writes) {
    if (entry.kind !== "property") {
      propositions.push({
        kind: "unsupported",
        reason: `${entry.kind === "map" ? "Map" : "Set"} mutation inside foreach body is out of R2 scope`,
      });
      return false;
    }
    propositions.push({
      kind: "equation",
      quantifiers: [] as OpaqueParam[],
      guards: [ast.gIn(stmt.binder, arrExpr)],
      lhs: ast.app(ast.primed(entry.prop), [entry.objExpr]),
      rhs: entry.value,
    });
    addModifiedProp(state, entry.prop);
  }

  return true;
}
