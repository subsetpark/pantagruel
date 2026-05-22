/**
 * L1 statement -> mutating-body Pantagruel propositions.
 *
 * This module is the body-level IR1 SSA result boundary. It recognizes
 * supported scalar, collection, and loop L1 statement shapes,
 * lowers them through the dedicated SSA helpers, and appends frames from
 * the resulting `modifiedRules` set. Unsupported statements fail closed
 * with diagnostics; there is no symbolic-state reducer fallback here.
 *
 * SSA reference: Cytron, Ferrante, Rosen, Wegman, and Zadeck,
 * "Efficiently Computing Static Single Assignment Form and the Control
 * Dependence Graph", ACM TOPLAS 13(4), 1991,
 * https://doi.org/10.1145/115372.115320. The boundary functions in this
 * file preserve the SSA-style invariants used by the lowerers: each emitted
 * write has one versioned definition, branch/loop helpers report explicit
 * joins before equation emission, and `modifiedRules` remains
 * the set of definitions that suppresses frame generation.
 */

import { lowerExpr } from "./ir-emit.js";
import type { IR1Stmt } from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import {
  isCollectionSsaL1Body,
  lowerCollectionSsaToProps,
} from "./ir1-ssa-collections.js";
import { lowerCounterLoopL1Body } from "./ir1-ssa-counter-loop.js";
import { lowerFixedPointLoopL1Body } from "./ir1-ssa-fixed-point.js";
import {
  type ForeachAsGeneralLoopResult,
  lowerForeachShapeAAsGeneralLoop,
  lowerForeachShapeBAsGeneralLoop,
} from "./ir1-ssa-foreach.js";
import {
  appendFramesForUnmodifiedRules,
  collectionSsaBodyLowerResult,
  combineIR1SsaBodyLowerResults,
  type IR1SsaBodyLowerResult,
  ir1SsaBodyLowerUnsupported,
  loopSsaBodyLowerResult,
  scalarSsaBodyLowerResult,
} from "./ir1-ssa-lower.js";
import { isScalarSsaL1Body, lowerScalarSsaToProps } from "./ir1-ssa-scalars.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { symbolicKey } from "./translate-body.js";
import type { NumericStrategy, SynthCell } from "./translate-types.js";
import type { PantDeclaration } from "./types.js";

export interface LowerBodyCtx {
  applyConst: (e: OpaqueExpr) => OpaqueExpr;
  ssaResultBoundary?: boolean;
}

export function adaptCollectionSsaLowerResult(
  result: ReturnType<typeof lowerCollectionSsaToProps>,
): IR1SsaBodyLowerResult {
  return collectionSsaBodyLowerResult(result);
}

export function adaptScalarSsaLowerResult(
  result: ReturnType<typeof lowerScalarSsaToProps>,
): IR1SsaBodyLowerResult {
  return scalarSsaBodyLowerResult(result);
}

export function adaptLoopSsaLowerResult(
  result: ForeachAsGeneralLoopResult,
): IR1SsaBodyLowerResult {
  return loopSsaBodyLowerResult(result);
}

export interface ScalarSsaBodyLowerOptions extends LowerBodyCtx {
  initialPropertyValues?: ReadonlyMap<string, OpaqueExpr>;
}

export interface SsaBodyLowerOptions extends LowerBodyCtx {
  initialPropertyValues?: ReadonlyMap<string, OpaqueExpr>;
  synthCell?: SynthCell;
  strategy?: NumericStrategy;
  declarations?: readonly PantDeclaration[];
  returnRuleName?: string;
}

export function lowerL1BodyToSsaProps(
  stmt: IR1Stmt,
  declarations: readonly PantDeclaration[],
  options: SsaBodyLowerOptions,
): IR1SsaBodyLowerResult {
  const lowerDeclarations = options.declarations ?? declarations;
  return appendFramesForUnmodifiedRules(
    lowerL1BodyToSsaResult(stmt, {
      ...options,
      declarations: lowerDeclarations,
    }),
    declarations,
  );
}

function lowerL1BodyToSsaResult(
  stmt: IR1Stmt,
  options: SsaBodyLowerOptions,
): IR1SsaBodyLowerResult {
  if (isCollectionSsaL1Body(stmt)) {
    return lowerCollectionL1BodyToSsaResult(stmt, options);
  }
  if (isScalarSsaL1Body(stmt)) {
    return lowerScalarL1BodyToSsaResult(stmt, options);
  }
  if (stmt.kind === "foreach") {
    return lowerForeachL1BodyToSsaResult(stmt, options);
  }
  if (stmt.kind === "while") {
    return lowerFixedPointL1BodyToSsaResult(stmt, options);
  }
  if (stmt.kind === "for") {
    if (hasBreakOrReturn(stmt.body)) {
      const whileStmt: IR1Stmt = {
        kind: "while",
        cond: stmt.cond ?? {
          kind: "lit",
          value: { kind: "bool", value: true },
        },
        body:
          stmt.step === null
            ? stmt.body
            : { kind: "block", stmts: [stmt.body, stmt.step] },
      };
      const stmts =
        stmt.init === null
          ? ([whileStmt] as [IR1Stmt])
          : ([stmt.init, whileStmt] as [IR1Stmt, IR1Stmt]);
      return lowerFixedPointL1BodyToSsaResult(
        {
          kind: "block",
          stmts,
        },
        options,
      );
    }
    return lowerCounterLoopL1BodyToSsaResult(stmt, options);
  }
  if (
    stmt.kind === "block" &&
    stmt.stmts.length > 0 &&
    stmt.stmts.at(-1)?.kind === "while" &&
    stmt.stmts.slice(0, -1).every((child) => child.kind === "let")
  ) {
    return lowerFixedPointL1BodyToSsaResult(stmt, options);
  }
  if (stmt.kind === "block") {
    const results: IR1SsaBodyLowerResult[] = [];
    const initialPropertyValues = new Map(options.initialPropertyValues);
    for (const child of stmt.stmts) {
      const result = lowerL1BodyToSsaResult(child, {
        ...options,
        initialPropertyValues,
      });
      results.push(result);
      if (result.diagnostics.length > 0) {
        continue;
      }
      for (const entry of result.finalProperties ?? []) {
        const prop = "location" in entry ? entry.location.ruleName : entry.prop;
        initialPropertyValues.set(symbolicKey(prop, entry.objExpr), entry.rhs);
      }
    }
    return combineIR1SsaBodyLowerResults(...results);
  }
  return ir1SsaBodyLowerUnsupported(
    "statement is not supported by unified SSA body lowering",
  );
}

function hasBreakOrReturn(stmt: IR1Stmt): boolean {
  switch (stmt.kind) {
    case "break":
    case "return":
      return true;
    case "block":
      return stmt.stmts.some(hasBreakOrReturn);
    case "cond-stmt":
      return (
        stmt.arms.some(([, body]) => hasBreakOrReturn(body)) ||
        (stmt.otherwise !== null && hasBreakOrReturn(stmt.otherwise))
      );
    case "while":
    case "for":
    case "foreach":
      return false;
    case "let":
    case "assign":
    case "continue":
    case "throw":
    case "expr-stmt":
    case "map-effect":
    case "set-effect":
      return false;
    default: {
      const _exhaustive: never = stmt;
      void _exhaustive;
      return false;
    }
  }
}

function lowerFixedPointL1BodyToSsaResult(
  stmt:
    | Extract<IR1Stmt, { kind: "while" }>
    | Extract<IR1Stmt, { kind: "block" }>,
  options: SsaBodyLowerOptions,
): IR1SsaBodyLowerResult {
  try {
    return lowerFixedPointLoopL1Body(stmt, {
      lowerOpaque: options.applyConst,
      ...(options.initialPropertyValues === undefined
        ? {}
        : { initialPropertyValues: options.initialPropertyValues }),
      ...(options.synthCell === undefined
        ? {}
        : { synthCell: options.synthCell }),
      ...(options.strategy === undefined ? {} : { strategy: options.strategy }),
      ...(options.declarations === undefined
        ? {}
        : { declarations: options.declarations }),
      ...(options.returnRuleName === undefined
        ? {}
        : { returnRuleName: options.returnRuleName }),
    });
  } catch (err) {
    return ir1SsaBodyLowerUnsupported(
      err instanceof Error
        ? err.message
        : "fixed-point while SSA lowering rejected the body",
    );
  }
}

function lowerCounterLoopL1BodyToSsaResult(
  stmt: Extract<IR1Stmt, { kind: "for" }>,
  options: SsaBodyLowerOptions,
): IR1SsaBodyLowerResult {
  try {
    return lowerCounterLoopL1Body(stmt, {
      lowerOpaque: options.applyConst,
      ...(options.initialPropertyValues === undefined
        ? {}
        : { initialPropertyValues: options.initialPropertyValues }),
    });
  } catch (err) {
    return ir1SsaBodyLowerUnsupported(
      err instanceof Error
        ? err.message
        : "counter loop SSA lowering rejected the body",
    );
  }
}

export function lowerCollectionL1BodyToSsaResult(
  stmt: IR1Stmt,
  ctx: LowerBodyCtx,
): IR1SsaBodyLowerResult {
  try {
    return adaptCollectionSsaLowerResult(
      lowerCollectionSsaToProps(stmt, {
        lowerOpaque: ctx.applyConst,
      }),
    );
  } catch (err) {
    return ir1SsaBodyLowerUnsupported(
      err instanceof Error
        ? err.message
        : "collection SSA lowering rejected the body",
    );
  }
}

export function lowerScalarL1BodyToSsaResult(
  stmt: IR1Stmt,
  options: ScalarSsaBodyLowerOptions,
): IR1SsaBodyLowerResult {
  try {
    return adaptScalarSsaLowerResult(
      lowerScalarSsaToProps(stmt, {
        lowerOpaque: options.applyConst,
        ...(options.initialPropertyValues === undefined
          ? {}
          : { initialPropertyValues: options.initialPropertyValues }),
      }),
    );
  } catch (err) {
    return ir1SsaBodyLowerUnsupported(
      err instanceof Error
        ? err.message
        : "scalar SSA lowering rejected the body",
    );
  }
}

function lowerForeachL1BodyToSsaResult(
  stmt: Extract<IR1Stmt, { kind: "foreach" }>,
  options: SsaBodyLowerOptions,
): IR1SsaBodyLowerResult {
  const results: IR1SsaBodyLowerResult[] = [];

  if (stmt.body !== null) {
    results.push(
      adaptLoopSsaLowerResult(
        lowerForeachShapeAAsGeneralLoop({
          binder: stmt.binder,
          source: stmt.source,
          body: stmt.body,
          lowerOpaque: options.applyConst,
          ...(options.initialPropertyValues === undefined
            ? {}
            : { initialPropertyValues: options.initialPropertyValues }),
        }),
      ),
    );
  }

  if (stmt.foldLeaves.length > 0) {
    results.push(
      adaptLoopSsaLowerResult(
        lowerForeachShapeBAsGeneralLoop({
          binder: stmt.binder,
          source: stmt.source,
          foldLeaves: stmt.foldLeaves,
          lowerExpr: (expr) => options.applyConst(lowerExpr(lowerL1Expr(expr))),
          priorAccumulatorValues: new Map(),
        }),
      ),
    );
  }

  if (results.length === 0) {
    return ir1SsaBodyLowerUnsupported("empty foreach body");
  }
  return combineIR1SsaBodyLowerResults(...results);
}
