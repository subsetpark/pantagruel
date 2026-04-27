/**
 * TS AST → L1 statements (mutating-body recognizers).
 *
 * Slice 1: `buildL1IfMutation` recognizes `if (g) { … }` / `if (g) { … }
 * else { … }` where each branch is a block (or single statement) of
 * simple property assignments `obj.p = v`. Anything more complex
 * (compound assigns, Map/Set effects in branches, nested ifs) returns
 * `{unsupported: …}` and the caller falls through to the legacy
 * rejection stub.
 *
 * Sub-expressions (condition, receiver, value) translate via the
 * existing legacy `translateBodyExpr` and wrap as `ir1FromL2`. The
 * lower pass unwraps via `lowerL1Expr` (passes the OpaqueExpr through
 * verbatim).
 *
 * Property names are qualified at build time via `qualifyFieldAccess`,
 * matching how the existing property-assign arm of `symbolicExecute`
 * does it. This keeps the canonicalized form consistent across L1
 * and legacy paths.
 */

import ts from "typescript";
import { irWrap } from "./ir.js";
import {
  ir1Assign,
  ir1Block,
  ir1CondStmt,
  ir1FromL2,
  ir1MapEffect,
  ir1Member,
  ir1SetEffect,
  type IR1Expr,
  type IR1Stmt,
} from "./ir1.js";
import type { OpaqueExpr } from "./pant-ast.js";
import {
  ambiguousFieldMsg,
  bodyExpr,
  expressionHasSideEffects,
  isBodyEffect,
  isBodyUnsupported,
  qualifyFieldAccess,
  type SymbolicState,
  translateBodyExpr,
  translateCallExpr,
  type UniqueSupply,
} from "./translate-body.js";
import type { NumericStrategy } from "./translate-types.js";

export interface BuildBodyCtx {
  checker: ts.TypeChecker;
  strategy: NumericStrategy;
  paramNames: ReadonlyMap<string, string>;
  state: SymbolicState;
  supply: UniqueSupply;
  applyConst: (e: OpaqueExpr) => OpaqueExpr;
}

export type BuildResult<T> = T | { unsupported: string };

export function isUnsupported<T>(
  x: BuildResult<T>,
): x is { unsupported: string } {
  return (
    typeof x === "object" &&
    x !== null &&
    "unsupported" in (x as Record<string, unknown>)
  );
}

/**
 * Recognize a TS `if`-statement with property-assignment branches and
 * build the canonical L1 `cond-stmt` form.
 */
export function buildL1IfMutation(
  stmt: ts.IfStatement,
  ctx: BuildBodyCtx,
): BuildResult<IR1Stmt> {
  if (expressionHasSideEffects(stmt.expression, ctx.checker)) {
    return { unsupported: "impure if-condition in mutating body" };
  }
  const gResult = translateBodyExpr(
    stmt.expression,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
  );
  if (isBodyUnsupported(gResult)) {
    return { unsupported: gResult.unsupported };
  }
  const guard: IR1Expr = ir1FromL2(irWrap(ctx.applyConst(bodyExpr(gResult))));

  const thenBody = buildL1MutationBody(stmt.thenStatement, ctx);
  if (isUnsupported(thenBody)) {
    return thenBody;
  }

  let elseBody: IR1Stmt | null = null;
  if (stmt.elseStatement) {
    const e = buildL1MutationBody(stmt.elseStatement, ctx);
    if (isUnsupported(e)) {
      return e;
    }
    elseBody = e;
  }

  return ir1CondStmt([[guard, thenBody]], elseBody);
}

/**
 * Build the L1 statement form for a single branch body. Accepts a
 * block of simple property-assignment statements or a single such
 * statement; rejects everything else.
 */
function buildL1MutationBody(
  stmt: ts.Statement,
  ctx: BuildBodyCtx,
): BuildResult<IR1Stmt> {
  if (ts.isBlock(stmt)) {
    const stmts: IR1Stmt[] = [];
    for (const child of stmt.statements) {
      const built = buildL1MutationBody(child, ctx);
      if (isUnsupported(built)) {
        return built;
      }
      stmts.push(built);
    }
    if (stmts.length === 0) {
      return { unsupported: "empty branch body" };
    }
    if (stmts.length === 1) {
      return stmts[0]!;
    }
    const [head, ...rest] = stmts;
    return ir1Block([head!, ...rest]);
  }
  if (ts.isExpressionStatement(stmt)) {
    if (ts.isCallExpression(stmt.expression)) {
      return buildL1EffectCall(stmt.expression, ctx);
    }
    return buildL1AssignStmt(stmt, ctx);
  }
  return {
    unsupported: `unsupported branch body kind: ${ts.SyntaxKind[stmt.kind]}`,
  };
}

/**
 * Build an L1 `map-effect` or `set-effect` from a Map/Set mutation
 * call inside a branch body. Reuses the existing `translateCallExpr`
 * recognizer which returns a `{ effect: CollectionMutation }` for
 * recognized `.set/.delete/.add/.delete/.clear` calls on Map/Set
 * receivers.
 */
function buildL1EffectCall(
  call: ts.CallExpression,
  ctx: BuildBodyCtx,
): BuildResult<IR1Stmt> {
  const result = translateCallExpr(
    call,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
  );
  if (isBodyUnsupported(result)) {
    return { unsupported: result.unsupported };
  }
  if (!isBodyEffect(result)) {
    return { unsupported: "branch call is not a recognized Map/Set effect" };
  }
  const effect = result.effect;
  if (effect.op === "set" || (effect.op === "delete" && "keyExpr" in effect)) {
    // Map mutation
    const m = effect as Extract<typeof effect, { op: "set" | "delete" }> & {
      keyExpr: OpaqueExpr;
    };
    return ir1MapEffect(
      m.op,
      m.ruleName,
      m.keyPredName,
      m.ownerType,
      m.keyType,
      ir1FromL2(irWrap(ctx.applyConst(m.objExpr))),
      ir1FromL2(irWrap(ctx.applyConst(m.keyExpr))),
      m.valueExpr !== null
        ? ir1FromL2(irWrap(ctx.applyConst(m.valueExpr)))
        : null,
    );
  }
  // Set mutation: op ∈ {add, delete, clear}, no keyExpr field
  const s = effect as Extract<
    typeof effect,
    { op: "add" | "delete" | "clear" }
  > & { elemExpr: OpaqueExpr | null };
  return ir1SetEffect(
    s.op,
    s.ruleName,
    s.ownerType,
    s.elemType,
    ir1FromL2(irWrap(ctx.applyConst(s.objExpr))),
    s.elemExpr !== null
      ? ir1FromL2(irWrap(ctx.applyConst(s.elemExpr)))
      : null,
  );
}

/**
 * Build an L1 `assign` statement from a TS property-assignment
 * expression statement. Slice 1 supports simple `=` only; compound
 * assigns (`+=` etc.) return `{unsupported}` and are deferred.
 */
function buildL1AssignStmt(
  stmt: ts.ExpressionStatement,
  ctx: BuildBodyCtx,
): BuildResult<IR1Stmt> {
  const expr = stmt.expression;
  if (!ts.isBinaryExpression(expr)) {
    return { unsupported: "branch statement must be an assignment" };
  }
  if (expr.operatorToken.kind !== ts.SyntaxKind.EqualsToken) {
    return { unsupported: "branch compound-assign deferred" };
  }
  if (!ts.isPropertyAccessExpression(expr.left)) {
    return { unsupported: "assign target must be a property access" };
  }
  const rawProp = expr.left.name.text;
  const receiverType = ctx.checker.getTypeAtLocation(expr.left.expression);
  const prop = qualifyFieldAccess(
    receiverType,
    rawProp,
    ctx.checker,
    ctx.strategy,
    ctx.supply.synthCell,
  );
  if (prop === null) {
    return { unsupported: ambiguousFieldMsg(rawProp) };
  }
  const objR = translateBodyExpr(
    expr.left.expression,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
  );
  if (isBodyUnsupported(objR)) {
    return { unsupported: objR.unsupported };
  }
  const valR = translateBodyExpr(
    expr.right,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
  );
  if (isBodyUnsupported(valR)) {
    return { unsupported: valR.unsupported };
  }
  const obj = ir1FromL2(irWrap(ctx.applyConst(bodyExpr(objR))));
  const val = ir1FromL2(irWrap(ctx.applyConst(bodyExpr(valR))));
  return ir1Assign(ir1Member(obj, prop), val);
}
