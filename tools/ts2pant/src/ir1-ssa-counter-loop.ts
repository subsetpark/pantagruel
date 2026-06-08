// @archlint.module core
// @archlint.domain ts2pant.ir1-ssa-counter-loop

import { lowerBinop, lowerExpr } from "./ir-emit.js";
import {
  type IR1Expr,
  type IR1SsaLocation,
  type IR1SsaLoopBody,
  type IR1SsaProgram,
  type IR1SsaVersion,
  type IR1SsaWrite,
  type IR1Stmt,
  ir1Binop,
  ir1LitNat,
  ir1OpaqueOriginId,
  ir1SsaCloseLoopHeader,
  ir1SsaInitialVersion,
  ir1SsaLocalBindingLocation,
  ir1SsaLocalBindingValue,
  ir1SsaLoopBody,
  ir1SsaOpenLoopHeader,
  ir1SsaPropertyLocation,
  ir1SsaPropertyValue,
  ir1SsaRuleOfLocation,
  ir1SsaTerminationMetric,
  ir1SsaWrite,
  ir1Var,
} from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import type { LoopSsaBuildOptions } from "./ir1-ssa-foreach.js";
import {
  type IR1SsaBodyLowerResult,
  ir1SsaBodyLowerSuccess,
  ir1SsaBodyLowerUnsupported,
} from "./ir1-ssa-lower.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import type { PropResult } from "./types.js";

export interface CounterLoopLowerOptions extends LoopSsaBuildOptions {
  lowerOpaque?: (e: OpaqueExpr) => OpaqueExpr;
  initialPropertyValues?: ReadonlyMap<string, OpaqueExpr>;
}

export interface CounterLoopShape {
  counterName: string;
  counterPantBinder: string;
  initExpr: IR1Expr;
  boundExpr: IR1Expr;
  boundCmpOp: "lt" | "le" | "gt" | "ge";
  direction: "asc" | "desc";
  body: IR1Stmt;
}

interface CounterLoopCtx {
  counterPantBinder?: string;
}

interface TargetInfo {
  target: Extract<IR1Expr, { kind: "member" | "var" }>;
  location: Extract<IR1SsaLocation, { kind: "property" | "local-binding" }>;
  objExpr?: OpaqueExpr;
  lhs: OpaqueExpr;
  prior: OpaqueExpr;
  key: string;
}

type FoldCombiner = "add" | "mul" | "and" | "or";

interface AccumulatorFoldBody {
  kind: "accumulator-fold";
  target: Extract<IR1Expr, { kind: "member" | "var" }>;
  rhs: IR1Expr;
  guard: IR1Expr | null;
  combiner: FoldCombiner;
  outerOp: "add" | "sub" | "mul" | "div" | "and" | "or";
  continueGuards: IR1Expr[];
}

interface SimpleAssignBody {
  kind: "simple-assign";
  target: Extract<IR1Expr, { kind: "member" | "var" }>;
  rhs: IR1Expr;
  continueGuards: IR1Expr[];
}

type CounterLoopBody = AccumulatorFoldBody | SimpleAssignBody;

export function recognizeCounterLoopShape(
  stmt: Extract<IR1Stmt, { kind: "for" }>,
  ctx: CounterLoopCtx = {},
): CounterLoopShape | { unsupported: string } {
  if (stmt.init === null) {
    return { unsupported: "counter loop initializer is missing" };
  }
  const init = recognizeCounterInit(stmt.init);
  if ("unsupported" in init) {
    return init;
  }

  if (stmt.cond === null) {
    return { unsupported: "counter loop condition is missing" };
  }
  const cond = recognizeCounterCondition(stmt.cond, init.counterName);
  if ("unsupported" in cond) {
    return cond;
  }
  const checkedInit = recognizeCounterInit(stmt.init, cond.direction);
  if ("unsupported" in checkedInit) {
    return checkedInit;
  }

  if (stmt.step === null) {
    return { unsupported: "counter loop step is missing" };
  }
  const step = recognizeCounterStep(
    stmt.step,
    init.counterName,
    cond.direction,
  );
  if ("unsupported" in step) {
    return step;
  }

  return {
    counterName: init.counterName,
    counterPantBinder: ctx.counterPantBinder ?? init.counterName,
    initExpr: checkedInit.initExpr,
    boundExpr: cond.boundExpr,
    boundCmpOp: cond.boundCmpOp,
    direction: cond.direction,
    body: stmt.body,
  };
}

export function lowerCounterLoopL1Body(
  stmt: Extract<IR1Stmt, { kind: "for" }>,
  options: CounterLoopLowerOptions = {},
): IR1SsaBodyLowerResult {
  const shape = recognizeCounterLoopShape(stmt);
  if ("unsupported" in shape) {
    return ir1SsaBodyLowerUnsupported(shape.unsupported);
  }

  const body = classifyCounterLoopBody(shape.body, shape.counterName);
  if ("unsupported" in body) {
    return ir1SsaBodyLowerUnsupported(body.unsupported);
  }

  const lowerOpaque = options.lowerOpaque ?? ((e: OpaqueExpr) => e);
  const targetInfo = buildTargetInfo(body.target, lowerOpaque, options);
  const preheaderVersion: IR1SsaVersion = ir1SsaInitialVersion(
    targetInfo.location,
  );
  const header = ir1SsaOpenLoopHeader(targetInfo.location, preheaderVersion);
  const write: IR1SsaWrite = ir1SsaWrite(
    targetInfo.location,
    targetInfo.location.kind === "local-binding"
      ? ir1SsaLocalBindingValue(body.rhs)
      : ir1SsaPropertyValue(body.rhs),
  );
  ir1SsaCloseLoopHeader(header, write.version);

  const terminationMetricExpr =
    shape.direction === "asc"
      ? ir1Binop("sub", shape.boundExpr, ir1Var(shape.counterName))
      : ir1Binop("sub", ir1Var(shape.counterName), shape.boundExpr);
  const terminationMetric = ir1SsaTerminationMetric(
    terminationMetricExpr,
    ir1LitNat(0),
  );
  const loopBody: IR1SsaLoopBody = ir1SsaLoopBody({
    headerJoins: [header],
    writes: [write],
    joins: [],
    breakHandles: [],
    continueHandles: body.continueGuards.map(() => ({
      kind: "ssa-continue-handle",
      location: targetInfo.location,
      version: write.version,
    })),
    returnHandles: [],
    throwHandles: [],
    terminationMetric,
  });

  const proposition =
    body.kind === "accumulator-fold"
      ? emitAccumulatorFold(shape, body, targetInfo, lowerOpaque)
      : emitSimpleAssign(shape, body, targetInfo, lowerOpaque);
  const modifiedRules =
    targetInfo.location.kind === "local-binding"
      ? []
      : [ir1SsaRuleOfLocation(targetInfo.location)];
  const declaredRules = new Set([
    ...(options.declaredRules ?? []),
    ...modifiedRules,
  ]);
  const program: IR1SsaProgram = {
    reads: [],
    writes: [write],
    joins: [],
    loopHeaderJoins: [header],
    loopBodies: [loopBody],
    declaredRules: [...declaredRules],
    modifiedRules,
    framedRules: [...declaredRules].filter(
      (rule) => !modifiedRules.includes(rule),
    ),
  };

  return ir1SsaBodyLowerSuccess({
    programs: [program],
    propositions: [proposition],
    modifiedRules,
    ...(targetInfo.location.kind === "local-binding"
      ? {}
      : {
          finalProperties: [
            {
              location: targetInfo.location,
              version: write.version,
              objExpr: targetInfo.objExpr!,
              lhs: targetInfo.lhs,
              rhs: proposition.rhs,
            },
          ],
        }),
  });
}

function recognizeCounterInit(
  stmt: IR1Stmt,
  direction?: "asc" | "desc",
): { counterName: string; initExpr: IR1Expr } | { unsupported: string } {
  if (stmt.kind === "let") {
    if (direction === "asc" && !isNumericLiteral(stmt.value)) {
      return {
        unsupported: "counter loop initializer must be a numeric literal",
      };
    }
    if (direction === "desc" && exprReferencesVar(stmt.value, stmt.name)) {
      return {
        unsupported: "counter loop initializer must not reference the counter",
      };
    }
    return { counterName: stmt.name, initExpr: stmt.value };
  }
  if (stmt.kind === "assign" && stmt.target.kind === "var") {
    if (direction === "asc" && !isNumericLiteral(stmt.value)) {
      return {
        unsupported: "counter loop initializer must be a numeric literal",
      };
    }
    if (
      direction === "desc" &&
      exprReferencesVar(stmt.value, stmt.target.name)
    ) {
      return {
        unsupported: "counter loop initializer must not reference the counter",
      };
    }
    return { counterName: stmt.target.name, initExpr: stmt.value };
  }
  return {
    unsupported: "counter loop initializer must be a single counter binding",
  };
}

function recognizeCounterCondition(
  cond: IR1Expr,
  counterName: string,
):
  | {
      boundExpr: IR1Expr;
      boundCmpOp: "lt" | "le" | "gt" | "ge";
      direction: "asc" | "desc";
    }
  | { unsupported: string } {
  if (
    cond.kind !== "binop" ||
    !isCounterBoundCmpOp(cond.op) ||
    cond.lhs.kind !== "var" ||
    cond.lhs.name !== counterName
  ) {
    return {
      unsupported:
        "counter loop condition must be `counter < bound`, `counter <= bound`, `counter > bound`, or `counter >= bound`",
    };
  }
  if (exprReferencesVar(cond.rhs, counterName)) {
    return { unsupported: "counter loop bound must not reference the counter" };
  }
  const direction = cond.op === "lt" || cond.op === "le" ? "asc" : "desc";
  if (direction === "desc" && !isNumericLiteral(cond.rhs)) {
    return {
      unsupported: "descending counter loop bound must be a numeric literal",
    };
  }
  return { boundExpr: cond.rhs, boundCmpOp: cond.op, direction };
}

function recognizeCounterStep(
  step: IR1Stmt,
  counterName: string,
  direction: "asc" | "desc",
): { ok: true } | { unsupported: string } {
  if (
    step.kind !== "assign" ||
    step.target.kind !== "var" ||
    step.target.name !== counterName ||
    step.value.kind !== "binop" ||
    (step.value.op !== "add" && step.value.op !== "sub") ||
    step.value.lhs.kind !== "var" ||
    step.value.lhs.name !== counterName ||
    !isNatLiteral(step.value.rhs, 1)
  ) {
    return {
      unsupported:
        "counter loop step must be a canonical +1 increment or -1 decrement on the counter",
    };
  }
  if (
    (direction === "asc" && step.value.op !== "add") ||
    (direction === "desc" && step.value.op !== "sub")
  ) {
    return { unsupported: "counter loop direction is inconsistent" };
  }
  return { ok: true };
}

function classifyCounterLoopBody(
  stmt: IR1Stmt,
  counterName: string,
): CounterLoopBody | { unsupported: string } {
  if (stmt.kind === "block") {
    const continueGuards: IR1Expr[] = [];
    const rest = stmt.stmts.filter((child) => {
      const guard = recognizeContinueGuard(child);
      if (guard === null) {
        return true;
      }
      continueGuards.push(guard);
      return false;
    });
    if (rest.length !== 1) {
      return {
        unsupported:
          "bounded counter loop body must contain exactly one supported mutation",
      };
    }
    const classified = classifyCounterLoopBody(rest[0]!, counterName);
    if ("unsupported" in classified) {
      return classified;
    }
    return {
      ...classified,
      continueGuards: [...classified.continueGuards, ...continueGuards],
    };
  }
  if (stmt.kind === "cond-stmt") {
    if (
      stmt.arms.length !== 1 ||
      stmt.otherwise !== null ||
      stmt.arms[0] === undefined
    ) {
      return {
        unsupported:
          "guarded bounded counter loop body must be a single if without else",
      };
    }
    const [guard, thenBody] = stmt.arms[0];
    const classified = classifyCounterLoopBody(thenBody, counterName);
    if ("unsupported" in classified) {
      return classified;
    }
    if (classified.kind !== "accumulator-fold") {
      return {
        unsupported:
          "guarded bounded counter loop body must contain an accumulator fold",
      };
    }
    return { ...classified, guard };
  }
  if (
    stmt.kind !== "assign" ||
    (stmt.target.kind !== "member" && stmt.target.kind !== "var")
  ) {
    return {
      unsupported:
        "bounded counter loop body must be an accumulator fold or simple assignment",
    };
  }
  if (
    stmt.target.kind === "member" &&
    rootName(stmt.target.receiver) === counterName
  ) {
    return {
      unsupported: "counter loop body cannot assign through the counter",
    };
  }

  const selfReads = countTargetReads(stmt.value, stmt.target);
  if (stmt.value.kind === "binop" && sameTarget(stmt.value.lhs, stmt.target)) {
    const fold = isAccumulatorOuterOp(stmt.value.op)
      ? foldForBinop(stmt.value.op)
      : null;
    if (fold === null) {
      return {
        unsupported:
          "bounded counter loop accumulator fold uses an unsupported operator",
      };
    }
    if (countTargetReads(stmt.value.rhs, stmt.target) > 0 || selfReads > 1) {
      return recurrenceUnsupported();
    }
    return {
      kind: "accumulator-fold",
      target: stmt.target,
      rhs: stmt.value.rhs,
      guard: null,
      combiner: fold.combiner,
      outerOp: fold.outerOp,
      continueGuards: [],
    };
  }
  if (selfReads > 0) {
    return recurrenceUnsupported();
  }
  return {
    kind: "simple-assign",
    target: stmt.target,
    rhs: stmt.value,
    continueGuards: [],
  };
}

function buildTargetInfo(
  target: Extract<IR1Expr, { kind: "member" | "var" }>,
  lowerOpaque: (e: OpaqueExpr) => OpaqueExpr,
  options: CounterLoopLowerOptions,
): TargetInfo {
  const ast = getAst();
  if (target.kind === "var") {
    const location = ir1SsaLocalBindingLocation(target.name);
    const lhs = ast.var(target.name);
    return {
      target,
      location,
      lhs,
      prior: ast.var(target.name),
      key: `local-binding::${target.name}`,
    };
  }
  const objExpr = lowerOpaque(lowerExpr(lowerL1Expr(target.receiver)));
  const location = ir1SsaPropertyLocation(
    target.name,
    target.receiver,
    target.name,
  ) as Extract<IR1SsaLocation, { kind: "property" }>;
  const lhs = ast.app(ast.primed(target.name), [objExpr]);
  const key = `${target.name}::${ast.strExpr(objExpr)}`;
  const prior =
    options.initialPropertyValues?.get(key) ??
    ast.app(ast.var(target.name), [objExpr]);
  return { target, location, objExpr, lhs, prior, key };
}

function emitAccumulatorFold(
  shape: CounterLoopShape,
  body: AccumulatorFoldBody,
  target: TargetInfo,
  lowerOpaque: (e: OpaqueExpr) => OpaqueExpr,
): Extract<PropResult, { kind: "equation" }> {
  const ast = getAst();
  const binderExpr = ast.var(shape.counterPantBinder);
  const init = lowerOpaque(lowerExpr(lowerL1Expr(shape.initExpr)));
  const bound = lowerOpaque(lowerExpr(lowerL1Expr(shape.boundExpr)));
  const rhs = lowerWithCounter(body.rhs, shape, binderExpr, lowerOpaque);
  const guards = [
    ast.gExpr(
      ast.binop(
        shape.direction === "asc" ? ast.opGe() : ast.opLe(),
        binderExpr,
        init,
      ),
    ),
    ast.gExpr(ast.binop(boundCmpToOpaque(shape.boundCmpOp), binderExpr, bound)),
  ];
  if (body.guard !== null) {
    guards.push(
      ast.gExpr(lowerWithCounter(body.guard, shape, binderExpr, lowerOpaque)),
    );
  }
  for (const continueGuard of body.continueGuards) {
    guards.push(
      ast.gExpr(
        ast.unop(
          ast.opNot(),
          lowerWithCounter(continueGuard, shape, binderExpr, lowerOpaque),
        ),
      ),
    );
  }
  const folded = ast.eachComb(
    [ast.param(shape.counterPantBinder, ast.tName("Nat0"))],
    guards,
    combinerToOpaque(body.combiner),
    rhs,
  );
  return {
    kind: "equation",
    quantifiers: [],
    lhs: target.lhs,
    rhs: ast.binop(lowerBinop(body.outerOp), target.prior, folded),
  };
}

function emitSimpleAssign(
  shape: CounterLoopShape,
  body: SimpleAssignBody,
  target: TargetInfo,
  lowerOpaque: (e: OpaqueExpr) => OpaqueExpr,
): Extract<PropResult, { kind: "equation" }> {
  const ast = getAst();
  const init = lowerOpaque(lowerExpr(lowerL1Expr(shape.initExpr)));
  const bound = lowerOpaque(lowerExpr(lowerL1Expr(shape.boundExpr)));
  const lastCounter = lastCounterValue(shape, bound);
  const rhs = lowerWithCounter(body.rhs, shape, lastCounter, lowerOpaque);
  const continueGuard = lowerContinueGuardAt(
    body.continueGuards,
    shape,
    lastCounter,
    lowerOpaque,
  );
  const loopExecutes = ast.binop(
    boundCmpToOpaque(shape.boundCmpOp),
    init,
    bound,
  );
  const executedRhs =
    continueGuard === null
      ? rhs
      : ast.cond([
          [continueGuard, target.prior],
          [ast.litBool(true), rhs],
        ]);
  return {
    kind: "equation",
    quantifiers: [],
    lhs: target.lhs,
    rhs: ast.cond([
      [loopExecutes, executedRhs],
      [ast.litBool(true), target.prior],
    ]),
  };
}

function recognizeContinueGuard(stmt: IR1Stmt): IR1Expr | null {
  if (stmt.kind === "continue") {
    return { kind: "lit", value: { kind: "bool", value: true } };
  }
  if (
    stmt.kind === "cond-stmt" &&
    stmt.arms.length === 1 &&
    stmt.otherwise === null &&
    stmt.arms[0]?.[1].kind === "continue"
  ) {
    return stmt.arms[0][0];
  }
  return null;
}

function lowerContinueGuardAt(
  guards: readonly IR1Expr[],
  shape: CounterLoopShape,
  counter: OpaqueExpr,
  lowerOpaque: (e: OpaqueExpr) => OpaqueExpr,
): OpaqueExpr | null {
  const ast = getAst();
  if (guards.length === 0) {
    return null;
  }
  return guards
    .map((guard) => lowerWithCounter(guard, shape, counter, lowerOpaque))
    .reduce((lhs, rhs) => ast.binop(ast.opOr(), lhs, rhs));
}

function lastCounterValue(
  shape: CounterLoopShape,
  bound: OpaqueExpr,
): OpaqueExpr {
  const ast = getAst();
  switch (shape.boundCmpOp) {
    case "lt":
      return ast.binop(ast.opSub(), bound, ast.litNat(1));
    case "le":
    case "ge":
      return bound;
    case "gt":
      return ast.binop(ast.opAdd(), bound, ast.litNat(1));
    default: {
      const _exhaustive: never = shape.boundCmpOp;
      void _exhaustive;
      throw new Error("unsupported counter loop comparison");
    }
  }
}

function boundCmpToOpaque(cmp: CounterLoopShape["boundCmpOp"]) {
  const ast = getAst();
  switch (cmp) {
    case "lt":
      return ast.opLt();
    case "le":
      return ast.opLe();
    case "gt":
      return ast.opGt();
    case "ge":
      return ast.opGe();
    default: {
      const _exhaustive: never = cmp;
      void _exhaustive;
      throw new Error("unsupported counter loop comparison");
    }
  }
}

function lowerWithCounter(
  expr: IR1Expr,
  shape: CounterLoopShape,
  replacement: OpaqueExpr,
  lowerOpaque: (e: OpaqueExpr) => OpaqueExpr,
): OpaqueExpr {
  const ast = getAst();
  return lowerOpaque(
    ast.substituteBinder(
      lowerExpr(lowerL1Expr(expr)),
      shape.counterName,
      replacement,
    ),
  );
}

function combinerToOpaque(combiner: FoldCombiner) {
  const ast = getAst();
  switch (combiner) {
    case "add":
      return ast.combAdd();
    case "mul":
      return ast.combMul();
    case "and":
      return ast.combAnd();
    case "or":
      return ast.combOr();
    default: {
      const _exhaustive: never = combiner;
      void _exhaustive;
      throw new Error("unsupported bounded counter loop combiner");
    }
  }
}

function foldForBinop(
  op: AccumulatorFoldBody["outerOp"],
): { combiner: FoldCombiner; outerOp: AccumulatorFoldBody["outerOp"] } | null {
  switch (op) {
    case "add":
    case "sub":
      return { combiner: "add", outerOp: op };
    case "mul":
    case "div":
      return { combiner: "mul", outerOp: op };
    case "and":
      return { combiner: "and", outerOp: op };
    case "or":
      return { combiner: "or", outerOp: op };
    default:
      return null;
  }
}

function isAccumulatorOuterOp(
  op: string,
): op is AccumulatorFoldBody["outerOp"] {
  return (
    op === "add" ||
    op === "sub" ||
    op === "mul" ||
    op === "div" ||
    op === "and" ||
    op === "or"
  );
}

function recurrenceUnsupported(): { unsupported: string } {
  return {
    unsupported:
      "bounded counter loop body contains an accumulator self-recurrence; lift to L4 fixed-point lowering",
  };
}

function isNumericLiteral(expr: IR1Expr): boolean {
  return expr.kind === "lit" && expr.value.kind === "nat";
}

function isNatLiteral(expr: IR1Expr, value: number): boolean {
  return (
    expr.kind === "lit" &&
    expr.value.kind === "nat" &&
    expr.value.value === value
  );
}

function isCounterBoundCmpOp(op: string): op is CounterLoopShape["boundCmpOp"] {
  return op === "lt" || op === "le" || op === "gt" || op === "ge";
}

function exprReferencesVar(expr: IR1Expr, name: string): boolean {
  switch (expr.kind) {
    case "var":
      return expr.name === name;
    case "lit":
    case "opaque":
      return false;
    case "binop":
      return (
        exprReferencesVar(expr.lhs, name) || exprReferencesVar(expr.rhs, name)
      );
    case "unop":
      return exprReferencesVar(expr.arg, name);
    case "is-nullish":
      return exprReferencesVar(expr.operand, name);
    case "app":
      return (
        exprReferencesVar(expr.callee, name) ||
        expr.args.some((arg) => exprReferencesVar(arg, name))
      );
    case "member":
      return exprReferencesVar(expr.receiver, name);
    case "cond":
      return (
        expr.arms.some(
          ([guard, value]) =>
            exprReferencesVar(guard, name) || exprReferencesVar(value, name),
        ) || exprReferencesVar(expr.otherwise, name)
      );
    case "each":
      if (expr.binder === name) {
        return exprReferencesVar(expr.src, name);
      }
      return (
        exprReferencesVar(expr.src, name) ||
        expr.guards.some((guard) => exprReferencesVar(guard, name)) ||
        exprReferencesVar(expr.proj, name)
      );
    case "comb-typed":
      if (expr.binder === name) {
        return false;
      }
      return (
        expr.guards.some((guard) => exprReferencesVar(guard, name)) ||
        exprReferencesVar(expr.proj, name)
      );
    case "forall":
    case "exists":
      if (expr.binder === name) {
        return false;
      }
      return (
        (expr.guard !== undefined && exprReferencesVar(expr.guard, name)) ||
        exprReferencesVar(expr.body, name)
      );
    case "map-read":
      return (
        exprReferencesVar(expr.receiver, name) ||
        exprReferencesVar(expr.key, name)
      );
    case "set-read":
      return (
        exprReferencesVar(expr.receiver, name) ||
        exprReferencesVar(expr.elem, name)
      );
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      return false;
    }
  }
}

function countTargetReads(
  expr: IR1Expr,
  target: Extract<IR1Expr, { kind: "member" | "var" }>,
): number {
  const self = sameTarget(expr, target) ? 1 : 0;
  switch (expr.kind) {
    case "var":
    case "lit":
    case "opaque":
      return self;
    case "member":
      return self + countTargetReads(expr.receiver, target);
    case "binop":
      return (
        self +
        countTargetReads(expr.lhs, target) +
        countTargetReads(expr.rhs, target)
      );
    case "unop":
      return self + countTargetReads(expr.arg, target);
    case "app":
      return (
        self +
        countTargetReads(expr.callee, target) +
        expr.args.reduce((n, arg) => n + countTargetReads(arg, target), 0)
      );
    case "cond":
      return (
        self +
        expr.arms.reduce(
          (n, [guard, value]) =>
            n +
            countTargetReads(guard, target) +
            countTargetReads(value, target),
          0,
        ) +
        countTargetReads(expr.otherwise, target)
      );
    case "is-nullish":
      return self + countTargetReads(expr.operand, target);
    case "each":
      return (
        self +
        countTargetReads(expr.src, target) +
        expr.guards.reduce(
          (n, guard) => n + countTargetReads(guard, target),
          0,
        ) +
        countTargetReads(expr.proj, target)
      );
    case "comb-typed":
      return (
        self +
        expr.guards.reduce(
          (n, guard) => n + countTargetReads(guard, target),
          0,
        ) +
        countTargetReads(expr.proj, target)
      );
    case "forall":
    case "exists":
      return (
        self +
        (expr.guard === undefined ? 0 : countTargetReads(expr.guard, target)) +
        countTargetReads(expr.body, target)
      );
    case "map-read":
      return (
        self +
        countTargetReads(expr.receiver, target) +
        countTargetReads(expr.key, target)
      );
    case "set-read":
      return (
        self +
        countTargetReads(expr.receiver, target) +
        countTargetReads(expr.elem, target)
      );
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      return self;
    }
  }
}

function sameTarget(
  expr: IR1Expr,
  target: Extract<IR1Expr, { kind: "member" | "var" }>,
): boolean {
  if (target.kind === "var") {
    return expr.kind === "var" && expr.name === target.name;
  }
  return (
    expr.kind === "member" &&
    expr.name === target.name &&
    ir1ExprEqual(expr.receiver, target.receiver)
  );
}

function ir1ExprEqual(a: IR1Expr, b: IR1Expr): boolean {
  if (a.kind !== b.kind) {
    return false;
  }
  switch (a.kind) {
    case "var":
      return (
        b.kind === "var" &&
        a.name === b.name &&
        (a.primed ?? false) === (b.primed ?? false)
      );
    case "lit":
      return (
        b.kind === "lit" &&
        a.value.kind === b.value.kind &&
        "value" in a.value &&
        "value" in b.value &&
        a.value.value === b.value.value
      );
    case "opaque":
      return (
        b.kind === "opaque" &&
        a.sort === b.sort &&
        ir1OpaqueOriginId(a.origin) === ir1OpaqueOriginId(b.origin)
      );
    case "binop":
      return (
        b.kind === "binop" &&
        a.op === b.op &&
        ir1ExprEqual(a.lhs, b.lhs) &&
        ir1ExprEqual(a.rhs, b.rhs)
      );
    case "unop":
      return b.kind === "unop" && a.op === b.op && ir1ExprEqual(a.arg, b.arg);
    case "app":
      return (
        b.kind === "app" &&
        ir1ExprEqual(a.callee, b.callee) &&
        ir1ExprArrayEqual(a.args, b.args)
      );
    case "member":
      return (
        b.kind === "member" &&
        a.name === b.name &&
        ir1ExprEqual(a.receiver, b.receiver)
      );
    case "cond":
      return (
        b.kind === "cond" &&
        a.arms.length === b.arms.length &&
        a.arms.every(
          ([guard, value], index) =>
            b.arms[index] !== undefined &&
            ir1ExprEqual(guard, b.arms[index][0]) &&
            ir1ExprEqual(value, b.arms[index][1]),
        ) &&
        ir1ExprEqual(a.otherwise, b.otherwise)
      );
    case "is-nullish":
      return b.kind === "is-nullish" && ir1ExprEqual(a.operand, b.operand);
    case "each":
      return (
        b.kind === "each" &&
        a.binder === b.binder &&
        ir1ExprEqual(a.src, b.src) &&
        ir1ExprArrayEqual(a.guards, b.guards) &&
        ir1ExprEqual(a.proj, b.proj)
      );
    case "comb-typed":
      return (
        b.kind === "comb-typed" &&
        a.combiner === b.combiner &&
        a.binder === b.binder &&
        a.binderType === b.binderType &&
        ir1ExprArrayEqual(a.guards, b.guards) &&
        ir1ExprEqual(a.proj, b.proj)
      );
    case "forall":
      return (
        b.kind === "forall" &&
        a.binder === b.binder &&
        a.binderType === b.binderType &&
        optionalIr1ExprEqual(a.guard, b.guard) &&
        ir1ExprEqual(a.body, b.body)
      );
    case "exists":
      return (
        b.kind === "exists" &&
        a.binder === b.binder &&
        a.binderType === b.binderType &&
        optionalIr1ExprEqual(a.guard, b.guard) &&
        ir1ExprEqual(a.body, b.body)
      );
    case "map-read":
      return (
        b.kind === "map-read" &&
        a.op === b.op &&
        a.ruleName === b.ruleName &&
        a.keyPredName === b.keyPredName &&
        a.ownerType === b.ownerType &&
        a.keyType === b.keyType &&
        ir1ExprEqual(a.receiver, b.receiver) &&
        ir1ExprEqual(a.key, b.key)
      );
    case "set-read":
      return (
        b.kind === "set-read" &&
        a.ruleName === b.ruleName &&
        a.ownerType === b.ownerType &&
        a.elemType === b.elemType &&
        ir1ExprEqual(a.receiver, b.receiver) &&
        ir1ExprEqual(a.elem, b.elem)
      );
    default: {
      const _exhaustive: never = a;
      void _exhaustive;
      return false;
    }
  }
}

function ir1ExprArrayEqual(
  a: readonly IR1Expr[],
  b: readonly IR1Expr[],
): boolean {
  return (
    a.length === b.length &&
    a.every((expr, index) => ir1ExprEqual(expr, b[index]!))
  );
}

function optionalIr1ExprEqual(
  a: IR1Expr | undefined,
  b: IR1Expr | undefined,
): boolean {
  return a === undefined || b === undefined
    ? a === undefined && b === undefined
    : ir1ExprEqual(a, b);
}

function rootName(expr: IR1Expr): string | null {
  if (expr.kind === "var") {
    return expr.name;
  }
  if (expr.kind === "member") {
    return rootName(expr.receiver);
  }
  return null;
}
