import { lowerBinop, lowerExpr } from "./ir-emit.js";
import {
  type IR1Expr,
  type IR1FoldLeaf,
  type IR1ForeachBody,
  type IR1SsaLocation,
  type IR1SsaLoopBody,
  type IR1SsaLoopHeaderJoin,
  type IR1SsaProgram,
  type IR1SsaRuleName,
  type IR1SsaTerminationMetric,
  type IR1SsaVersion,
  type IR1SsaWrite,
  ir1Binop,
  ir1Cond,
  ir1Member,
  ir1SsaCloseLoopHeader,
  ir1SsaInitialVersion,
  ir1SsaIteratingSourceMetric,
  ir1SsaLoopBody,
  ir1SsaOpenLoopHeader,
  ir1SsaPropertyLocation,
  ir1SsaPropertyValue,
  ir1SsaRuleOfLocation,
  ir1SsaWrite,
} from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import type { PropResult } from "./types.js";

type UnsupportedDiagnostic = Extract<PropResult, { kind: "unsupported" }>;

export interface LoopSsaBuildOptions {
  declaredRules?: Iterable<IR1SsaRuleName>;
}

export interface ShapeAAsGeneralLoopOptions extends LoopSsaBuildOptions {
  binder: string;
  source: IR1Expr;
  body: IR1ForeachBody;
  lowerOpaque?: (e: OpaqueExpr) => OpaqueExpr;
  initialPropertyValues?: ReadonlyMap<string, OpaqueExpr>;
}

export interface ShapeBAsGeneralLoopOptions extends LoopSsaBuildOptions {
  binder: string;
  source: IR1Expr;
  foldLeaves: readonly IR1FoldLeaf[];
  lowerExpr?: (e: IR1Expr) => OpaqueExpr;
  priorAccumulatorValues?: ReadonlyMap<string, OpaqueExpr>;
}

export interface ForeachAsGeneralLoopResult {
  program: IR1SsaProgram;
  propositions: PropResult[];
  modifiedRules: IR1SsaRuleName[];
  diagnostics: UnsupportedDiagnostic[];
}

export interface ForeachShapeBAsGeneralLoopResult
  extends ForeachAsGeneralLoopResult {
  accumulatorKeys: string[];
}

export interface ShapeAPropertyEntry {
  location: Extract<IR1SsaLocation, { kind: "property" }>;
  objExpr: OpaqueExpr;
  value: OpaqueExpr;
  valueExpr: IR1Expr;
}

export interface ShapeASummaryState {
  current: Map<string, ShapeAPropertyEntry>;
  lowerOpaque: (e: OpaqueExpr) => OpaqueExpr;
  initialPropertyValues: ReadonlyMap<string, OpaqueExpr>;
}

export function lowerForeachShapeAAsGeneralLoop(
  options: ShapeAAsGeneralLoopOptions,
): ForeachAsGeneralLoopResult {
  const state: ShapeASummaryState = {
    current: new Map(),
    lowerOpaque: options.lowerOpaque ?? ((e) => e),
    initialPropertyValues: options.initialPropertyValues ?? new Map(),
  };
  const diagnostics: UnsupportedDiagnostic[] = [];

  if (!summarizeForeachShapeABody(options.body, state, diagnostics)) {
    return emptyForeachProgram(options, diagnostics);
  }

  const ast = getAst();
  const loweredSource = state.lowerOpaque(
    lowerExpr(lowerL1Expr(options.source)),
  );
  const metric = ir1SsaIteratingSourceMetric(options.source);
  const loopParts = [...state.current.values()].map((entry) =>
    buildForeachLoopBody(entry.location, entry.valueExpr, metric),
  );
  const propositions: PropResult[] = [...state.current.values()].map(
    (entry) => ({
      kind: "equation",
      quantifiers: [],
      guards: [ast.gIn(options.binder, loweredSource)],
      lhs: ast.app(ast.primed(entry.location.ruleName), [entry.objExpr]),
      rhs: entry.value,
    }),
  );
  const program = buildForeachProgram(options, loopParts);
  return {
    program,
    propositions,
    modifiedRules: program.modifiedRules,
    diagnostics,
  };
}

export function lowerForeachShapeBAsGeneralLoop(
  options: ShapeBAsGeneralLoopOptions,
): ForeachShapeBAsGeneralLoopResult {
  const ast = getAst();
  const lower = options.lowerExpr ?? ((e) => lowerExpr(lowerL1Expr(e)));
  const loweredSource = lower(options.source);
  const accumulatorKeys: string[] = [];
  const metric = ir1SsaIteratingSourceMetric(options.source);
  const loopParts: ForeachLoopPart[] = [];
  const propositions: PropResult[] = [];

  for (const leaf of options.foldLeaves) {
    const target = lower(leaf.target);
    const rhs = lower(leaf.rhs);
    const guards = [ast.gIn(options.binder, loweredSource)];
    if (leaf.guard !== null) {
      guards.push(ast.gExpr(lower(leaf.guard)));
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
    const key = foreachShapeBAccumulatorKey(leaf.prop, target);
    accumulatorKeys.push(key);
    const prior =
      options.priorAccumulatorValues?.get(key) ??
      ast.app(ast.var(leaf.prop), [target]);

    const location = ir1SsaPropertyLocation(leaf.prop, leaf.target, leaf.prop);
    loopParts.push(
      buildForeachLoopBody(
        location,
        ir1Binop(leaf.outerOp, ir1Member(leaf.target, leaf.prop), leaf.rhs),
        metric,
      ),
    );
    propositions.push({
      kind: "equation",
      quantifiers: [],
      lhs: ast.app(ast.primed(leaf.prop), [target]),
      rhs: ast.binop(lowerBinop(leaf.outerOp), prior, folded),
    });
  }

  const program = buildForeachProgram(options, loopParts);
  return {
    program,
    propositions,
    modifiedRules: program.modifiedRules,
    diagnostics: [],
    accumulatorKeys,
  };
}

interface ForeachLoopPart {
  header: IR1SsaLoopHeaderJoin;
  write: IR1SsaWrite;
  body: IR1SsaLoopBody;
}

function buildForeachLoopBody(
  location: IR1SsaLocation,
  value: IR1Expr,
  terminationMetric: IR1SsaTerminationMetric,
): ForeachLoopPart {
  const preheaderVersion: IR1SsaVersion = ir1SsaInitialVersion(location);
  const header = ir1SsaOpenLoopHeader(location, preheaderVersion);
  const write = ir1SsaWrite(location, ir1SsaPropertyValue(value));
  ir1SsaCloseLoopHeader(header, write.version);
  const body = ir1SsaLoopBody({
    headerJoins: [header],
    writes: [write],
    joins: [],
    breakHandles: [],
    continueHandles: [],
    returnHandles: [],
    throwHandles: [],
    terminationMetric,
  });
  return { header, write, body };
}

function buildForeachProgram(
  options: LoopSsaBuildOptions,
  loopParts: readonly ForeachLoopPart[],
): IR1SsaProgram {
  const modifiedRules = [
    ...new Set(
      loopParts.map((part) => ir1SsaRuleOfLocation(part.write.location)),
    ),
  ];
  const declaredRules = new Set([
    ...(options.declaredRules ?? []),
    ...modifiedRules,
  ]);
  return {
    reads: [],
    writes: loopParts.map((part) => part.write),
    joins: [],
    loopHeaderJoins: loopParts.map((part) => part.header),
    loopBodies: loopParts.map((part) => part.body),
    declaredRules: [...declaredRules],
    modifiedRules,
    framedRules: [...declaredRules].filter(
      (rule) => !modifiedRules.includes(rule),
    ),
  };
}

function emptyForeachProgram(
  options: LoopSsaBuildOptions,
  diagnostics: readonly UnsupportedDiagnostic[],
): ForeachAsGeneralLoopResult {
  const program = buildForeachProgram(options, []);
  return {
    program,
    propositions: [],
    modifiedRules: program.modifiedRules,
    diagnostics: [...diagnostics],
  };
}

export function summarizeForeachShapeABody(
  stmt: IR1ForeachBody,
  state: ShapeASummaryState,
  diagnostics: UnsupportedDiagnostic[],
): boolean {
  switch (stmt.kind) {
    case "assign":
      return summarizeShapeAAssign(stmt, state, diagnostics);
    case "block": {
      let ok = true;
      for (const child of stmt.stmts) {
        if (!summarizeForeachShapeABody(child, state, diagnostics)) {
          ok = false;
        }
      }
      return ok;
    }
    case "cond-stmt":
      return summarizeShapeACond(stmt, state, diagnostics);
    default: {
      diagnostics.push({
        kind: "unsupported",
        reason:
          "nested proposition-emitting loop body is not supported — the inner proposition would escape the outer iterator scope",
      });
      return false;
    }
  }
}

export function summarizeShapeAAssign(
  stmt: Extract<IR1ForeachBody, { kind: "assign" }>,
  state: ShapeASummaryState,
  diagnostics: UnsupportedDiagnostic[],
): boolean {
  if (stmt.target.kind !== "member") {
    diagnostics.push({
      kind: "unsupported",
      reason: "foreach Shape A summary assignment target must be a property",
    });
    return false;
  }

  const objExpr = state.lowerOpaque(
    lowerShapeAExpr(stmt.target.receiver, state),
  );
  const value = state.lowerOpaque(lowerShapeAExpr(stmt.value, state));
  const valueExpr = lowerShapeAExprToIR1(stmt.value, state);
  const location = ir1SsaPropertyLocation(
    stmt.target.name,
    stmt.target.receiver,
    stmt.target.name,
  ) as Extract<IR1SsaLocation, { kind: "property" }>;
  state.current.set(shapeAKey(stmt.target.name, objExpr), {
    location,
    objExpr,
    value,
    valueExpr,
  });
  return true;
}

export function summarizeShapeACond(
  stmt: Extract<IR1ForeachBody, { kind: "cond-stmt" }>,
  state: ShapeASummaryState,
  diagnostics: UnsupportedDiagnostic[],
): boolean {
  if (stmt.arms.length !== 1) {
    diagnostics.push({
      kind: "unsupported",
      reason: "multi-armed foreach Shape A conditional is not supported",
    });
    return false;
  }

  const ast = getAst();
  const [guard, thenBody] = stmt.arms[0]!;
  const before = new Map(state.current);
  const thenState: ShapeASummaryState = {
    ...state,
    current: new Map(state.current),
  };
  if (!summarizeForeachShapeABody(thenBody, thenState, diagnostics)) {
    return false;
  }

  const elseState: ShapeASummaryState = {
    ...state,
    current: new Map(before),
  };
  if (
    stmt.otherwise !== null &&
    !summarizeForeachShapeABody(stmt.otherwise, elseState, diagnostics)
  ) {
    return false;
  }

  const guardExpr = state.lowerOpaque(lowerShapeAExpr(guard, state));
  const guardIr1 = lowerShapeAExprToIR1(guard, state);
  const touched = new Set<string>();
  for (const [key, entry] of thenState.current) {
    if (before.get(key) !== entry) {
      touched.add(key);
    }
  }
  for (const [key, entry] of elseState.current) {
    if (before.get(key) !== entry) {
      touched.add(key);
    }
  }
  for (const key of touched) {
    const thenEntry = thenState.current.get(key);
    const elseEntry = elseState.current.get(key);
    const location = thenEntry?.location ?? elseEntry?.location;
    const objExpr = thenEntry?.objExpr ?? elseEntry?.objExpr;
    if (location === undefined || objExpr === undefined) {
      continue;
    }
    const prior = before.get(key);
    const fallback =
      prior?.value ??
      state.initialPropertyValues.get(key) ??
      ast.app(ast.var(location.ruleName), [objExpr]);
    const fallbackExpr =
      prior?.valueExpr ?? ir1Member(location.receiver, location.property);
    const thenValue = thenEntry?.value ?? fallback;
    const elseValue = elseEntry?.value ?? fallback;
    const thenValueExpr = thenEntry?.valueExpr ?? fallbackExpr;
    const elseValueExpr = elseEntry?.valueExpr ?? fallbackExpr;
    const value = ast.cond([
      [guardExpr, thenValue],
      [ast.litBool(true), elseValue],
    ]);
    const valueExpr = ir1Cond([[guardIr1, thenValueExpr]], elseValueExpr);
    state.current.set(key, { location, objExpr, value, valueExpr });
  }
  return true;
}

export function lowerShapeAExpr(
  expr: IR1Expr,
  state: ShapeASummaryState,
): OpaqueExpr {
  const ast = getAst();
  switch (expr.kind) {
    case "member": {
      const objExpr = state.lowerOpaque(lowerShapeAExpr(expr.receiver, state));
      const key = shapeAKey(expr.name, objExpr);
      return (
        state.current.get(key)?.value ??
        state.initialPropertyValues.get(key) ??
        ast.app(ast.var(expr.name), [objExpr])
      );
    }
    case "binop":
      return ast.binop(
        lowerBinop(expr.op),
        lowerShapeAExpr(expr.lhs, state),
        lowerShapeAExpr(expr.rhs, state),
      );
    case "unop": {
      const op =
        expr.op === "not"
          ? ast.opNot()
          : expr.op === "neg"
            ? ast.opNeg()
            : ast.opCard();
      return ast.unop(op, lowerShapeAExpr(expr.arg, state));
    }
    case "app": {
      const args = expr.args.map((arg) => lowerShapeAExpr(arg, state));
      if (expr.callee.kind === "var" && !expr.callee.primed) {
        return ast.app(ast.var(expr.callee.name), args);
      }
      return ast.app(lowerShapeAExpr(expr.callee, state), args);
    }
    case "cond": {
      const arms: Array<[OpaqueExpr, OpaqueExpr]> = expr.arms.map(([g, v]) => [
        lowerShapeAExpr(g, state),
        lowerShapeAExpr(v, state),
      ]);
      arms.push([ast.litBool(true), lowerShapeAExpr(expr.otherwise, state)]);
      return ast.cond(arms);
    }
    case "is-nullish":
      return ast.binop(
        ast.opEq(),
        ast.unop(ast.opCard(), lowerShapeAExpr(expr.operand, state)),
        ast.litNat(0),
      );
    case "each":
      return ast.each(
        [],
        [
          ast.gIn(expr.binder, lowerShapeAExpr(expr.src, state)),
          ...expr.guards.map((guard) =>
            ast.gExpr(lowerShapeAExpr(guard, state)),
          ),
        ],
        lowerShapeAExpr(expr.proj, state),
      );
    case "comb-typed":
      return ast.eachComb(
        [ast.param(expr.binder, ast.tName(expr.binderType))],
        expr.guards.map((guard) => ast.gExpr(lowerShapeAExpr(guard, state))),
        expr.combiner === "min" ? ast.combMin() : ast.combMax(),
        lowerShapeAExpr(expr.proj, state),
      );
    case "forall": {
      const guards =
        expr.guard === undefined
          ? []
          : [ast.gExpr(lowerShapeAExpr(expr.guard, state))];
      return ast.forall(
        [ast.param(expr.binder, ast.tName(expr.binderType))],
        guards,
        lowerShapeAExpr(expr.body, state),
      );
    }
    case "exists": {
      const guards =
        expr.guard === undefined
          ? []
          : [ast.gExpr(lowerShapeAExpr(expr.guard, state))];
      return ast.exists(
        [ast.param(expr.binder, ast.tName(expr.binderType))],
        guards,
        lowerShapeAExpr(expr.body, state),
      );
    }
    case "map-read": {
      const callee = expr.op === "has" ? expr.keyPredName : expr.ruleName;
      return ast.app(ast.var(callee), [
        lowerShapeAExpr(expr.receiver, state),
        lowerShapeAExpr(expr.key, state),
      ]);
    }
    case "set-read":
      return ast.binop(
        ast.opIn(),
        lowerShapeAExpr(expr.elem, state),
        ast.app(ast.var(expr.ruleName), [
          lowerShapeAExpr(expr.receiver, state),
        ]),
      );
    case "var":
    case "lit":
      return lowerExpr(lowerL1Expr(expr));
    default: {
      const _exhaustive: never = expr;
      void _exhaustive;
      throw new Error("unsupported IR1 expression in foreach Shape A summary");
    }
  }
}

function lowerShapeAExprToIR1(
  expr: IR1Expr,
  state: ShapeASummaryState,
): IR1Expr {
  if (expr.kind !== "member") {
    return expr;
  }
  const objExpr = state.lowerOpaque(lowerShapeAExpr(expr.receiver, state));
  const key = shapeAKey(expr.name, objExpr);
  return state.current.get(key)?.valueExpr ?? expr;
}

export function shapeAKey(prop: string, objExpr: OpaqueExpr): string {
  return `${prop}::${getAst().strExpr(objExpr)}`;
}

export function foreachShapeBAccumulatorKey(
  prop: string,
  objExpr: OpaqueExpr,
): string {
  return `${prop}::${getAst().strExpr(objExpr)}`;
}
