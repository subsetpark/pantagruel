import { lowerBinop, lowerExpr } from "./ir-emit.js";
import {
  type IR1Expr,
  type IR1FoldLeaf,
  type IR1ForeachBody,
  type IR1SsaLocation,
  type IR1SsaLoopSummary,
  type IR1SsaProgram,
  type IR1SsaRuleName,
  ir1SsaLoopSummary,
  ir1SsaPropertyLocation,
  ir1SsaRuleOfLocation,
} from "./ir1.js";
import { lowerL1Expr } from "./ir1-lower.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import type { PropResult } from "./types.js";

export type LoopSsaShape = IR1SsaLoopSummary["shape"];

type UnsupportedDiagnostic = Extract<PropResult, { kind: "unsupported" }>;

interface LoopSummaryInputBase {
  location: IR1SsaLocation;
}

export interface MuSearchSummaryInput extends LoopSummaryInputBase {
  kind: "mu-search";
  loweredExpr?: OpaqueExpr;
}

export interface ForeachShapeASummaryInput extends LoopSummaryInputBase {
  kind: "foreach-shape-a";
  propositions?: readonly PropResult[];
}

export interface ForeachShapeBSummaryInput extends LoopSummaryInputBase {
  kind: "foreach-shape-b";
  propositions?: readonly PropResult[];
}

export interface UnsupportedLoopSummaryInput {
  kind: "unsupported";
  reason: string;
}

export type IR1LoopSsaInput =
  | MuSearchSummaryInput
  | ForeachShapeASummaryInput
  | ForeachShapeBSummaryInput
  | UnsupportedLoopSummaryInput;

export interface LoopSsaBuildOptions {
  declaredRules?: Iterable<IR1SsaRuleName>;
}

export interface ForeachSummaryLowerOptions extends LoopSsaBuildOptions {
  input: ForeachShapeASummaryInput | ForeachShapeBSummaryInput;
}

export interface ForeachShapeASummaryOptions extends LoopSsaBuildOptions {
  binder: string;
  source: OpaqueExpr;
  body: IR1ForeachBody;
  lowerOpaque?: (e: OpaqueExpr) => OpaqueExpr;
  initialPropertyValues?: ReadonlyMap<string, OpaqueExpr>;
}

export interface ForeachShapeBSummaryOptions extends LoopSsaBuildOptions {
  binder: string;
  source: OpaqueExpr;
  foldLeaves: readonly IR1FoldLeaf[];
  lowerExpr?: (e: IR1Expr) => OpaqueExpr;
  priorAccumulatorValues?: ReadonlyMap<string, OpaqueExpr>;
}

export interface ForeachSummaryLowerResult {
  program: IR1SsaProgram;
  summary: IR1SsaLoopSummary | null;
  propositions: PropResult[];
  modifiedRules: IR1SsaRuleName[];
  diagnostics: UnsupportedDiagnostic[];
}

export interface ForeachShapeASummaryResult {
  program: IR1SsaProgram;
  summaries: IR1SsaLoopSummary[];
  propositions: PropResult[];
  modifiedRules: IR1SsaRuleName[];
  diagnostics: UnsupportedDiagnostic[];
}

export interface ForeachShapeBSummaryResult {
  program: IR1SsaProgram;
  summaries: IR1SsaLoopSummary[];
  propositions: PropResult[];
  modifiedRules: IR1SsaRuleName[];
  diagnostics: UnsupportedDiagnostic[];
  accumulatorKeys: string[];
}

export interface MuSearchSummaryLowerOptions extends LoopSsaBuildOptions {
  location: IR1SsaLocation;
  counterType: string;
  counterPantName: string;
  binder: string;
  initExpr: OpaqueExpr;
  predicateExpr: OpaqueExpr;
}

export interface MuSearchSummaryLowerResult {
  program: IR1SsaProgram;
  summary: IR1SsaLoopSummary | null;
  loweredExpr: OpaqueExpr | null;
  modifiedRules: IR1SsaRuleName[];
  diagnostics: UnsupportedDiagnostic[];
}

export interface LoopSsaBuildResult {
  program: IR1SsaProgram;
  loweredExpr: OpaqueExpr[];
  propositions: PropResult[];
  diagnostics: UnsupportedDiagnostic[];
}

export function buildLoopSsaProgram(
  inputs: IR1LoopSsaInput | readonly IR1LoopSsaInput[],
  options: LoopSsaBuildOptions = {},
): LoopSsaBuildResult {
  const normalizedInputs = Array.isArray(inputs) ? inputs : [inputs];
  const loopSummaries: IR1SsaLoopSummary[] = [];
  const modifiedRules = new Set<IR1SsaRuleName>();
  const declaredRules = new Set<IR1SsaRuleName>(options.declaredRules ?? []);
  const loweredExpr: OpaqueExpr[] = [];
  const propositions: PropResult[] = [];
  const diagnostics: UnsupportedDiagnostic[] = [];

  for (const input of normalizedInputs) {
    if (input.kind === "unsupported") {
      diagnostics.push({ kind: "unsupported", reason: input.reason });
      continue;
    }

    const summary = ir1SsaLoopSummary(input.kind, input.location);
    loopSummaries.push(summary);

    const modifiedRule = ir1SsaRuleOfLocation(input.location);
    modifiedRules.add(modifiedRule);
    declaredRules.add(modifiedRule);

    if (input.kind === "mu-search") {
      if (input.loweredExpr !== undefined) {
        loweredExpr.push(input.loweredExpr);
      }
      continue;
    }

    if (input.propositions !== undefined) {
      propositions.push(...input.propositions);
    }
  }

  const framedRules = [...declaredRules].filter(
    (rule) => !modifiedRules.has(rule),
  );
  return {
    program: {
      reads: [],
      writes: [],
      joins: [],
      loopSummaries,
      declaredRules: [...declaredRules],
      modifiedRules: [...modifiedRules],
      framedRules,
    },
    loweredExpr,
    propositions,
    diagnostics,
  };
}

export function lowerForeachSummary(
  options: ForeachSummaryLowerOptions,
): ForeachSummaryLowerResult {
  const result = buildLoopSsaProgram(options.input, options);
  return {
    program: result.program,
    summary: result.program.loopSummaries[0] ?? null,
    propositions: result.propositions,
    modifiedRules: result.program.modifiedRules,
    diagnostics: result.diagnostics,
  };
}

interface ShapeAPropertyEntry {
  location: IR1SsaLocation;
  objExpr: OpaqueExpr;
  value: OpaqueExpr;
}

interface ShapeASummaryState {
  current: Map<string, ShapeAPropertyEntry>;
  lowerOpaque: (e: OpaqueExpr) => OpaqueExpr;
  initialPropertyValues: ReadonlyMap<string, OpaqueExpr>;
}

export function lowerForeachShapeASummaries(
  options: ForeachShapeASummaryOptions,
): ForeachShapeASummaryResult {
  const state: ShapeASummaryState = {
    current: new Map(),
    lowerOpaque: options.lowerOpaque ?? ((e) => e),
    initialPropertyValues: options.initialPropertyValues ?? new Map(),
  };
  const diagnostics: UnsupportedDiagnostic[] = [];

  if (!summarizeForeachShapeABody(options.body, state, diagnostics)) {
    const result = buildLoopSsaProgram(
      diagnostics.map((d) => ({ kind: "unsupported", reason: d.reason })),
      options,
    );
    return {
      program: result.program,
      summaries: [],
      propositions: [],
      modifiedRules: result.program.modifiedRules,
      diagnostics: result.diagnostics,
    };
  }

  const ast = getAst();
  const inputs: ForeachShapeASummaryInput[] = [...state.current.values()].map(
    (entry) => ({
      kind: "foreach-shape-a",
      location: entry.location,
      propositions: [
        {
          kind: "equation",
          quantifiers: [],
          guards: [ast.gIn(options.binder, options.source)],
          lhs: ast.app(ast.primed(entry.location.ruleName), [entry.objExpr]),
          rhs: entry.value,
        },
      ],
    }),
  );

  const result = buildLoopSsaProgram(inputs, options);
  return {
    program: result.program,
    summaries: result.program.loopSummaries,
    propositions: result.propositions,
    modifiedRules: result.program.modifiedRules,
    diagnostics: result.diagnostics,
  };
}

export function lowerForeachShapeBSummaries(
  options: ForeachShapeBSummaryOptions,
): ForeachShapeBSummaryResult {
  const ast = getAst();
  const lower = options.lowerExpr ?? ((e) => lowerExpr(lowerL1Expr(e)));
  const accumulatorKeys: string[] = [];
  const inputs: ForeachShapeBSummaryInput[] = options.foldLeaves.map((leaf) => {
    const target = lower(leaf.target);
    const rhs = lower(leaf.rhs);
    const guards = [ast.gIn(options.binder, options.source)];
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
    return {
      kind: "foreach-shape-b",
      location,
      propositions: [
        {
          kind: "equation",
          quantifiers: [],
          lhs: ast.app(ast.primed(leaf.prop), [target]),
          rhs: ast.binop(lowerBinop(leaf.outerOp), prior, folded),
        },
      ],
    };
  });

  const result = buildLoopSsaProgram(inputs, options);
  return {
    program: result.program,
    summaries: result.program.loopSummaries,
    propositions: result.propositions,
    modifiedRules: result.program.modifiedRules,
    diagnostics: result.diagnostics,
    accumulatorKeys,
  };
}

function summarizeForeachShapeABody(
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

function summarizeShapeAAssign(
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
  const location = ir1SsaPropertyLocation(
    stmt.target.name,
    stmt.target.receiver,
    stmt.target.name,
  );
  state.current.set(shapeAKey(stmt.target.name, objExpr), {
    location,
    objExpr,
    value,
  });
  return true;
}

function summarizeShapeACond(
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
    const thenValue = thenEntry?.value ?? fallback;
    const elseValue = elseEntry?.value ?? fallback;
    const value = ast.cond([
      [guardExpr, thenValue],
      [ast.litBool(true), elseValue],
    ]);
    state.current.set(key, { location, objExpr, value });
  }
  return true;
}

function lowerShapeAExpr(expr: IR1Expr, state: ShapeASummaryState): OpaqueExpr {
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

function shapeAKey(prop: string, objExpr: OpaqueExpr): string {
  return `${prop}::${getAst().strExpr(objExpr)}`;
}

export function foreachShapeBAccumulatorKey(
  prop: string,
  objExpr: OpaqueExpr,
): string {
  return `${prop}::${getAst().strExpr(objExpr)}`;
}

export function lowerMuSearchSummary(
  options: MuSearchSummaryLowerOptions,
): MuSearchSummaryLowerResult {
  const result = buildLoopSsaProgram(
    {
      kind: "mu-search",
      location: options.location,
    },
    options,
  );

  const ast = getAst();
  const predicateOpaque = ast.substituteBinder(
    options.predicateExpr,
    options.counterPantName,
    ast.var(options.binder),
  );
  const loweredExpr = ast.eachComb(
    [ast.param(options.binder, ast.tName(options.counterType))],
    [
      ast.gExpr(
        ast.binop(ast.opGe(), ast.var(options.binder), options.initExpr),
      ),
      ast.gExpr(ast.unop(ast.opNot(), predicateOpaque)),
    ],
    ast.combMin(),
    ast.var(options.binder),
  );

  return {
    program: result.program,
    summary: result.program.loopSummaries[0] ?? null,
    loweredExpr,
    modifiedRules: result.program.modifiedRules,
    diagnostics: result.diagnostics,
  };
}
