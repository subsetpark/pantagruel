import { lowerBinop, lowerExpr } from "./ir-emit.js";
import {
  type IR1Expr,
  type IR1FoldLeaf,
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

export interface ForeachShapeBSummaryInput extends LoopSummaryInputBase {
  kind: "foreach-shape-b";
  propositions?: readonly PropResult[];
}

export interface UnsupportedLoopSummaryInput {
  kind: "unsupported";
  reason: string;
}

export type IR1LoopSsaInput =
  | ForeachShapeBSummaryInput
  | UnsupportedLoopSummaryInput;

export interface LoopSsaBuildOptions {
  declaredRules?: Iterable<IR1SsaRuleName>;
}

export interface ForeachSummaryLowerOptions extends LoopSsaBuildOptions {
  input: ForeachShapeBSummaryInput;
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

export interface ForeachShapeBSummaryResult {
  program: IR1SsaProgram;
  summaries: IR1SsaLoopSummary[];
  propositions: PropResult[];
  modifiedRules: IR1SsaRuleName[];
  diagnostics: UnsupportedDiagnostic[];
  accumulatorKeys: string[];
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
      loopHeaderJoins: [],
      loopBodies: [],
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

export function foreachShapeBAccumulatorKey(
  prop: string,
  objExpr: OpaqueExpr,
): string {
  return `${prop}::${getAst().strExpr(objExpr)}`;
}
