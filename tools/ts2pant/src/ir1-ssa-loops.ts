import {
  type IR1SsaLocation,
  type IR1SsaLoopSummary,
  type IR1SsaProgram,
  type IR1SsaRuleName,
  ir1SsaLoopSummary,
  ir1SsaRuleOfLocation,
} from "./ir1.js";
import type { OpaqueExpr } from "./pant-ast.js";
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

export interface ForeachSummaryLowerResult {
  program: IR1SsaProgram;
  summary: IR1SsaLoopSummary | null;
  propositions: PropResult[];
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
