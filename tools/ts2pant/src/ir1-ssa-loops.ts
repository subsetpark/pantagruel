import type {
  IR1SsaLoopSummary,
  IR1SsaProgram,
  IR1SsaRuleName,
} from "./ir1.js";
import type { OpaqueExpr } from "./pant-ast.js";
import type { PropResult } from "./types.js";

export type LoopSsaShape = IR1SsaLoopSummary["shape"];

type UnsupportedDiagnostic = Extract<PropResult, { kind: "unsupported" }>;

export interface UnsupportedLoopSummaryInput {
  kind: "unsupported";
  reason: string;
}

export type IR1LoopSsaInput = UnsupportedLoopSummaryInput;

export interface LoopSsaBuildOptions {
  declaredRules?: Iterable<IR1SsaRuleName>;
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
  const declaredRules = new Set<IR1SsaRuleName>(options.declaredRules ?? []);
  const diagnostics: UnsupportedDiagnostic[] = [];

  for (const input of normalizedInputs) {
    diagnostics.push({ kind: "unsupported", reason: input.reason });
  }

  return {
    program: {
      reads: [],
      writes: [],
      joins: [],
      loopSummaries: [],
      loopHeaderJoins: [],
      loopBodies: [],
      declaredRules: [...declaredRules],
      modifiedRules: [],
      framedRules: [...declaredRules],
    },
    loweredExpr: [],
    propositions: [],
    diagnostics,
  };
}
