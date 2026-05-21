import type { IR1SsaProgram, IR1SsaRuleName } from "./ir1.js";
import type {
  CollectionSsaFinalPropertyEntry,
  CollectionSsaLowerResult,
} from "./ir1-ssa-collections.js";
import type {
  ForeachShapeASummaryResult,
  ForeachShapeBSummaryResult,
  ForeachSummaryLowerResult,
  MuSearchSummaryLowerResult,
} from "./ir1-ssa-loops.js";
import type {
  ScalarSsaFinalPropertyEntry,
  ScalarSsaLowerResult,
} from "./ir1-ssa-scalars.js";
import type { OpaqueExpr, OpaqueParam } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import type { PantDeclaration, PropResult } from "./types.js";

type UnsupportedDiagnostic = Extract<PropResult, { kind: "unsupported" }>;

export type IR1SsaFinalProperty =
  | ScalarSsaFinalPropertyEntry
  | CollectionSsaFinalPropertyEntry;

export interface IR1SsaBodyLowerResult {
  propositions: PropResult[];
  modifiedRules: IR1SsaRuleName[];
  diagnostics: UnsupportedDiagnostic[];
  programs: IR1SsaProgram[];
  returnValue: IR1SsaReturnValueEmission | null;
  finalProperties?: IR1SsaFinalProperty[];
}

export interface IR1SsaReturnValueEmission {
  ruleName: IR1SsaRuleName;
  expression: OpaqueExpr;
}

export interface IR1SsaBodyLowerSuccessOptions {
  propositions?: Iterable<PropResult>;
  modifiedRules?: Iterable<IR1SsaRuleName>;
  programs?: Iterable<IR1SsaProgram>;
  returnValue?: IR1SsaReturnValueEmission | null;
  finalProperties?: Iterable<IR1SsaFinalProperty>;
}

export interface IR1SsaBodyLowerUnsupportedOptions {
  diagnostics?: Iterable<UnsupportedDiagnostic>;
  programs?: Iterable<IR1SsaProgram>;
  propositions?: Iterable<PropResult>;
  modifiedRules?: Iterable<IR1SsaRuleName>;
  returnValue?: IR1SsaReturnValueEmission | null;
  finalProperties?: Iterable<IR1SsaFinalProperty>;
}

export function ir1SsaBodyLowerSuccess(
  options: IR1SsaBodyLowerSuccessOptions = {},
): IR1SsaBodyLowerResult {
  return {
    propositions: [...(options.propositions ?? [])],
    modifiedRules: dedupeRules(options.modifiedRules),
    diagnostics: [],
    programs: [...(options.programs ?? [])],
    returnValue: options.returnValue ?? null,
    ...(options.finalProperties === undefined
      ? {}
      : { finalProperties: [...options.finalProperties] }),
  };
}

export function ir1SsaBodyLowerUnsupported(
  reason: string,
  options: IR1SsaBodyLowerUnsupportedOptions = {},
): IR1SsaBodyLowerResult {
  return ir1SsaBodyLowerResult({
    diagnostics: [
      { kind: "unsupported", reason },
      ...(options.diagnostics ?? []),
    ],
    ...(options.propositions === undefined
      ? {}
      : { propositions: options.propositions }),
    ...(options.modifiedRules === undefined
      ? {}
      : { modifiedRules: options.modifiedRules }),
    ...(options.programs === undefined ? {} : { programs: options.programs }),
    ...(options.returnValue === undefined
      ? {}
      : { returnValue: options.returnValue }),
    ...(options.finalProperties === undefined
      ? {}
      : { finalProperties: options.finalProperties }),
  });
}

export function combineIR1SsaBodyLowerResults(
  ...results: readonly IR1SsaBodyLowerResult[]
): IR1SsaBodyLowerResult {
  const propositions: PropResult[] = [];
  const modifiedRules = new Set<IR1SsaRuleName>();
  const diagnostics: UnsupportedDiagnostic[] = [];
  const programs: IR1SsaProgram[] = [];
  const finalProperties: IR1SsaFinalProperty[] = [];
  let returnValue: IR1SsaReturnValueEmission | null = null;
  let hasFinalProperties = false;

  for (const result of results) {
    propositions.push(...result.propositions);
    diagnostics.push(...result.diagnostics);
    programs.push(...result.programs);
    for (const rule of result.modifiedRules) {
      modifiedRules.add(rule);
    }
    if (result.returnValue !== null) {
      if (returnValue !== null) {
        diagnostics.push({
          kind: "unsupported",
          reason: "multiple return-value emissions in one SSA body",
        });
      } else {
        returnValue = result.returnValue;
      }
    }
    if (result.finalProperties !== undefined) {
      hasFinalProperties = true;
      finalProperties.push(...result.finalProperties);
    }
  }

  return {
    propositions,
    modifiedRules: [...modifiedRules],
    diagnostics,
    programs,
    returnValue,
    ...(hasFinalProperties ? { finalProperties } : {}),
  };
}

export function appendFramesForUnmodifiedRules(
  result: IR1SsaBodyLowerResult,
  declarations: readonly PantDeclaration[],
): IR1SsaBodyLowerResult {
  if (result.diagnostics.length > 0) {
    return result;
  }

  const ast = getAst();
  const extraEquations: PropResult[] = [];
  const modifiedRules = new Set(result.modifiedRules);
  const framedRules = new Set<IR1SsaRuleName>();

  if (result.returnValue !== null) {
    const returnDecl = declarations.find(
      (decl) =>
        decl.kind === "rule" && decl.name === result.returnValue?.ruleName,
    );
    const actionDecl = declarations.find(
      (decl) =>
        decl.kind === "action" &&
        decl.label === actionLabelForRuleName(result.returnValue!.ruleName),
    );
    const params =
      returnDecl?.kind === "rule"
        ? returnDecl.params
        : actionDecl?.kind === "action"
          ? actionDecl.params
          : [];
    extraEquations.push({
      kind: "equation",
      quantifiers: [] as OpaqueParam[],
      lhs: ast.app(
        ast.primed(result.returnValue.ruleName),
        params.map((p) => ast.var(p.name)),
      ),
      rhs: result.returnValue.expression,
    });
  }

  for (const decl of declarations) {
    if (
      decl.kind !== "rule" ||
      modifiedRules.has(decl.name) ||
      result.returnValue?.ruleName === decl.name ||
      framedRules.has(decl.name)
    ) {
      continue;
    }
    framedRules.add(decl.name);
    const paramArgs = decl.params.map((p) => ast.var(p.name));
    extraEquations.push({
      kind: "equation",
      quantifiers: [] as OpaqueParam[],
      lhs: ast.app(ast.primed(decl.name), paramArgs),
      rhs: ast.app(ast.var(decl.name), paramArgs),
    });
  }

  if (extraEquations.length === 0) {
    return result;
  }

  return {
    ...result,
    propositions: [...result.propositions, ...extraEquations],
  };
}

export function scalarSsaBodyLowerResult(
  result: ScalarSsaLowerResult,
): IR1SsaBodyLowerResult {
  return ir1SsaBodyLowerResult({
    propositions: result.propositions,
    modifiedRules: result.modifiedRules,
    diagnostics: result.diagnostics,
    programs: [result.program],
    returnValue: null,
    finalProperties: result.finalProperties,
  });
}

export function collectionSsaBodyLowerResult(
  result: CollectionSsaLowerResult,
): IR1SsaBodyLowerResult {
  return ir1SsaBodyLowerResult({
    propositions: result.propositions,
    modifiedRules: result.modifiedRules,
    diagnostics: result.diagnostics,
    programs: [result.program],
    returnValue: null,
    finalProperties: result.finalProperties,
  });
}

export function loopSsaBodyLowerResult(
  result:
    | ForeachSummaryLowerResult
    | ForeachShapeASummaryResult
    | ForeachShapeBSummaryResult
    | MuSearchSummaryLowerResult,
): IR1SsaBodyLowerResult {
  return ir1SsaBodyLowerResult({
    propositions: "propositions" in result ? result.propositions : [],
    modifiedRules: result.modifiedRules,
    diagnostics: result.diagnostics,
    programs: [result.program],
    returnValue: null,
  });
}

function ir1SsaBodyLowerResult(
  options: IR1SsaBodyLowerUnsupportedOptions,
): IR1SsaBodyLowerResult {
  return {
    propositions: [...(options.propositions ?? [])],
    modifiedRules: dedupeRules(options.modifiedRules),
    diagnostics: [...(options.diagnostics ?? [])],
    programs: [...(options.programs ?? [])],
    returnValue: options.returnValue ?? null,
    ...(options.finalProperties === undefined
      ? {}
      : { finalProperties: [...options.finalProperties] }),
  };
}

function dedupeRules(
  rules: Iterable<IR1SsaRuleName> | undefined,
): IR1SsaRuleName[] {
  return rules === undefined ? [] : [...new Set(rules)];
}

function actionLabelForRuleName(ruleName: IR1SsaRuleName): string {
  return ruleName.charAt(0).toUpperCase() + ruleName.slice(1);
}
