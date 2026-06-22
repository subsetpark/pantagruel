// @archlint.module core
// @archlint.domain ts2pant.mono-survey

import { Project } from "ts-morph";
import ts from "typescript";

import { COMPILER_OPTIONS } from "./extract.js";

// Read-only monomorphization survey (workstream ts2pant-any-unknown-opaque,
// milestone M5). For every function whose signature has a top-level
// `any`/`unknown` parameter, find its call sites across the program and
// record the observed argument type at each site, then classify how
// amenable the function is to call-site monomorphization (M6). This pass
// never changes translation output — it only measures.

/** Monomorphization-friendliness verdict for one candidate function. */
export type Verdict =
  | "all-agree"
  | "disagree-but-bounded"
  | "disagree-unbounded"
  | "recursive"
  | "oversized"
  | "no-visible-call-sites";

export interface ObservedArg {
  /** Parameter name the argument flows into. */
  readonly param: string;
  /** `checker.typeToString` of the (flow-narrowed) argument type. */
  readonly type: string;
  /** The observed type is itself `any`/`unknown` (pins no concrete type). */
  readonly dynamic: boolean;
}

export interface CallSiteSurvey {
  readonly file: string;
  readonly line: number;
  readonly observed: readonly ObservedArg[];
}

export interface FunctionSurvey {
  readonly file: string;
  readonly name: string;
  readonly line: number;
  readonly bodyLoc: number;
  readonly exported: boolean;
  /** Top-level `any`/`unknown` parameters — the monomorphization targets. */
  readonly dynamicParams: readonly string[];
  /** The return type is (top-level) `any`/`unknown`. Informational. */
  readonly dynamicReturn: boolean;
  readonly recursive: boolean;
  readonly callSites: readonly CallSiteSurvey[];
  /** Per dynamic param: the distinct observed argument types across sites. */
  readonly observedTypesByParam: Readonly<Record<string, readonly string[]>>;
  /**
   * Exported functions can be called from outside the surveyed program, so
   * their full caller set is unknown and the function cannot be soundly
   * replaced wholesale (only per-visible-site specialization is sound).
   */
  readonly externalCallersUnknown: boolean;
  readonly verdict: Verdict;
}

export interface MonoSurveyReport {
  readonly files: readonly string[];
  readonly totalFunctions: number;
  readonly candidateFunctions: number;
  readonly byVerdict: Readonly<Record<Verdict, number>>;
  /** all-agree / candidateFunctions. */
  readonly allAgreeFraction: number;
  /** all-agree / (candidates with >= 1 visible call site). */
  readonly allAgreeFractionWithCallSites: number;
  /** Context: functions with a top-level `any`/`unknown` return. */
  readonly dynamicReturnFunctions: number;
  readonly functions: readonly FunctionSurvey[];
}

/** Body line count above which a function is treated as oversized (tunable). */
export const OVERSIZED_LOC = 30;
/** Max distinct observed types for a position to count as "bounded". */
export const BOUNDED_MAX = 3;

const EMPTY_VERDICT_COUNTS: Record<Verdict, number> = {
  "all-agree": 0,
  "disagree-but-bounded": 0,
  "disagree-unbounded": 0,
  recursive: 0,
  oversized: 0,
  "no-visible-call-sites": 0,
};

function isDynamic(type: ts.Type): boolean {
  return (type.flags & (ts.TypeFlags.Any | ts.TypeFlags.Unknown)) !== 0;
}

function lineOf(node: ts.Node, sf: ts.SourceFile): number {
  return sf.getLineAndCharacterOfPosition(node.getStart(sf)).line + 1;
}

function isExported(node: ts.FunctionDeclaration): boolean {
  return (ts.getCombinedModifierFlags(node) & ts.ModifierFlags.Export) !== 0;
}

/**
 * Resolve a call's callee to a function declaration node, mirroring the
 * conservative idiom in translate-signature.ts: identifier calls only (no
 * dynamic dispatch), follow `getResolvedSignature` then a const-bound symbol,
 * and bail on node_modules / non-function targets.
 */
function resolveCalleeDecl(
  call: ts.CallExpression,
  checker: ts.TypeChecker,
): ts.SignatureDeclaration | undefined {
  if (!ts.isIdentifier(call.expression)) {
    return undefined;
  }
  const sig = checker.getResolvedSignature(call);
  let decl: ts.Declaration | undefined = sig?.declaration;
  if (!decl || !ts.isFunctionLike(decl)) {
    const symbol = checker.getSymbolAtLocation(call.expression);
    decl = symbol?.valueDeclaration;
  }
  if (!decl || !ts.isFunctionLike(decl)) {
    return undefined;
  }
  if (decl.getSourceFile().fileName.includes("node_modules")) {
    return undefined;
  }
  return decl;
}

interface Candidate {
  readonly node: ts.FunctionDeclaration;
  readonly sf: ts.SourceFile;
  readonly file: string;
  readonly name: string;
  readonly dynamicParamIndices: readonly { name: string; index: number }[];
  readonly dynamicReturn: boolean;
  readonly callSites: CallSiteSurvey[];
  recursive: boolean;
}

function collectCandidates(
  sf: ts.SourceFile,
  file: string,
  checker: ts.TypeChecker,
): Candidate[] {
  const out: Candidate[] = [];
  for (const stmt of sf.statements) {
    if (!ts.isFunctionDeclaration(stmt) || !stmt.body || !stmt.name) {
      continue;
    }
    const sig = checker.getSignatureFromDeclaration(stmt);
    if (!sig) {
      continue;
    }
    const dynamicParamIndices: { name: string; index: number }[] = [];
    sig.getParameters().forEach((param, index) => {
      const t = checker.getTypeOfSymbolAtLocation(
        param,
        param.valueDeclaration ?? stmt,
      );
      if (isDynamic(t)) {
        dynamicParamIndices.push({ name: param.getName(), index });
      }
    });
    const dynamicReturn = isDynamic(sig.getReturnType());
    if (dynamicParamIndices.length === 0) {
      // Not a param-monomorphization candidate; only track dynamic-return
      // context via a zero-param sentinel handled by the caller.
      if (dynamicReturn) {
        out.push({
          node: stmt,
          sf,
          file,
          name: stmt.name.text,
          dynamicParamIndices: [],
          dynamicReturn: true,
          callSites: [],
          recursive: false,
        });
      }
      continue;
    }
    out.push({
      node: stmt,
      sf,
      file,
      name: stmt.name.text,
      dynamicParamIndices,
      dynamicReturn,
      callSites: [],
      recursive: false,
    });
  }
  return out;
}

function classify(candidate: Candidate): {
  observedTypesByParam: Record<string, string[]>;
  verdict: Verdict;
} {
  const observedTypesByParam: Record<string, string[]> = {};
  for (const { name } of candidate.dynamicParamIndices) {
    observedTypesByParam[name] = [];
  }
  let sawDynamicObservation = false;
  for (const site of candidate.callSites) {
    for (const obs of site.observed) {
      const bucket = observedTypesByParam[obs.param];
      if (bucket && !bucket.includes(obs.type)) {
        bucket.push(obs.type);
      }
      if (obs.dynamic) {
        sawDynamicObservation = true;
      }
    }
  }

  let verdict: Verdict;
  if (candidate.recursive) {
    verdict = "recursive";
  } else if (
    candidate.node.body &&
    bodyLineCount(candidate.node.body, candidate.sf) > OVERSIZED_LOC
  ) {
    verdict = "oversized";
  } else if (candidate.callSites.length === 0) {
    verdict = "no-visible-call-sites";
  } else {
    const distinctCounts = Object.values(observedTypesByParam).map(
      (types) => types.length,
    );
    const maxDistinct = distinctCounts.reduce((a, b) => Math.max(a, b), 0);
    if (!sawDynamicObservation && maxDistinct === 1) {
      verdict = "all-agree";
    } else if (!sawDynamicObservation && maxDistinct <= BOUNDED_MAX) {
      verdict = "disagree-but-bounded";
    } else {
      verdict = "disagree-unbounded";
    }
  }
  return { observedTypesByParam, verdict };
}

function bodyLineCount(body: ts.Node, sf: ts.SourceFile): number {
  const start = sf.getLineAndCharacterOfPosition(body.getStart(sf)).line;
  const end = sf.getLineAndCharacterOfPosition(body.getEnd()).line;
  return end - start + 1;
}

/** Run the read-only monomorphization survey over the given source files. */
export function surveyMonomorphization(filePaths: string[]): MonoSurveyReport {
  const project = new Project({ compilerOptions: COMPILER_OPTIONS });
  project.addSourceFilesAtPaths(filePaths);
  project.resolveSourceFileDependencies();
  const program = project.getProgram().compilerObject;
  const checker = project.getTypeChecker().compilerObject;

  const requested = new Set(filePaths.map((p) => program.getSourceFile(p)));
  const inProgram = program
    .getSourceFiles()
    .filter(
      (sf) =>
        !program.isSourceFileDefaultLibrary(sf) &&
        !program.isSourceFileFromExternalLibrary(sf),
    );

  // Candidates come only from the requested files; call sites are scanned
  // across the whole (non-foreign) program so cross-file calls are visible.
  const byNode = new Map<ts.FunctionDeclaration, Candidate>();
  const candidates: Candidate[] = [];
  let totalFunctions = 0;
  for (const sf of inProgram) {
    if (!requested.has(sf)) {
      continue;
    }
    const file = baseName(sf.fileName);
    for (const stmt of sf.statements) {
      if (ts.isFunctionDeclaration(stmt) && stmt.body && stmt.name) {
        totalFunctions += 1;
      }
    }
    for (const c of collectCandidates(sf, file, checker)) {
      candidates.push(c);
      byNode.set(c.node, c);
    }
  }

  // Single pass over every call expression in the program: attribute each to
  // its callee candidate (if any) and record the observed argument types at
  // the dynamic parameter positions; flag direct recursion.
  for (const sf of inProgram) {
    const visit = (node: ts.Node): void => {
      if (ts.isCallExpression(node)) {
        const decl = resolveCalleeDecl(node, checker);
        if (decl && ts.isFunctionDeclaration(decl)) {
          const candidate = byNode.get(decl);
          if (candidate) {
            recordCall(candidate, node, sf, checker);
          }
        }
      }
      ts.forEachChild(node, visit);
    };
    visit(sf);
  }

  const functions: FunctionSurvey[] = candidates
    .filter((c) => c.dynamicParamIndices.length > 0)
    .map((c) => {
      const { observedTypesByParam, verdict } = classify(c);
      return {
        file: c.file,
        name: c.name,
        line: lineOf(c.node, c.sf),
        bodyLoc: c.node.body ? bodyLineCount(c.node.body, c.sf) : 0,
        exported: isExported(c.node),
        dynamicParams: c.dynamicParamIndices.map((p) => p.name),
        dynamicReturn: c.dynamicReturn,
        recursive: c.recursive,
        callSites: c.callSites,
        observedTypesByParam,
        externalCallersUnknown: isExported(c.node),
        verdict,
      };
    })
    .sort(
      (a, b) => a.file.localeCompare(b.file) || a.name.localeCompare(b.name),
    );

  const byVerdict = { ...EMPTY_VERDICT_COUNTS };
  for (const f of functions) {
    byVerdict[f.verdict] += 1;
  }
  const withCallSites = functions.filter((f) => f.callSites.length > 0).length;
  const allAgree = byVerdict["all-agree"];

  return {
    files: filePaths.map(baseName).sort(),
    totalFunctions,
    candidateFunctions: functions.length,
    byVerdict,
    allAgreeFraction: functions.length === 0 ? 0 : allAgree / functions.length,
    allAgreeFractionWithCallSites:
      withCallSites === 0 ? 0 : allAgree / withCallSites,
    dynamicReturnFunctions: candidates.filter(
      (c) => c.dynamicReturn && c.dynamicParamIndices.length === 0,
    ).length,
    functions,
  };
}

function recordCall(
  candidate: Candidate,
  call: ts.CallExpression,
  sf: ts.SourceFile,
  checker: ts.TypeChecker,
): void {
  // Direct recursion: the call is lexically inside the candidate's own body.
  if (callIsInside(call, candidate.node)) {
    candidate.recursive = true;
  }
  if (call.arguments.some(ts.isSpreadElement)) {
    return;
  }
  const observed: ObservedArg[] = [];
  for (const { name, index } of candidate.dynamicParamIndices) {
    const arg = call.arguments[index];
    if (!arg) {
      continue;
    }
    const t = checker.getTypeAtLocation(arg);
    observed.push({
      param: name,
      type: checker.typeToString(t),
      dynamic: isDynamic(t),
    });
  }
  candidate.callSites.push({
    file: baseName(sf.fileName),
    line: lineOf(call, sf),
    observed,
  });
}

function callIsInside(call: ts.Node, fn: ts.FunctionDeclaration): boolean {
  let n: ts.Node | undefined = call;
  while (n) {
    if (n === fn) {
      return true;
    }
    n = n.parent;
  }
  return false;
}

function baseName(path: string): string {
  return path.split(/[\\/]/u).pop() ?? path;
}

/** Threshold (all-agree fraction) at or above which M6 is justified. */
export const M6_THRESHOLD = 0.25;

/** Render the survey as a human-readable Markdown report. */
export function formatMonoSurveyMarkdown(
  report: MonoSurveyReport,
  meta: {
    title: string;
    date: string;
    corpus: string;
    /** Corpus-specific interpretive caveats appended to the verdict. */
    notes?: readonly string[];
  },
): string {
  const lines: string[] = [];
  lines.push(`# ${meta.title}`, "");
  lines.push(
    `**Workstream:** ts2pant \`any\`/\`unknown\` handling via Opaque sort — Milestone 5 (\`ts2pant-opaque-mono-survey\`). **Date:** ${meta.date}. **Method:** read-only whole-program pass over ${meta.corpus}; no translation output is modified.`,
    "",
  );
  lines.push("## Summary", "");
  lines.push(
    `- Files surveyed: **${report.files.length}**`,
    `- Top-level function declarations: **${report.totalFunctions}**`,
    `- Monomorphization candidates (>=1 top-level \`any\`/\`unknown\` parameter): **${report.candidateFunctions}**`,
    `- Functions with a top-level \`any\`/\`unknown\` return (context): **${report.dynamicReturnFunctions}**`,
    "",
  );
  lines.push("### Verdict distribution", "");
  lines.push("| Verdict | Count |", "| --- | --- |");
  for (const v of Object.keys(EMPTY_VERDICT_COUNTS) as Verdict[]) {
    lines.push(`| ${v} | ${report.byVerdict[v]} |`);
  }
  lines.push("");
  lines.push("### M6 decision metric", "");
  lines.push(
    `- all-agree fraction (of all candidates): **${pct(report.allAgreeFraction)}**`,
    `- all-agree fraction (of candidates with >=1 visible call site): **${pct(report.allAgreeFractionWithCallSites)}**`,
    `- Threshold to proceed to M6 (\`ts2pant-opaque-mono\`): **>= ${pct(M6_THRESHOLD)} all-agree**.`,
    "",
  );
  lines.push("### Verdict", "");
  const proceed = report.allAgreeFraction >= M6_THRESHOLD;
  lines.push(
    proceed
      ? `**Proceed to M6.** ${pct(report.allAgreeFraction)} of candidates are all-agree, at or above the ${pct(M6_THRESHOLD)} threshold.`
      : `**Do not proceed to M6 on this corpus.** ${pct(report.allAgreeFraction)} of candidates are all-agree, below the ${pct(M6_THRESHOLD)} threshold.`,
    "",
  );
  for (const note of meta.notes ?? []) {
    lines.push(`- ${note}`);
  }
  if (meta.notes && meta.notes.length > 0) {
    lines.push("");
  }
  lines.push("## Per-function detail", "");
  if (report.functions.length === 0) {
    lines.push("_No monomorphization candidates found in the corpus._", "");
  } else {
    lines.push(
      "| Function | Dynamic params | Call sites | Verdict | Exported | Body LOC |",
      "| --- | --- | --- | --- | --- | --- |",
    );
    for (const f of report.functions) {
      lines.push(
        `| \`${f.file}:${f.name}\` (L${f.line}) | ${f.dynamicParams.join(", ")} | ${f.callSites.length} | ${f.verdict} | ${f.externalCallersUnknown ? "yes" : "no"} | ${f.bodyLoc} |`,
      );
    }
    lines.push("");
    for (const f of report.functions) {
      const observed = Object.entries(f.observedTypesByParam)
        .map(([p, types]) => `${p}: {${types.join(", ") || "—"}}`)
        .join("; ");
      lines.push(`- \`${f.file}:${f.name}\` — observed: ${observed}`);
    }
    lines.push("");
  }
  return lines.join("\n");
}

function pct(x: number): string {
  return `${(x * 100).toFixed(1)}%`;
}
