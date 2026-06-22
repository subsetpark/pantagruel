// Runner for the read-only monomorphization survey (workstream M5,
// ts2pant-opaque-mono-survey). Surveys ts2pant's own src/ corpus and writes a
// machine-readable JSON report plus a human-readable Markdown summary, then
// prints the M6 decision metric. Invoke: `npm run survey`.

import { readdirSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  formatMonoSurveyMarkdown,
  surveyMonomorphization,
} from "../src/mono-survey.js";

const here = dirname(fileURLToPath(import.meta.url));
const SRC = resolve(here, "../src");
const DOCS = resolve(here, "../docs");

function collectTsFiles(dir: string): string[] {
  const files: string[] = [];

  for (const entry of readdirSync(dir, { withFileTypes: true }).sort((a, b) =>
    a.name.localeCompare(b.name),
  )) {
    const fullPath = resolve(dir, entry.name);
    if (entry.isDirectory()) {
      files.push(...collectTsFiles(fullPath));
    } else if (entry.isFile() && entry.name.endsWith(".ts")) {
      files.push(fullPath);
    }
  }

  return files;
}

const files = collectTsFiles(SRC);

const report = surveyMonomorphization(files);

const date = process.env.SURVEY_DATE ?? "(date unset)";
const markdown = formatMonoSurveyMarkdown(report, {
  title: "Opaque Monomorphization Survey",
  date,
  corpus: "ts2pant `src/` (its own corpus)",
  notes: [
    "The dogfood corpus is a strict, self-hosted TypeScript codebase that avoids top-level `any`/`unknown` by construction, so it is **not representative** of the `any`/`unknown`-heavy user code M6 targets (third-party APIs, JSON, gradual code). A near-zero candidate count here reflects the corpus, not the value of monomorphization for real inputs.",
    "Treat this run as evidence that the dogfood corpus alone cannot justify M6. A go decision needs a re-survey over representative `any`/`unknown`-heavy user TypeScript; absent that, the workstream legitimately closes at M5 (the M5 Definition of Done permits this).",
  ],
});

writeFileSync(
  resolve(DOCS, "opaque-mono-survey.json"),
  `${JSON.stringify(report, null, 2)}\n`,
);
writeFileSync(resolve(DOCS, "opaque-mono-survey.md"), markdown);

// Console summary for the operator's M6 go/no-go.
process.stdout.write(
  [
    `files: ${report.files.length}`,
    `top-level functions: ${report.totalFunctions}`,
    `candidates (top-level any/unknown param): ${report.candidateFunctions}`,
    `dynamic-return functions (context): ${report.dynamicReturnFunctions}`,
    `by verdict: ${JSON.stringify(report.byVerdict)}`,
    `all-agree fraction (all candidates): ${(report.allAgreeFraction * 100).toFixed(1)}%`,
    `all-agree fraction (with call sites): ${(report.allAgreeFractionWithCallSites * 100).toFixed(1)}%`,
    "M6 threshold: >= 25% all-agree",
    "",
  ].join("\n"),
);
