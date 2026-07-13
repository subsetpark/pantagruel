// @archlint.module exempt
// @archlint.exempt-reason read-only diagnostic script

import { readdirSync } from "node:fs";
import { relative, resolve } from "node:path";
import process from "node:process";
import { emitDocument } from "../src/emit.js";
import { createSourceFile } from "../src/extract.js";
import { buildPantDocument } from "../src/pipeline.js";
import { IntStrategy } from "../src/translate-types.js";

const SRC_DIR = resolve(import.meta.dirname, "../src");
const SUMMARY_ONLY = process.argv.includes("--summary-only");
const OWNER_THRESHOLD = 10;

// M5 ownership is deliberately attached to normalized diagnostic contracts,
// not inferred from source syntax. A new high-volume reason therefore appears
// as UNASSIGNED until its semantic owner is audited explicitly.
const RESIDUAL_OWNERS = new Map<string, string>([
  [
    "alias IR1ForeachCondStmt: discriminated union could not be registered for tagged Pantagruel encoding",
    "DU/type-shape encoding",
  ],
  ["early-return predicate has side effects", "guard/foreign-call purity"],
  ["early-return value has side effects", "guard/foreign-call purity"],
  [
    "if-with-return block must contain only const bindings followed by a return",
    "follow-up: generalized nested sequencing",
  ],
  [
    "for-of loop is not a recognized build-list comprehension",
    "follow-up: generalized accumulator/search loops",
  ],
  [
    "expression statement before return (only const / μ-search / if-early-return allowed)",
    "follow-up: local-effect/statement SSA",
  ],
  [
    "Map builder construction is not supported in this milestone",
    "follow-up: Map builder semantics",
  ],
  [
    "unsupported pure expression in const initializer",
    "expression/type lowering",
  ],
  [
    "local bindings or multiple statements before return",
    "follow-up: generalized local sequencing",
  ],
  [
    "Set builder from iterable is not supported",
    "follow-up: collection constructor semantics",
  ],
  [
    "switch default must end with `return EXPR`",
    "intentional: switch exhaustiveness/throw semantics",
  ],
  [
    "switch case must end with `return EXPR` (no fall-through, no break/throw cases in M1)",
    "intentional: switch fall-through/break/throw semantics",
  ],
  [
    "statement is not supported by unified SSA body lowering",
    "follow-up: statement-position SSA",
  ],
  [
    "branch call is not a recognized Map/Set effect",
    "follow-up: branch-local collection effects",
  ],
  [
    "let captured by closure that reassigns it is not supported",
    "intentional: closure conversion",
  ],
]);

interface DiagnosticResult {
  file: string;
  functionName: string;
  output?: string;
  unsupportedReasons: string[];
  error?: string;
}

function sourceFiles(dir: string): string[] {
  const out: string[] = [];
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    const path = resolve(dir, entry.name);
    if (entry.isDirectory()) {
      out.push(...sourceFiles(path));
      continue;
    }
    if (entry.isFile() && entry.name.endsWith(".ts")) {
      out.push(path);
    }
  }
  return out.sort();
}

function unsupportedReasons(output: string): string[] {
  const reasons: string[] = [];
  const re = /^> UNSUPPORTED: (?<reason>.+?)\.?$/gmu;
  for (const match of output.matchAll(re)) {
    const reason = match.groups?.reason;
    if (reason) {
      reasons.push(reason);
    }
  }
  return reasons;
}

function normalizeReason(reason: string): string {
  return reason
    .replace(/^[^—]+ — /u, "")
    .replace(/\b\d+\b/gu, "<n>")
    .trim();
}

function printDocument(result: DiagnosticResult): void {
  console.log(`\n===== ${result.file} > ${result.functionName} =====\n`);
  if (result.output !== undefined) {
    console.log(result.output.trimEnd());
    return;
  }
  console.log(`ERROR: ${result.error ?? "unknown error"}`);
}

const results: DiagnosticResult[] = [];

for (const filePath of sourceFiles(SRC_DIR)) {
  const relFile = relative(SRC_DIR, filePath);
  const sourceFile = createSourceFile(filePath);
  try {
    const functionNames = sourceFile
      .getFunctions()
      .map((fn) => fn.getName())
      .filter((name): name is string => name !== undefined)
      .sort();

    for (const functionName of functionNames) {
      try {
        const doc = await buildPantDocument({
          sourceFile,
          functionName,
          strategy: IntStrategy,
        });
        const output = emitDocument(doc);
        const result = {
          file: relFile,
          functionName,
          output,
          unsupportedReasons: unsupportedReasons(output),
        };
        results.push(result);
        if (!SUMMARY_ONLY) {
          printDocument(result);
        }
      } catch (err) {
        const result = {
          file: relFile,
          functionName,
          unsupportedReasons: [],
          error: err instanceof Error ? err.message : String(err),
        };
        results.push(result);
        if (!SUMMARY_ONLY) {
          printDocument(result);
        }
      }
    }
  } finally {
    sourceFile.getProject().removeSourceFile(sourceFile);
  }
}

const unsupportedBuckets = new Map<string, number>();
const errorBuckets = new Map<string, number>();
let clean = 0;
let unsupported = 0;
let errored = 0;

for (const result of results) {
  if (result.error !== undefined) {
    errored += 1;
    const bucket = `ERROR: ${result.error.split("\n", 1)[0]}`;
    errorBuckets.set(bucket, (errorBuckets.get(bucket) ?? 0) + 1);
    continue;
  }
  if (result.unsupportedReasons.length === 0) {
    clean += 1;
    continue;
  }
  unsupported += 1;
  for (const reason of result.unsupportedReasons) {
    const bucket = normalizeReason(reason);
    unsupportedBuckets.set(bucket, (unsupportedBuckets.get(bucket) ?? 0) + 1);
  }
}

console.log("\n===== SUMMARY =====");
console.log(`functions: ${results.length}`);
console.log(`clean: ${clean}`);
console.log(`unsupported: ${unsupported}`);
console.log(`errors: ${errored}`);
console.log("\nUNSUPPORTED buckets:");
for (const [reason, count] of [...unsupportedBuckets.entries()].sort(
  (a, b) => b[1] - a[1] || a[0].localeCompare(b[0]),
)) {
  console.log(`${count}\t${reason}`);
}

console.log(`\nOWNERSHIP (normalized buckets > ${OWNER_THRESHOLD}):`);
for (const [reason, count] of [...unsupportedBuckets.entries()]
  .filter(([, count]) => count > OWNER_THRESHOLD)
  .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))) {
  console.log(
    `${count}\t${RESIDUAL_OWNERS.get(reason) ?? "UNASSIGNED"}\t${reason}`,
  );
}

if (errorBuckets.size > 0) {
  console.log("\nERROR buckets:");
  for (const [reason, count] of [...errorBuckets.entries()].sort(
    (a, b) => b[1] - a[1] || a[0].localeCompare(b[0]),
  )) {
    console.log(`${count}\t${reason}`);
  }
}
