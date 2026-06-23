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

const buckets = new Map<string, number>();
let clean = 0;
let unsupported = 0;
let errored = 0;

for (const result of results) {
  if (result.error !== undefined) {
    errored += 1;
    const bucket = `ERROR: ${result.error.split("\n", 1)[0]}`;
    buckets.set(bucket, (buckets.get(bucket) ?? 0) + 1);
    continue;
  }
  if (result.unsupportedReasons.length === 0) {
    clean += 1;
    continue;
  }
  unsupported += 1;
  for (const reason of result.unsupportedReasons) {
    const bucket = normalizeReason(reason);
    buckets.set(bucket, (buckets.get(bucket) ?? 0) + 1);
  }
}

console.log("\n===== SUMMARY =====");
console.log(`functions: ${results.length}`);
console.log(`clean: ${clean}`);
console.log(`unsupported: ${unsupported}`);
console.log(`errors: ${errored}`);
console.log("\nUNSUPPORTED buckets:");
for (const [reason, count] of [...buckets.entries()].sort(
  (a, b) => b[1] - a[1] || a[0].localeCompare(b[0]),
)) {
  console.log(`${count}\t${reason}`);
}
