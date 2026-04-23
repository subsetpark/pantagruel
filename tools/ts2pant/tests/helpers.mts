import { execFileSync, execSync } from "node:child_process";
import { existsSync } from "node:fs";
import { resolve } from "node:path";
import { emitDocument } from "../src/emit.js";
import { type SourceFile, createSourceFile } from "../src/extract.js";
import { buildPantDocument } from "../src/pipeline.js";
import { IntStrategy } from "../src/translate-types.js";
import type { PantDocument } from "../src/types.js";

export const PROJECT_ROOT = resolve(import.meta.dirname, "../../..");

let cachedPantBin: string | undefined;

/**
 * Resolve the `pant` binary path, running `dune build` once per process.
 * Invoking the binary directly avoids the ~75ms/call overhead of `dune exec`.
 */
export function getPantBin(): string {
  if (cachedPantBin !== undefined) return cachedPantBin;
  execSync("dune build bin/main.exe", { cwd: PROJECT_ROOT, stdio: "inherit" });
  const binPath = resolve(PROJECT_ROOT, "_build/default/bin/main.exe");
  if (!existsSync(binPath)) {
    throw new Error(`pant binary not found at ${binPath} after dune build`);
  }
  cachedPantBin = binPath;
  return binPath;
}

export function assertPantTypeChecks(output: string, timeoutMs = 30_000): void {
  execFileSync(getPantBin(), [], {
    input: output,
    encoding: "utf-8",
    timeout: timeoutMs,
    killSignal: "SIGKILL",
  });
}

/**
 * Build a full PantDocument from a file path and function name.
 * Uses the shared pipeline (same as CLI).
 *
 * Constructing the ts-morph type checker is expensive (~600ms), so callers
 * iterating many functions in the same file should use {@link buildDocumentFromSourceFile}
 * with a shared {@link SourceFile}.
 */
export async function buildDocument(
  fileName: string,
  functionName: string,
  opts: { noBody?: boolean } = {},
): Promise<PantDocument> {
  return buildDocumentFromSourceFile(
    createSourceFile(fileName),
    functionName,
    opts,
  );
}

export async function buildDocumentFromSourceFile(
  sourceFile: SourceFile,
  functionName: string,
  opts: { noBody?: boolean } = {},
): Promise<PantDocument> {
  return buildPantDocument({
    sourceFile,
    functionName,
    strategy: IntStrategy,
    noBody: opts.noBody,
  });
}

export { emitDocument };
