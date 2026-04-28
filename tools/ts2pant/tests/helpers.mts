import { execFileSync } from "node:child_process";
import { existsSync } from "node:fs";
import { resolve } from "node:path";
import { emitDocument } from "../src/emit.js";
import { createSourceFile, type SourceFile } from "../src/extract.js";
import { assertWasmTypeChecks } from "../src/pant-wasm.js";
import { buildPantDocument } from "../src/pipeline.js";
import { IntStrategy } from "../src/translate-types.js";
import type { PantDocument } from "../src/types.js";

export const PROJECT_ROOT = resolve(import.meta.dirname, "../../..");

let cachedPantBin: string | undefined;

/**
 * Resolve the `pant` binary path. **Pure** — never invokes a build.
 *
 * Two resolution paths:
 *   1. `PANT_BIN` env var (CI override, or any caller that already knows
 *      where the binary lives).
 *   2. `${PROJECT_ROOT}/_build/default/bin/main.exe` from a prior
 *      `npm run prebuild:pant` (or `dune build @install` in CI).
 *
 * If neither is satisfied, throw with an actionable message. We refuse
 * to lazily `dune build` here because Node's test runner spawns one
 * worker per test file (`--test-isolation=process`), so the per-process
 * cache below doesn't amortize across workers — every worker would race
 * for `_build/.lock`, and a SIGKILLed worker would poison the lock for
 * every subsequent run. The pre-commit hook that hung for 15+ hours
 * before this fix landed traced back to that exact pattern.
 */
export function getPantBin(): string {
  if (cachedPantBin !== undefined) return cachedPantBin;
  const fromEnv = process.env.PANT_BIN;
  if (fromEnv && existsSync(fromEnv)) {
    cachedPantBin = fromEnv;
    return fromEnv;
  }
  const binPath = resolve(PROJECT_ROOT, "_build/default/bin/main.exe");
  if (!existsSync(binPath)) {
    throw new Error(
      `pant binary not found at ${binPath}. Run \`just build-pant\` from ` +
        `the workspace root before invoking integration tests, or set ` +
        `PANT_BIN to the path of an already-built pant binary. ` +
        `\`just ts2pant-test-integration\` does both in one step.`,
    );
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

/**
 * Emit a PantDocument and verify that the resulting Pantagruel text
 * type-checks via the embedded wasm checker (parse + collect + check —
 * the same passes the `pant` CLI runs in its default mode). This is
 * the universal verification gate for any test that exercises the
 * emit pipeline: every place ts2pant produces Pantagruel should run
 * through this so we don't snapshot output that the OCaml typechecker
 * would reject.
 *
 * If the emitted text contains a `> UNSUPPORTED:` line, the document
 * is a deliberate-rejection signal and the wasm typecheck is skipped —
 * the snapshot still captures the rejection message verbatim. See
 * `tools/ts2pant/CLAUDE.md` § "Test layout".
 *
 * If the document carries a `bundleModules` map (added by the dep-
 * module pipeline), it is forwarded as the in-memory dep registry so
 * the consumer's `import` declarations resolve.
 *
 * Returns the emitted text so the caller can still snapshot or assert
 * on it. Throws (with a snippet of the input) if checking fails.
 */
export async function emitAndCheck(
  doc: PantDocument & { bundleModules?: Map<string, string> },
): Promise<string> {
  const output = emitDocument(doc);
  if (containsUnsupportedLine(output)) {
    return output;
  }
  await assertWasmTypeChecks(output, doc.bundleModules);
  return output;
}

/**
 * Detect a deliberate-rejection signal — an emitted line beginning with
 * `> UNSUPPORTED:`. Mirrors the marker `emit.ts` writes for
 * `kind: "unsupported"` PropResults.
 */
export function containsUnsupportedLine(output: string): boolean {
  return /^> UNSUPPORTED:/mu.test(output);
}
