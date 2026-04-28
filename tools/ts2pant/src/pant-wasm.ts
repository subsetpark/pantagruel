/**
 * TypeScript wrapper for the Pantagruel wasm module (wasm_of_ocaml).
 *
 * Provides two exports from the same wasm binary:
 * - pantParser: parse -> rename -> pretty-print for annotation rewriting
 * - pantAst: opaque AST constructors + rendering for document emission
 */

import { createRequire } from "node:module";
import path from "node:path";
import { fileURLToPath } from "node:url";
import type { PantAstModule } from "./pant-ast.js";

interface PantParserModule {
  parseAndRename: (text: string, renames: [string, string][]) => string | null;
  prettyPrint: (text: string) => string | null;
  checkDocument: (text: string) => string | null;
  checkDocumentWithDeps: (
    consumer: string,
    deps: [string, string][],
  ) => string | null;
}

let wasmLoadPromise: Promise<void> | null = null;
let pantParser: PantParserModule | null = null;
let pantAst: PantAstModule | null = null;

/**
 * Load the wasm binary and extract both pantParser and pantAst exports.
 * Safe to call multiple times -- only loads once.
 *
 * The wasm loader JS file is patched during build (build:wasm) to use
 * __filename instead of require.main.filename, so it works in both
 * CJS and ESM contexts.
 */
async function ensureWasmLoaded(): Promise<void> {
  if (pantAst) {
    return;
  }
  if (wasmLoadPromise) {
    return wasmLoadPromise;
  }

  wasmLoadPromise = (async () => {
    const __filename = fileURLToPath(import.meta.url);
    const __dirname = path.dirname(__filename);
    const require = createRequire(import.meta.url);
    const loaderPath = path.join(__dirname, "wasm", "pant_wasm.bc.wasm.js");

    require(loaderPath);

    // Poll for async wasm initialization (the loader attaches globals asynchronously)
    const g = globalThis as Record<string, unknown>;
    const deadline = Date.now() + 10_000;
    let delay = 1;
    while (!g["pantParser"] || !g["pantAst"]) {
      if (Date.now() > deadline) {
        throw new Error("Timed out waiting for wasm module initialization");
      }
      await new Promise((resolve) => setTimeout(resolve, delay));
      delay = Math.min(delay * 2, 100);
    }

    pantParser = g["pantParser"] as PantParserModule;
    pantAst = g["pantAst"] as PantAstModule;
  })();

  return wasmLoadPromise;
}

/** Load and return the parser module. */
export async function loadParser(): Promise<PantParserModule> {
  await ensureWasmLoaded();
  return pantParser!;
}

/** Load and return the AST constructor module. */
export async function loadAst(): Promise<PantAstModule> {
  await ensureWasmLoaded();
  return pantAst!;
}

/** Get the AST module (must call loadAst first). */
export function getAst(): PantAstModule {
  if (!pantAst) {
    throw new Error("AST module not loaded. Call loadAst() first.");
  }
  return pantAst;
}

/**
 * Rewrite variable names in a Pantagruel proposition string.
 * Falls back to the original text if parsing fails.
 */
export function rewriteAnnotation(
  text: string,
  renames: Map<string, string>,
): string {
  if (!pantParser) {
    throw new Error(
      "Parser not loaded. Call loadParser() before rewriteAnnotation().",
    );
  }

  if (renames.size === 0) {
    return text;
  }

  const renameArray: [string, string][] = [...renames.entries()];
  const result = pantParser.parseAndRename(text, renameArray);
  if (result === null) {
    return text;
  }
  return result;
}

/**
 * Type-check a full Pantagruel document string via the embedded wasm
 * checker (parse + collect + check, same passes the `pant` CLI runs in
 * its default mode). Returns `null` on success or an error message on
 * failure. For documents with imports, use {@link checkPantBundle} —
 * this entry point routes through the legacy single-document path that
 * rejects imports.
 */
export async function checkPantDocument(text: string): Promise<string | null> {
  const parser = await loadParser();
  return parser.checkDocument(text);
}

/**
 * Type-check a consumer Pantagruel document together with an in-memory
 * bundle of dep modules. Each entry of `deps` is `(module-name, text)`
 * — the consumer's `import NAME.` declarations resolve against the
 * module-name keys. Returns `null` on success or an error message on
 * failure. Mirrors the `pant` CLI's import semantics (parse + collect +
 * check across the full registry).
 *
 * With an empty `deps`, behaves as if the consumer were checked alone —
 * any unresolved import in the consumer surfaces as a missing-module
 * error.
 */
export async function checkPantBundle(
  consumer: string,
  deps: Map<string, string> = new Map(),
): Promise<string | null> {
  const parser = await loadParser();
  const depsArray: [string, string][] = [...deps.entries()];
  return parser.checkDocumentWithDeps(consumer, depsArray);
}

/**
 * Assert that a Pantagruel document string type-checks via the wasm
 * checker. Throws with the formatted error message on failure,
 * including a bounded preview of the input for diagnostics.
 *
 * Pass `deps` to resolve cross-module imports against an in-memory
 * bundle (see {@link checkPantBundle}); the default empty map mirrors
 * the legacy single-document behaviour.
 */
export async function assertWasmTypeChecks(
  text: string,
  deps: Map<string, string> = new Map(),
): Promise<void> {
  const error = await checkPantBundle(text, deps);
  if (error !== null) {
    const maxPreview = 4_000;
    const preview =
      text.length <= maxPreview
        ? text
        : `${text.slice(0, maxPreview)}\n...<truncated ${text.length - maxPreview} chars>`;
    throw new Error(
      `pant typecheck failed: ${error}\n--- input preview (total length: ${text.length}) ---\n${preview}`,
    );
  }
}
