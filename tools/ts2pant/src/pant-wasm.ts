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
