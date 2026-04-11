/**
 * TypeScript wrapper for the Pantagruel parser compiled to WebAssembly.
 *
 * The wasm module provides parse → rename → pretty-print functionality
 * for rewriting @pant annotation variable names.
 */

import { createRequire } from "node:module";
import path from "node:path";
import { fileURLToPath } from "node:url";

interface PantParserModule {
  parseAndRename: (text: string, renames: [string, string][]) => string | null;
  prettyPrint: (text: string) => string | null;
}

let pantParser: PantParserModule | null = null;
let loadPromise: Promise<PantParserModule> | null = null;

/**
 * Load and initialize the wasm Pantagruel parser.
 * Safe to call multiple times — only loads once.
 */
export async function loadParser(): Promise<PantParserModule> {
  if (pantParser) {
    return pantParser;
  }
  if (loadPromise) {
    return loadPromise;
  }

  loadPromise = (async () => {
    // The wasm_of_ocaml loader uses CJS require() internally, which is
    // unavailable in ESM-only environments like vitest.
    if (typeof globalThis.process?.env?.["VITEST"] === "string") {
      throw new Error("Wasm parser unavailable in vitest environment");
    }

    // The wasm_of_ocaml loader is CJS and resolves .wasm assets relative
    // to require.main.filename. Use createRequire to load it from ESM.
    const __filename = fileURLToPath(import.meta.url);
    const __dirname = path.dirname(__filename);
    const require = createRequire(import.meta.url);

    // Temporarily set require.main.filename so the loader can find its
    // .wasm assets directory (pant_wasm.bc.wasm.assets/) next to the .js file.
    const wasmDir = path.join(__dirname, "wasm");
    const origMain = require.main;
    if (require.main) {
      require.main.filename = path.join(wasmDir, "pant_wasm.bc.wasm.js");
    }

    try {
      require(path.join(wasmDir, "pant_wasm.bc.wasm.js"));
    } finally {
      if (origMain && require.main) {
        require.main.filename = origMain.filename;
      }
    }

    // Wait for async wasm initialization
    await new Promise((resolve) => setTimeout(resolve, 200));

    const g = globalThis as Record<string, unknown>;
    const parser = g["pantParser"] as PantParserModule;
    if (!parser) {
      throw new Error("Failed to load Pantagruel wasm parser");
    }
    pantParser = parser;
    return parser;
  })();

  return loadPromise;
}

/**
 * Rewrite variable names in a Pantagruel proposition string.
 *
 * Parses the proposition using the embedded Pantagruel parser,
 * applies the rename map, and pretty-prints back to text.
 * Falls back to the original text if parsing fails.
 *
 * @param text - A Pantagruel proposition (e.g., "f a b >= a")
 * @param renames - Map from old variable names to new names
 * @returns The rewritten proposition text
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
    // Parse failed — return original text as fallback
    return text;
  }
  return result;
}
