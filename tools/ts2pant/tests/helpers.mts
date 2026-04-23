import { emitDocument } from "../src/emit.js";
import { type SourceFile, createSourceFile } from "../src/extract.js";
import { buildPantDocument } from "../src/pipeline.js";
import { IntStrategy } from "../src/translate-types.js";
import type { PantDocument } from "../src/types.js";

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
