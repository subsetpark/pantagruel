import { emitDocument } from "../src/emit.js";
import { createSourceFile } from "../src/extract.js";
import { buildPantDocument } from "../src/pipeline.js";
import { IntStrategy } from "../src/translate-types.js";
import type { PantDocument } from "../src/types.js";

/**
 * Build a full PantDocument from a file path and function name.
 * Uses the shared pipeline (same as CLI).
 */
export async function buildDocument(
  fileName: string,
  functionName: string,
  opts: { noBody?: boolean } = {},
): Promise<PantDocument> {
  const sourceFile = createSourceFile(fileName);
  return buildPantDocument({
    sourceFile,
    functionName,
    strategy: IntStrategy,
    noBody: opts.noBody,
  });
}

export { emitDocument };
