import { extractFunctionAnnotations } from "../src/annotations.js";
import { emitDocument } from "../src/emit.js";
import {
  createSourceFile,
  extractReferencedTypes,
  getChecker,
} from "../src/extract.js";
import { translateBody } from "../src/translate-body.js";
import { translateSignature } from "../src/translate-signature.js";
import { IntStrategy, translateTypes } from "../src/translate-types.js";
import type { PantDocument } from "../src/types.js";

/**
 * Build a full PantDocument from a file path and function name.
 * Runs the complete pipeline: extract → translateTypes → translateSignature
 * → translateBody → extractAnnotations.
 */
export function buildDocument(
  fileName: string,
  functionName: string,
  opts: { noBody?: boolean } = {},
): PantDocument {
  const sourceFile = createSourceFile(fileName);
  const checker = getChecker(sourceFile);
  const strategy = IntStrategy;

  // Extract types
  const extracted = extractReferencedTypes(sourceFile, functionName);
  const typeDecls = translateTypes(extracted, checker, strategy);

  // Translate signature
  const { declaration: sigDecl } = translateSignature(
    sourceFile,
    functionName,
    strategy,
  );
  const declarations = [...typeDecls, sigDecl];

  // Module name (strip class qualifier if present)
  const baseName = functionName.includes(".")
    ? functionName.split(".", 2)[1]!
    : functionName;
  const moduleName = baseName.charAt(0).toUpperCase() + baseName.slice(1);

  let doc: PantDocument = {
    moduleName,
    declarations,
    propositions: [],
    checks: [],
  };

  // Body translation
  if (!opts.noBody) {
    const bodyProps = translateBody({
      sourceFile,
      functionName,
      strategy,
      declarations,
    });
    doc = { ...doc, propositions: [...doc.propositions, ...bodyProps] };
  }

  // Annotations go to checks (entailment goals) — skip for skeleton docs
  if (!opts.noBody && doc.propositions.length > 0) {
    const annotations = extractFunctionAnnotations(sourceFile, functionName);
    const annotationProps = annotations.map((text) => ({ text }));
    doc = { ...doc, checks: [...doc.checks, ...annotationProps] };
  }

  return doc;
}

export { emitDocument };
