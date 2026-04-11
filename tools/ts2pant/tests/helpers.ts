import { createProgram, extractReferencedTypes } from "../src/extract.js";
import { translateTypes, IntStrategy } from "../src/translate-types.js";
import { translateSignature } from "../src/translate-signature.js";
import { translateBody } from "../src/translate-body.js";
import { extractFunctionAnnotations } from "../src/annotations.js";
import { emitDocument } from "../src/emit.js";
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
  const program = createProgram(fileName);
  const checker = program.getTypeChecker();
  const strategy = IntStrategy;

  // Extract types
  const extracted = extractReferencedTypes(program, fileName, functionName);
  const typeDecls = translateTypes(extracted, checker, strategy);

  // Translate signature
  const { declaration: sigDecl } = translateSignature(
    program, fileName, functionName, strategy,
  );
  const declarations = [...typeDecls, sigDecl];

  // Module name
  const moduleName = functionName.charAt(0).toUpperCase() + functionName.slice(1);

  let doc: PantDocument = { moduleName, declarations, propositions: [], checks: [] };

  // Body translation
  if (!opts.noBody) {
    const bodyProps = translateBody({
      program,
      fileName,
      functionName,
      strategy,
      declarations,
    });
    doc = { ...doc, propositions: [...doc.propositions, ...bodyProps] };
  }

  // Annotations go to checks (entailment goals)
  const annotations = extractFunctionAnnotations(program, fileName, functionName);
  const annotationProps = annotations.map((text) => ({ text }));
  doc = { ...doc, checks: [...doc.checks, ...annotationProps] };

  return doc;
}

export { emitDocument };
