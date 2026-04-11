import type { SourceFile } from "ts-morph";
import { extractFunctionAnnotations } from "./annotations.js";
import { extractReferencedTypes, getChecker } from "./extract.js";
import { translateBody } from "./translate-body.js";
import { translateSignature } from "./translate-signature.js";
import { type NumericStrategy, translateTypes } from "./translate-types.js";
import type { PantDocument } from "./types.js";

export interface PipelineOptions {
  sourceFile: SourceFile;
  functionName: string;
  strategy: NumericStrategy;
  noBody?: boolean;
}

/**
 * Build a PantDocument from a parsed SourceFile and function name.
 * Shared by the CLI entry point and the test helpers.
 */
export function buildPantDocument(opts: PipelineOptions): PantDocument {
  const { sourceFile, functionName, strategy, noBody } = opts;
  const checker = getChecker(sourceFile);

  // Strip class qualifier for module name
  const baseName = functionName.includes(".")
    ? functionName.split(".", 2)[1]!
    : functionName;

  // Extract and translate types
  const extracted = extractReferencedTypes(sourceFile, functionName);
  const typeDecls = translateTypes(extracted, checker, strategy);

  // Translate signature
  const { declaration: sigDecl } = translateSignature(
    sourceFile,
    functionName,
    strategy,
  );
  const declarations = [...typeDecls, sigDecl];

  const moduleName = baseName.charAt(0).toUpperCase() + baseName.slice(1);
  let doc: PantDocument = {
    moduleName,
    declarations,
    propositions: [],
    checks: [],
  };

  // Body translation
  if (!noBody) {
    const bodyProps = translateBody({
      sourceFile,
      functionName,
      strategy,
      declarations,
    });
    doc = { ...doc, propositions: [...doc.propositions, ...bodyProps] };
  }

  // Annotations go to checks (entailment goals) — skip for skeleton docs
  if (!noBody && doc.propositions.length > 0) {
    const annotations = extractFunctionAnnotations(sourceFile, functionName);
    const annotationProps = annotations.map((text) => ({ text }));
    doc = { ...doc, checks: [...doc.checks, ...annotationProps] };
  }

  return doc;
}
