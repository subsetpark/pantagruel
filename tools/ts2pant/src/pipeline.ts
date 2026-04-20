import type { SourceFile } from "ts-morph";
import { extractFunctionAnnotationsAndOverrides } from "./annotations.js";
import { extractReferencedTypes, getChecker } from "./extract.js";
import { NameRegistry } from "./name-registry.js";
import { loadAst, loadParser, rewriteAnnotation } from "./pant-wasm.js";
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
export async function buildPantDocument(
  opts: PipelineOptions,
): Promise<PantDocument> {
  const { sourceFile, functionName, strategy, noBody } = opts;

  // Ensure wasm AST module is loaded before any translation
  await loadAst();

  const checker = getChecker(sourceFile);

  // Strip class qualifier for module name
  const baseName = functionName.includes(".")
    ? functionName.split(".", 2)[1]!
    : functionName;

  // Document-wide name registry ensures unique variable names across
  // type-derived rules and the main function's parameters.
  // Register the function's own param names first (they keep natural names);
  // type-derived accessor rules adapt with suffixes if there's a collision.
  const registry = new NameRegistry();

  // Extract @pant propositions and @pant-type overrides in one JSDoc pass.
  // Overrides influence parameter type mapping during signature translation;
  // propositions become entailment checks once the body is translated.
  const { propositionTexts: annotations, typeOverrides: overrides } =
    extractFunctionAnnotationsAndOverrides(sourceFile, functionName);

  // Translate signature first to claim the function's param names
  const { declaration: sigDecl, paramNameMap } = translateSignature(
    sourceFile,
    functionName,
    strategy,
    registry,
    overrides,
  );

  // Extract and translate types (type-derived param names adapt to registry)
  const extracted = extractReferencedTypes(sourceFile, functionName);
  const typeDecls = translateTypes(extracted, checker, strategy, registry);
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
    // Rewrite annotation variable names using the embedded Pantagruel parser.
    // paramNameMap maps TS names → pant names; annotations use TS names.
    const hasRenames = [...paramNameMap.entries()].some(([k, v]) => k !== v);
    let annotationTexts: string[];
    if (hasRenames) {
      await loadParser();
      annotationTexts = annotations.map((text) =>
        rewriteAnnotation(text, paramNameMap),
      );
    } else {
      annotationTexts = annotations;
    }

    const annotationProps = annotationTexts.map((text) => ({ text }));
    doc = { ...doc, checks: [...doc.checks, ...annotationProps] };
  }

  return doc;
}
