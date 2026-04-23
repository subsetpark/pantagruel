import type { SourceFile } from "ts-morph";
import { extractFunctionAnnotationsAndOverrides } from "./annotations.js";
import { extractReferencedTypes, getChecker } from "./extract.js";
import { loadAst, loadParser, rewriteAnnotation } from "./pant-wasm.js";
import { translateBody } from "./translate-body.js";
import {
  classifyFunction,
  detectOptionalParamDefault,
  findFunction,
  translateSignature,
} from "./translate-signature.js";
import {
  cellEmitSynth,
  type NumericStrategy,
  newSynthCell,
  translateTypes,
} from "./translate-types.js";
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
  // type-derived rules and the main function's parameters. Held inside a
  // `SynthCell` together with the Map synthesizer so deep body-level call
  // sites can register on demand without threading state through every
  // BodyResult. Register the function's own param names first (they keep
  // natural names); type-derived accessor rules adapt with suffixes if
  // there's a collision.
  const synthCell = newSynthCell();

  // Extract @pant propositions and @pant-type overrides in one JSDoc pass.
  // Overrides influence parameter type mapping during signature translation;
  // propositions become entailment checks once the body is translated.
  const { propositionTexts: annotations, typeOverrides: overrides } =
    extractFunctionAnnotationsAndOverrides(sourceFile, functionName);

  // Detect the optional-param-with-??-default idiom up front. When it
  // matches, the function translates to two arity overloads (with-param and
  // without-param) under Pantagruel's positional-coherence rules rather than
  // one signature carrying a `T | Nothing` union that has no useful
  // inhabitants.
  //
  // Gated to pure functions: nullishRewrite is only threaded through
  // translatePureBody, so a mutating body like `obj.timeout = extra ?? 10`
  // would emit two action heads while both body passes still hit the
  // unsupported `??` operator. Fall back to the single-head path there.
  const { node: fnNode } = findFunction(sourceFile, functionName);
  const optSplit =
    classifyFunction(fnNode, checker) === "pure"
      ? detectOptionalParamDefault(fnNode)
      : null;

  // Translate signature first to claim the function's param names. When
  // splitting, the "with-param" head's signature strips `| undefined` from
  // the optional param's declared type; the paramNameMap from this head is
  // the one we thread to annotation-rewriting (the full-arity head is the
  // referent for any @pant mention of the optional param).
  const { declaration: sigDecl, paramNameMap } = translateSignature(
    sourceFile,
    functionName,
    strategy,
    synthCell,
    overrides,
    optSplit ? { unwrapOptionalParam: optSplit.paramName } : undefined,
  );

  // Second signature (reduced arity) when splitting: same name, optional
  // param dropped. Emitted alongside the full-arity head. Reuses the first
  // head's param-name map so shared positions name their params identically
  // — Pantagruel's positional coherence requires this, and without the
  // reuse the synthCell's allocator would suffix the duplicate names.
  let reducedSigDecl: typeof sigDecl | undefined;
  if (optSplit) {
    const { declaration } = translateSignature(
      sourceFile,
      functionName,
      strategy,
      synthCell,
      overrides,
      { excludeParam: optSplit.paramName, reuseParamNames: paramNameMap },
    );
    reducedSigDecl = declaration;
  }

  // Extract and translate types (type-derived param names adapt to registry).
  // Pass the synthesizer so nested Maps inside interface-field V register too.
  const extracted = extractReferencedTypes(sourceFile, functionName);
  const typeDecls = translateTypes(extracted, checker, strategy, synthCell);
  // After both sig and types have registered their Maps, emit the synth
  // decls (one domain + membership predicate + guarded value rule per
  // unique (K, V)). Splice before sigDecl so the sig's references resolve.
  const synthDecls = cellEmitSynth(synthCell);
  const declarations = [
    ...typeDecls,
    ...synthDecls,
    sigDecl,
    ...(reducedSigDecl ? [reducedSigDecl] : []),
  ];

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
      synthCell,
      ...(optSplit
        ? {
            nullishRewrite: {
              paramName: optSplit.paramName,
              mode: "take-lhs",
            },
          }
        : {}),
    });
    doc = { ...doc, propositions: [...doc.propositions, ...bodyProps] };

    // Second body translation for the reduced-arity head: substitute the
    // default expression for the nullish-coalesce (take-rhs) and drop the
    // optional param from scope.
    if (optSplit) {
      const reducedProps = translateBody({
        sourceFile,
        functionName,
        strategy,
        declarations,
        synthCell,
        nullishRewrite: {
          paramName: optSplit.paramName,
          mode: "take-rhs",
        },
        excludeParam: optSplit.paramName,
      });
      doc = { ...doc, propositions: [...doc.propositions, ...reducedProps] };
    }

    // Drain any Map (K, V) pairs registered on demand during body translation
    // (e.g., `build().get(k)!` where `build`'s return type wasn't surfaced
    // through the signature or referenced types). cellEmitSynth is
    // incremental, so this returns only entries new since the pre-body emit.
    const extraSynthDecls = cellEmitSynth(synthCell);
    if (extraSynthDecls.length > 0) {
      doc = {
        ...doc,
        declarations: [...doc.declarations, ...extraSynthDecls],
      };
    }
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
