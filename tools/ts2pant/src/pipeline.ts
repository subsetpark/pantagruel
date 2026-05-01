import type { SourceFile } from "ts-morph";
import { extractFunctionAnnotationsAndOverrides } from "./annotations.js";
import { type DepModuleName, loadBuiltinDepModule } from "./builtins.js";
import {
  extractModuleConsts,
  extractReferencedFunctions,
  extractReferencedTypes,
  getChecker,
} from "./extract.js";
import { loadAst, loadParser, rewriteAnnotation } from "./pant-wasm.js";
import { translateBody } from "./translate-body.js";
import { translateSignature } from "./translate-signature.js";
import {
  cellEmitSynth,
  fieldRuleName,
  type NumericStrategy,
  newSynthCell,
  toPantTermName,
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

  // Translate signature first to claim the function's param names.
  const { declaration: sigDecl, paramNameMap } = translateSignature(
    sourceFile,
    functionName,
    strategy,
    synthCell,
    overrides,
  );

  // Extract and translate types (type-derived param names adapt to registry).
  // Pass the synthesizer so nested Maps inside interface-field V register too.
  const extracted = extractReferencedTypes(sourceFile, functionName);
  const typeDecls = translateTypes(extracted, checker, strategy, synthCell);
  // After both sig and types have registered their Maps, emit the synth
  // decls (one domain + membership predicate + guarded value rule per
  // unique (K, V)). Splice before sigDecl so the sig's references resolve.
  const synthDecls = cellEmitSynth(synthCell);

  // Module-level `const NAME = <literal>` declarations map onto 0-arity
  // rules + body equations. Done after `translateSignature` so the
  // function's params claim names first; constant names pick up
  // collision suffixes via the registry. The TS->Pant rename is
  // threaded into `paramNameMap` so identifier references inside the
  // function body resolve to the kebab'd Pant name.
  const moduleConsts = extractModuleConsts(
    sourceFile,
    functionName,
    strategy,
    synthCell,
  );
  const constDecls = moduleConsts.map((c) => c.declaration);
  const constEquations = moduleConsts.map((c) => c.equation);
  for (const c of moduleConsts) {
    paramNameMap.set(c.tsName, c.pantName);
  }

  // Sibling / cross-file functions called from the consumer's body
  // get rule heads emitted (signature only — body stays opaque under
  // EUF). Without these, the call site's `App(Var("kebab-name"), …)`
  // would dangle. Same `paramNameMap` rename plumbing as module
  // constants so the call dispatcher resolves the un-kebab'd TS name
  // through to the kebab'd rule head.
  const refFns = extractReferencedFunctions(
    sourceFile,
    functionName,
    strategy,
    synthCell,
  );
  const fnDecls = refFns.map((f) => f.declaration);
  for (const f of refFns) {
    paramNameMap.set(f.tsName, f.pantName);
  }

  const declarations = [
    ...typeDecls,
    ...synthDecls,
    ...constDecls,
    ...fnDecls,
    sigDecl,
  ];

  // Module names are SHOUTING_SNAKE_CASE — same convention as the
  // ambient module (`expressions-calls.ts -> EXPRESSIONS_CALLS_AMBIENT`)
  // and the hand-written stdlib bundles (`JS_MATH`, `TS_PRELUDE`). Reuse
  // `toPantTermName`'s camelCase/punctuation-splitting and shift to
  // underscore-uppercase so `isUnsupportedUnknown -> IS_UNSUPPORTED_UNKNOWN`.
  const moduleName = toPantTermName(baseName).replace(/-/gu, "_").toUpperCase();
  let doc: PantDocument = {
    moduleName,
    imports: [],
    declarations,
    propositions: [...constEquations],
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
      paramNameMap,
    });
    doc = { ...doc, propositions: [...doc.propositions, ...bodyProps] };

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

  // Drain dep-module imports requested during build (template-literal
  // recognizer, future stdlib dispatchers). Each entry becomes one
  // `import M.` line and one bundleModules entry so the wasm bridge's
  // cross-module typecheck path can resolve qualified references.
  if (synthCell.imports.size > 0) {
    const newImports = [...synthCell.imports]
      .filter((name) => !doc.imports.some((imp) => imp.name === name))
      .map((name) => ({ name }));
    const bundle = new Map(doc.bundleModules ?? []);
    for (const name of synthCell.imports) {
      if (!bundle.has(name)) {
        bundle.set(name, loadBuiltinDepModule(name as DepModuleName));
      }
    }
    doc = {
      ...doc,
      imports: [...doc.imports, ...newImports],
      bundleModules: bundle,
    };
  }

  // Annotations go to checks (entailment goals) — skip for skeleton docs
  if (!noBody && doc.propositions.length > 0) {
    // Rewrite annotation names using the embedded Pantagruel parser.
    // paramNameMap maps TS params -> Pant names; function renames map the
    // translated function's TS spelling to its emitted Pant rule name; field
    // renames map unique property names (e.g. `balance`) to the synthesized
    // accessor rule symbol (e.g. `account-balance`) so user annotations can
    // keep referring to the TS property spelling.
    //
    // Collision policy: params and the function name are written first and
    // win against field-accessor renames. `annotationFieldRenames` produces
    // only unique (name, owner) pairs across declared interfaces AND
    // synthesized record shapes; an unqualified field name that maps to
    // more than one owner is omitted so the annotation text reaches the
    // parser unrewritten (and the user-level error surfaces at check time).
    const renames = new Map(paramNameMap);
    if (sigDecl.kind === "rule") {
      renames.set(baseName, sigDecl.name);
    }
    for (const [from, to] of annotationFieldRenames(extracted, synthCell)) {
      // Don't overwrite: a param or function rename with the same TS
      // spelling is ambiguous with the field accessor; leaving the
      // earlier entry in place keeps the user's param/fn reference
      // intact and forces a qualified form for the accessor.
      if (!renames.has(from)) {
        renames.set(from, to);
      }
    }
    const hasRenames = [...renames.entries()].some(([k, v]) => k !== v);
    let annotationTexts: string[];
    if (hasRenames) {
      await loadParser();
      annotationTexts = annotations.map((text) =>
        rewriteAnnotation(text, renames),
      );
    } else {
      annotationTexts = annotations;
    }

    const annotationProps = annotationTexts.map((text) => ({ text }));
    doc = { ...doc, checks: [...doc.checks, ...annotationProps] };
  }

  return doc;
}

function annotationFieldRenames(
  extracted: Awaited<ReturnType<typeof extractReferencedTypes>>,
  synthCell: ReturnType<typeof newSynthCell>,
): Map<string, string> {
  // Enumerate (ownerName, fieldName) pairs from both declared interfaces
  // and synthesized record-return shapes so bare field names emitted by
  // anonymous-record accessors can also be rewritten in annotations.
  // Counting across both sources lets us drop renames that would be
  // ambiguous — a field name shared by two owners has no unique target.
  const ownersByField = new Map<string, string[]>();
  const addOwner = (fieldName: string, ownerName: string): void => {
    const existing = ownersByField.get(fieldName);
    if (existing) {
      existing.push(ownerName);
    } else {
      ownersByField.set(fieldName, [ownerName]);
    }
  };
  for (const iface of extracted.interfaces) {
    for (const prop of iface.properties) {
      addOwner(prop.name, iface.name);
    }
  }
  for (const entry of synthCell.recordSynth.byShape.values()) {
    for (const field of entry.fields) {
      addOwner(field.name, entry.domain);
    }
  }

  const renames = new Map<string, string>();
  for (const [fieldName, owners] of ownersByField) {
    if (owners.length === 1) {
      renames.set(fieldName, fieldRuleName(owners[0]!, fieldName));
    }
  }
  return renames;
}
