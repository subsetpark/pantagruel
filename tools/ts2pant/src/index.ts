#!/usr/bin/env node

import process from "node:process";
import { Command } from "commander";
import { extractFunctionAnnotations } from "./annotations.js";
import { emitDocument, runCheck } from "./emit.js";
import { createProgram, extractReferencedTypes } from "./extract.js";
import { translateBody as translateFunctionBody } from "./translate-body.js";
import { translateSignature } from "./translate-signature.js";
import {
  IntStrategy,
  type NumericStrategy,
  RealStrategy,
  translateTypes,
} from "./translate-types.js";
import type { CliOptions, NumericType, PantDocument } from "./types.js";

function parseArgs(): CliOptions {
  const program = new Command();
  program
    .name("ts2pant")
    .description(
      "Translate TypeScript functions into checkable Pantagruel specifications",
    )
    .argument("<file>", "TypeScript source file")
    .argument("<function>", "Function name to translate")
    .option("--check", "Write .pant and run pant --check", false)
    .option("--no-body", "Skip body translation (skeleton only)")
    .option("--numeric-type <type>", "Default numeric type mapping", "Int")
    .parse();

  const [inputFile, functionName] = program.args as [string, string];
  const opts = program.opts();

  const numericType = opts["numericType"] as string;
  if (!["Int", "Real", "Nat0"].includes(numericType)) {
    program.error(
      `--numeric-type must be one of: Int, Real, Nat0 (got "${numericType}")`,
    );
  }

  return {
    inputFile,
    functionName,
    check: opts["check"] as boolean,
    noBody: opts["body"] === false,
    numericType: numericType as NumericType,
  };
}

function getStrategy(numericType: NumericType): NumericStrategy {
  switch (numericType) {
    case "Real":
      return RealStrategy;
    default:
      return IntStrategy;
  }
}

/** Build a PantDocument from a TS source file and function name. */
export function extract(opts: CliOptions): PantDocument {
  const strategy = getStrategy(opts.numericType);
  const program = createProgram(opts.inputFile);
  const checker = program.getTypeChecker();

  // Extract referenced types and translate to declarations
  const extracted = extractReferencedTypes(
    program,
    opts.inputFile,
    opts.functionName,
  );
  const typeDecls = translateTypes(extracted, checker, strategy);

  // Translate function signature
  const { declaration: sigDecl } = translateSignature(
    program,
    opts.inputFile,
    opts.functionName,
    strategy,
  );

  const declarations = [...typeDecls, sigDecl];

  // Capitalize function name for module name
  const moduleName =
    opts.functionName.charAt(0).toUpperCase() + opts.functionName.slice(1);

  return { moduleName, declarations, propositions: [] };
}

/** Translate function body to propositions and append to doc. */
export function addBodyPropositions(
  doc: PantDocument,
  opts: CliOptions,
): PantDocument {
  const strategy = getStrategy(opts.numericType);
  const program = createProgram(opts.inputFile);

  const propositions = translateFunctionBody({
    program,
    fileName: opts.inputFile,
    functionName: opts.functionName,
    strategy,
    declarations: doc.declarations,
  });

  return { ...doc, propositions: [...doc.propositions, ...propositions] };
}

/** Extract @pant annotations and append as propositions. */
export function addAnnotations(
  doc: PantDocument,
  opts: CliOptions,
): PantDocument {
  const program = createProgram(opts.inputFile);
  const annotations = extractFunctionAnnotations(
    program,
    opts.inputFile,
    opts.functionName,
  );

  const annotationProps = annotations.map((text) => ({ text }));
  return { ...doc, propositions: [...doc.propositions, ...annotationProps] };
}

function main(): void {
  const opts = parseArgs();

  // Pipeline: extract -> translate body -> add annotations -> emit
  let doc = extract(opts);

  if (!opts.noBody) {
    doc = addBodyPropositions(doc, opts);
  }

  doc = addAnnotations(doc, opts);

  const output = emitDocument(doc);

  if (opts.check) {
    const result = runCheck(output);
    process.stdout.write(result.output);
    process.exit(result.passed ? 0 : 1);
  }

  process.stdout.write(output);
}

main();
