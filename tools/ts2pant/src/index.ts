#!/usr/bin/env node

import process from "node:process";
import { Command } from "commander";
import { extractFunctionAnnotations } from "./annotations.js";
import { emitDocument, runCheck } from "./emit.js";
import {
  createSourceFile,
  extractReferencedTypes,
  getChecker,
} from "./extract.js";
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

  const inputFile = program.args[0];
  const functionName = program.args[1];
  if (!inputFile || !functionName) {
    throw new Error("Expected <file> and <function>");
  }
  const opts = program.opts<{
    check: boolean;
    body: boolean;
    numericType: string;
  }>();

  const numericType = opts.numericType;
  if (!["Int", "Real", "Nat0"].includes(numericType)) {
    program.error(
      `--numeric-type must be one of: Int, Real, Nat0 (got "${numericType}")`,
    );
  }

  return {
    inputFile,
    functionName,
    check: opts.check,
    noBody: opts.body === false,
    numericType: numericType as NumericType,
  };
}

function getStrategy(numericType: NumericType): NumericStrategy {
  switch (numericType) {
    case "Real":
      return RealStrategy;
    case "Int":
    case "Nat0":
      return IntStrategy;
    default: {
      const _exhaustive: never = numericType;
      throw new Error(`Unhandled numeric type: ${_exhaustive}`);
    }
  }
}

/** Build a PantDocument from a TS source file and function name. */
export function extract(opts: CliOptions): PantDocument {
  const strategy = getStrategy(opts.numericType);
  const sourceFile = createSourceFile(opts.inputFile);
  const checker = getChecker(sourceFile);

  const extracted = extractReferencedTypes(sourceFile, opts.functionName);
  const typeDecls = translateTypes(extracted, checker, strategy);

  const { declaration: sigDecl } = translateSignature(
    sourceFile,
    opts.functionName,
    strategy,
  );

  const declarations = [...typeDecls, sigDecl];

  const moduleName =
    opts.functionName.charAt(0).toUpperCase() + opts.functionName.slice(1);

  return { moduleName, declarations, propositions: [], checks: [] };
}

/** Translate function body to propositions and append to doc. */
export function addBodyPropositions(
  doc: PantDocument,
  opts: CliOptions,
): PantDocument {
  const strategy = getStrategy(opts.numericType);
  const sourceFile = createSourceFile(opts.inputFile);

  const propositions = translateFunctionBody({
    sourceFile,
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
  const sourceFile = createSourceFile(opts.inputFile);
  const annotations = extractFunctionAnnotations(sourceFile, opts.functionName);

  if (opts.noBody || doc.propositions.length === 0) {
    return doc;
  }
  const annotationProps = annotations.map((text) => ({ text }));
  return { ...doc, checks: [...doc.checks, ...annotationProps] };
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
