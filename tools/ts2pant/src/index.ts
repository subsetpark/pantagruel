#!/usr/bin/env node

import process from "node:process";
import { Command } from "commander";
import { emitDocument, runCheck } from "./emit.js";
import { createSourceFile } from "./extract.js";
import { buildPantDocument } from "./pipeline.js";
import {
  IntStrategy,
  type NumericStrategy,
  RealStrategy,
} from "./translate-types.js";
import type { CliOptions, NumericType } from "./types.js";

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

function main(): void {
  const opts = parseArgs();
  if (opts.check && opts.noBody) {
    process.stderr.write("Error: --check cannot be combined with --no-body\n");
    process.exit(1);
  }
  const strategy = getStrategy(opts.numericType);
  const sourceFile = createSourceFile(opts.inputFile);

  const doc = buildPantDocument({
    sourceFile,
    functionName: opts.functionName,
    strategy,
    noBody: opts.noBody,
  });

  const output = emitDocument(doc);

  if (opts.check) {
    const result = runCheck(output);
    process.stdout.write(result.output);
    process.exit(result.passed ? 0 : 1);
  }

  process.stdout.write(output);
}

main();
