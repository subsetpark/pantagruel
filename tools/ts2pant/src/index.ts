#!/usr/bin/env node

import { Command } from "commander";
import type { CliOptions, NumericType, PantDocument } from "./types.js";

function parseArgs(): CliOptions {
  const program = new Command();
  program
    .name("ts2pant")
    .description("Translate TypeScript functions into checkable Pantagruel specifications")
    .argument("<file>", "TypeScript source file")
    .argument("<function>", "Function name to translate")
    .option("--check", "Write .pant and run pant --check", false)
    .option("--no-body", "Skip body translation (skeleton only)")
    .option("--numeric-type <type>", "Default numeric type mapping", "Int")
    .parse();

  const [inputFile, functionName] = program.args;
  const opts = program.opts();

  const numericType = opts.numericType as string;
  if (!["Int", "Real", "Nat0"].includes(numericType)) {
    program.error(`--numeric-type must be one of: Int, Real, Nat0 (got "${numericType}")`);
  }

  return {
    inputFile,
    functionName,
    check: opts.check as boolean,
    noBody: opts.body === false,
    numericType: numericType as NumericType,
  };
}

/** Stub: extract types and function from the TS source. */
function extract(_opts: CliOptions): PantDocument {
  // TODO (Patch 7-8): Use TS compiler API to extract types and function signature
  return { moduleName: "Generated", declarations: [], propositions: [] };
}

/** Stub: translate function body to propositions. */
function translateBody(doc: PantDocument, _opts: CliOptions): PantDocument {
  // TODO (Patch 9): Translate function body to Pantagruel propositions
  return doc;
}

/** Stub: emit Pantagruel source text. */
function emit(doc: PantDocument): string {
  const lines: string[] = [];
  lines.push(`module ${doc.moduleName}.`);
  lines.push("");

  for (const decl of doc.declarations) {
    switch (decl.kind) {
      case "domain":
        lines.push(`${decl.name}.`);
        break;
      case "alias":
        lines.push(`${decl.name} = ${decl.type}.`);
        break;
      case "rule": {
        const params = decl.params.map((p) => `${p.name}: ${p.type}`).join(", ");
        lines.push(`${decl.name} ${params} => ${decl.returnType}.`);
        break;
      }
      case "action": {
        if (decl.params.length === 0) {
          lines.push(`~> ${decl.label}.`);
        } else {
          const params = decl.params.map((p) => `${p.name}: ${p.type}`).join(", ");
          lines.push(`~> ${decl.label} @ ${params}.`);
        }
        break;
      }
    }
  }

  lines.push("");
  lines.push("---");
  lines.push("");

  if (doc.propositions.length === 0) {
    lines.push("true.");
  } else {
    for (const prop of doc.propositions) {
      lines.push(`${prop.text}.`);
    }
  }

  lines.push("");
  return lines.join("\n");
}

function main(): void {
  const opts = parseArgs();

  // Pipeline: extract -> translate body -> emit
  let doc = extract(opts);

  if (!opts.noBody) {
    doc = translateBody(doc, opts);
  }

  const output = emit(doc);

  if (opts.check) {
    // TODO (Patch 11): Write to temp file and invoke pant --check
    console.error("--check not yet implemented");
    process.exit(1);
  }

  process.stdout.write(output);
}

main();
