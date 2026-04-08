#!/usr/bin/env node

import type { CliOptions, NumericType, PantDocument } from "./types.js";

function parseArgs(argv: string[]): CliOptions {
  const args = argv.slice(2);
  let inputFile = "";
  let functionName = "";
  let check = false;
  let noBody = false;
  let numericType: NumericType = "Int";

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    switch (arg) {
      case "--check":
        check = true;
        break;
      case "--no-body":
        noBody = true;
        break;
      case "--numeric-type":
        i++;
        if (!args[i] || !["Int", "Real", "Nat0"].includes(args[i])) {
          console.error("--numeric-type must be one of: Int, Real, Nat0");
          process.exit(1);
        }
        numericType = args[i] as NumericType;
        break;
      default:
        if (arg.startsWith("-")) {
          console.error(`Unknown option: ${arg}`);
          process.exit(1);
        }
        if (!inputFile) {
          inputFile = arg;
        } else if (!functionName) {
          functionName = arg;
        } else {
          console.error(`Unexpected argument: ${arg}`);
          process.exit(1);
        }
    }
  }

  if (!inputFile || !functionName) {
    console.error("Usage: ts2pant <file.ts> <functionName> [--check] [--no-body] [--numeric-type Int|Real|Nat0]");
    process.exit(1);
  }

  return { inputFile, functionName, check, noBody, numericType };
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
        const params = decl.params.map((p) => `${p.name}: ${p.type}`).join(", ");
        lines.push(`~> ${decl.label} @ ${params}.`);
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
  const opts = parseArgs(process.argv);

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
