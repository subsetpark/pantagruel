import { describe, it, expect } from "vitest";
import { resolve } from "path";
import { execSync } from "child_process";
import { createProgram, extractReferencedTypes } from "../src/extract.js";
import { translateTypes, IntStrategy } from "../src/translate-types.js";
import { translateSignature } from "../src/translate-signature.js";
import { translateBody } from "../src/translate-body.js";
import { extractFunctionAnnotations } from "../src/annotations.js";
import { emitDocument, runCheck } from "../src/emit.js";
import type { PantDocument } from "../src/types.js";
import { RawProp } from "../src/pant-expr.js";

const FIXTURES = resolve(__dirname, "fixtures");
const PROJECT_ROOT = resolve(__dirname, "../../..");

function solverAvailable(): boolean {
  try {
    execSync("which z3", { stdio: "ignore" });
    return true;
  } catch {
    return false;
  }
}

/**
 * Build a full PantDocument from a fixture file and function name.
 * Mirrors the pipeline in index.ts.
 */
function buildDocument(
  fixtureName: string,
  functionName: string,
  opts: { noBody?: boolean } = {},
): PantDocument {
  const fileName = resolve(FIXTURES, fixtureName);
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

  let doc: PantDocument = { moduleName, declarations, propositions: [] };

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

  // Annotations
  const annotations = extractFunctionAnnotations(program, fileName, functionName);
  const annotationProps = annotations.map((text) => RawProp(text));
  doc = { ...doc, propositions: [...doc.propositions, ...annotationProps] };

  return doc;
}

// --- Emission tests ---

describe("emitDocument", () => {
  it("emits max.ts as valid Pantagruel", () => {
    const doc = buildDocument("max.ts", "larger");
    const output = emitDocument(doc);

    expect(output).toContain("module Larger.");
    expect(output).toContain("larger a: Int, b: Int => Int.");
    expect(output).toContain("---");
    // Should contain the user annotation
    expect(output).toContain("all a: Int, b: Int | larger a b >= a and larger a b >= b.");
  });

  it("emits deposit.ts as valid Pantagruel", () => {
    const doc = buildDocument("deposit.ts", "deposit");
    const output = emitDocument(doc);

    expect(output).toContain("module Deposit.");
    expect(output).toContain("Account.");
    expect(output).toContain("balance a: Account => Int.");
    expect(output).toContain("~> Deposit @");
    expect(output).toContain("---");
    // Should contain the user annotation
    expect(output).toContain("all a: Account | balance' a >= 0.");
  });

  it("emits skeleton-only when noBody is set", () => {
    const doc = buildDocument("max.ts", "larger", { noBody: true });
    const output = emitDocument(doc);

    expect(output).toContain("module Larger.");
    expect(output).toContain("larger a: Int, b: Int => Int.");
    // Should still contain the annotation
    expect(output).toContain("all a: Int, b: Int | larger a b >= a and larger a b >= b.");
  });
});

// --- Annotation extraction tests ---

describe("extractFunctionAnnotations", () => {
  it("extracts @pant from max.ts", () => {
    const fileName = resolve(FIXTURES, "max.ts");
    const program = createProgram(fileName);
    const annotations = extractFunctionAnnotations(program, fileName, "larger");

    expect(annotations).toHaveLength(1);
    expect(annotations[0]).toBe("all a: Int, b: Int | larger a b >= a and larger a b >= b");
  });

  it("extracts @pant from deposit.ts", () => {
    const fileName = resolve(FIXTURES, "deposit.ts");
    const program = createProgram(fileName);
    const annotations = extractFunctionAnnotations(program, fileName, "deposit");

    expect(annotations).toHaveLength(1);
    expect(annotations[0]).toBe("all a: Account | balance' a >= 0");
  });
});

// --- Full pipeline tests ---

describe("full pipeline", () => {
  it("max.ts produces a checkable document", () => {
    const doc = buildDocument("max.ts", "larger");
    const output = emitDocument(doc);

    const lines = output.split("\n").filter(l => l.trim());
    expect(lines[0]).toBe("module Larger.");
    expect(lines).toContain("---");
    expect(doc.propositions.length).toBeGreaterThanOrEqual(1);
  });

  it("deposit.ts produces a checkable document", () => {
    const doc = buildDocument("deposit.ts", "deposit");
    const output = emitDocument(doc);

    const lines = output.split("\n").filter(l => l.trim());
    expect(lines[0]).toBe("module Deposit.");
    expect(lines).toContain("---");
    expect(doc.propositions.length).toBeGreaterThanOrEqual(1);
  });

  it("max.ts emitted .pant type-checks through pant", () => {
    const doc = buildDocument("max.ts", "larger");
    const output = emitDocument(doc);

    // Run through pant (no --check, just type-checking) — should exit 0
    execSync(
      `echo '${output.replace(/'/g, "'\\''")}' | dune exec pant --`,
      { encoding: "utf-8", cwd: PROJECT_ROOT },
    );
  });

  it("deposit.ts emitted .pant type-checks through pant", () => {
    const doc = buildDocument("deposit.ts", "deposit");
    const output = emitDocument(doc);

    execSync(
      `echo '${output.replace(/'/g, "'\\''")}' | dune exec pant --`,
      { encoding: "utf-8", cwd: PROJECT_ROOT },
    );
  });
});

// --- SMT check tests (require z3) ---

describe("pant --check", () => {
  const hasSolver = solverAvailable();

  it.skipIf(!hasSolver)("max.ts assertions pass", () => {
    const doc = buildDocument("max.ts", "larger");
    const output = emitDocument(doc);
    const result = runCheck(output, { projectRoot: PROJECT_ROOT });

    expect(result.passed).toBe(true);
    expect(result.checks.length).toBeGreaterThan(0);
    expect(result.checks.every(c => c.passed)).toBe(true);
  });

  it.skipIf(!hasSolver)("deposit.ts assertions are checkable", () => {
    const doc = buildDocument("deposit.ts", "deposit");
    const output = emitDocument(doc);
    const result = runCheck(output, { projectRoot: PROJECT_ROOT });

    expect(result.checks.length).toBeGreaterThan(0);
  });
});
