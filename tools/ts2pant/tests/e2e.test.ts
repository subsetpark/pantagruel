import { describe, it, expect } from "vitest";
import { resolve } from "path";
import { execSync } from "child_process";
import { createSourceFile } from "../src/extract.js";
import { extractFunctionAnnotations } from "../src/annotations.js";
import { runCheck } from "../src/emit.js";
import { buildDocument as buildDocumentFromPath, emitDocument } from "./helpers.js";

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

function buildDocument(
  fixtureName: string,
  functionName: string,
  opts: { noBody?: boolean } = {},
) {
  return buildDocumentFromPath(resolve(FIXTURES, fixtureName), functionName, opts);
}

// --- Emission tests ---

describe("emitDocument", () => {
  it("emits max.ts as valid Pantagruel", () => {
    const doc = buildDocument("max.ts", "larger");
    const output = emitDocument(doc);

    expect(output).toContain("module Larger.");
    expect(output).toContain("larger a: Int, b: Int => Int.");
    expect(output).toContain("---");
    // Annotation should be in the check block
    expect(output).toContain("check");
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
    // Annotation should be in the check block
    expect(output).toContain("check");
    expect(output).toContain("balance' account > balance account.");
  });

  it("emits skeleton-only when noBody is set", () => {
    const doc = buildDocument("max.ts", "larger", { noBody: true });
    const output = emitDocument(doc);

    expect(output).toContain("module Larger.");
    expect(output).toContain("larger a: Int, b: Int => Int.");
    // Should still contain the annotation in check block
    expect(output).toContain("check");
    expect(output).toContain("all a: Int, b: Int | larger a b >= a and larger a b >= b.");
  });
});

// --- Annotation extraction tests ---

describe("extractFunctionAnnotations", () => {
  it("extracts @pant from max.ts", () => {
    const fileName = resolve(FIXTURES, "max.ts");
    const sourceFile = createSourceFile(fileName);
    const annotations = extractFunctionAnnotations(sourceFile, "larger");

    expect(annotations).toHaveLength(1);
    expect(annotations[0]).toBe("all a: Int, b: Int | larger a b >= a and larger a b >= b");
  });

  it("extracts @pant from deposit.ts", () => {
    const fileName = resolve(FIXTURES, "deposit.ts");
    const sourceFile = createSourceFile(fileName);
    const annotations = extractFunctionAnnotations(sourceFile, "deposit");

    expect(annotations).toHaveLength(1);
    expect(annotations[0]).toBe("balance' account > balance account");
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
    expect(doc.checks.length).toBeGreaterThanOrEqual(1);
  });

  it("deposit.ts produces a checkable document", () => {
    const doc = buildDocument("deposit.ts", "deposit");
    const output = emitDocument(doc);

    const lines = output.split("\n").filter(l => l.trim());
    expect(lines[0]).toBe("module Deposit.");
    expect(lines).toContain("---");
    expect(doc.checks.length).toBeGreaterThanOrEqual(1);
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

// --- Snapshot tests ---

describe("emission snapshots", () => {
  it("max.ts larger", () => {
    const doc = buildDocument("max.ts", "larger");
    expect(emitDocument(doc)).toMatchSnapshot();
  });

  it("max.ts larger (skeleton only)", () => {
    const doc = buildDocument("max.ts", "larger", { noBody: true });
    expect(emitDocument(doc)).toMatchSnapshot();
  });

  it("deposit.ts deposit", () => {
    const doc = buildDocument("deposit.ts", "deposit");
    expect(emitDocument(doc)).toMatchSnapshot();
  });

  it("assert-guard.ts deposit", () => {
    const doc = buildDocument("assert-guard.ts", "deposit");
    expect(emitDocument(doc)).toMatchSnapshot();
  });

  it("validate-helper.ts deposit", () => {
    const doc = buildDocument("validate-helper.ts", "deposit");
    expect(emitDocument(doc)).toMatchSnapshot();
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
