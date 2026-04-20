import { execFileSync } from "node:child_process";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { extractFunctionAnnotations } from "../src/annotations.js";
import { runCheck } from "../src/emit.js";
import { createSourceFile } from "../src/extract.js";
import {
  buildDocument as buildDocumentFromPath,
  emitDocument,
} from "./helpers.mjs";
import type { PantDocument } from "../src/types.js";

const FIXTURES = resolve(import.meta.dirname, "fixtures");
const PROJECT_ROOT = resolve(import.meta.dirname, "../../..");

function solverAvailable(): boolean {
  try {
    execFileSync("z3", ["-version"], { stdio: "ignore" });
    return true;
  } catch {
    return false;
  }
}

function assertPantTypeChecks(output: string): void {
  execFileSync("dune", ["exec", "pant", "--"], {
    cwd: PROJECT_ROOT,
    input: output,
    encoding: "utf-8",
  });
}

function buildDocument(
  fixtureName: string,
  functionName: string,
  opts: { noBody?: boolean } = {},
): Promise<PantDocument> {
  return buildDocumentFromPath(
    resolve(FIXTURES, fixtureName),
    functionName,
    opts,
  );
}

// --- Emission tests ---

describe("emitDocument", () => {
  it("emits max.ts as valid Pantagruel", async () => {
    const doc = await buildDocument("max.ts", "larger");
    const output = emitDocument(doc);

    assert.ok(output.includes("module Larger."));
    assert.ok(output.includes("larger a: Int, b: Int => Int."));
    assert.ok(output.includes("---"));
    // Annotation should be in the check block
    assert.ok(output.includes("check"));
    assert.ok(output.includes(
      "all a: Int, b: Int | larger a b >= a and larger a b >= b.",
    ));
  });

  it("emits deposit.ts as valid Pantagruel", async () => {
    const doc = await buildDocument("deposit.ts", "deposit");
    const output = emitDocument(doc);

    assert.ok(output.includes("module Deposit."));
    assert.ok(output.includes("Account."));
    assert.ok(output.includes("balance a: Account => Int."));
    assert.ok(output.includes("~> Deposit @"));
    assert.ok(output.includes("---"));
    // Annotation should be in the check block
    assert.ok(output.includes("check"));
    assert.ok(output.includes("balance' account > balance account."));
  });

  it("emits skeleton-only when noBody is set", async () => {
    const doc = await buildDocument("max.ts", "larger", { noBody: true });
    const output = emitDocument(doc);

    assert.ok(output.includes("module Larger."));
    assert.ok(output.includes("larger a: Int, b: Int => Int."));
    // Skeleton output should NOT contain checks (no body to entail from)
    assert.ok(!output.includes("check"));
  });
});

// --- Annotation extraction tests ---

describe("extractFunctionAnnotations", () => {
  it("extracts @pant from max.ts", () => {
    const fileName = resolve(FIXTURES, "max.ts");
    const sourceFile = createSourceFile(fileName);
    const annotations = extractFunctionAnnotations(sourceFile, "larger");

    assert.equal(annotations.length, 1);
    assert.equal(annotations[0],
      "all a: Int, b: Int | larger a b >= a and larger a b >= b",
    );
  });

  it("extracts @pant from deposit.ts", () => {
    const fileName = resolve(FIXTURES, "deposit.ts");
    const sourceFile = createSourceFile(fileName);
    const annotations = extractFunctionAnnotations(sourceFile, "deposit");

    assert.equal(annotations.length, 1);
    assert.equal(annotations[0], "balance' account > balance account");
  });
});

// --- Full pipeline tests ---

describe("full pipeline", () => {
  it("max.ts produces a checkable document", async () => {
    const doc = await buildDocument("max.ts", "larger");
    const output = emitDocument(doc);

    const lines = output.split("\n").filter((l) => l.trim());
    assert.equal(lines[0], "module Larger.");
    assert.ok(lines.includes("---"));
    assert.ok(doc.propositions.length >= 1);
    assert.ok(doc.checks.length >= 1);
  });

  it("deposit.ts produces a checkable document", async () => {
    const doc = await buildDocument("deposit.ts", "deposit");
    const output = emitDocument(doc);

    const lines = output.split("\n").filter((l) => l.trim());
    assert.equal(lines[0], "module Deposit.");
    assert.ok(lines.includes("---"));
    assert.ok(doc.checks.length >= 1);
  });

  it("max.ts emitted .pant type-checks through pant", async () => {
    const doc = await buildDocument("max.ts", "larger");
    const output = emitDocument(doc);

    // Run through pant (no --check, just type-checking) — should exit 0
    assertPantTypeChecks(output);
  });

  it("deposit.ts emitted .pant type-checks through pant", async () => {
    const doc = await buildDocument("deposit.ts", "deposit");
    const output = emitDocument(doc);

    assertPantTypeChecks(output);
  });

  it("apply-fee.ts emitted .pant type-checks through pant", async () => {
    const doc = await buildDocument("apply-fee.ts", "applyFee");
    const output = emitDocument(doc);

    assertPantTypeChecks(output);
  });
});

// --- Snapshot tests ---

describe("emission snapshots", () => {
  it("max.ts larger", async (t) => {
    const doc = await buildDocument("max.ts", "larger");
    t.assert.snapshot(emitDocument(doc));
  });

  it("max.ts larger (skeleton only)", async (t) => {
    const doc = await buildDocument("max.ts", "larger", { noBody: true });
    t.assert.snapshot(emitDocument(doc));
  });

  it("deposit.ts deposit", async (t) => {
    const doc = await buildDocument("deposit.ts", "deposit");
    t.assert.snapshot(emitDocument(doc));
  });

  it("assert-guard.ts deposit", async (t) => {
    const doc = await buildDocument("assert-guard.ts", "deposit");
    t.assert.snapshot(emitDocument(doc));
  });

  it("validate-helper.ts deposit", async (t) => {
    const doc = await buildDocument("validate-helper.ts", "deposit");
    t.assert.snapshot(emitDocument(doc));
  });
});

// --- SMT check tests (require z3) ---

describe("pant --check", () => {
  const hasSolver = solverAvailable();

  it("max.ts assertions pass", { skip: !hasSolver ? "z3 not available" : undefined }, async () => {
    const doc = await buildDocument("max.ts", "larger");
    const output = emitDocument(doc);
    const result = runCheck(output, { projectRoot: PROJECT_ROOT });

    assert.equal(result.passed, true);
    assert.ok(result.checks.length > 0);
    assert.ok(result.checks.every((c) => c.passed));
  });

  it("deposit.ts assertions are checkable", { skip: !hasSolver ? "z3 not available" : undefined }, async () => {
    const doc = await buildDocument("deposit.ts", "deposit");
    const output = emitDocument(doc);
    const result = runCheck(output, { projectRoot: PROJECT_ROOT });

    assert.equal(result.passed, true);
    assert.ok(result.checks.length > 0);
    assert.ok(result.checks.every((c) => c.passed));
  });

  it("apply-fee.ts conditional mutation is checkable", { skip: !hasSolver ? "z3 not available" : undefined }, async () => {
    const doc = await buildDocument("apply-fee.ts", "applyFee");
    const output = emitDocument(doc);
    const result = runCheck(output, { projectRoot: PROJECT_ROOT });

    assert.equal(result.passed, true);
    assert.ok(result.checks.length > 0);
    assert.ok(result.checks.every((c) => c.passed));
  });
});
