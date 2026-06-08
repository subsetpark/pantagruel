// @archlint.module test
// @archlint.domain ts2pant.ir1-ssa-ripout

import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { describe, it } from "node:test";

import { createSourceFileFromSource } from "../src/extract.js";
import { translateBody } from "../src/translate-body.js";
import { IntStrategy } from "../src/translate-types.js";
import type { PropResult } from "../src/types.js";
import { buildDocument, emitAndCheck } from "./helpers.mts";

const CONSTRUCTS_DIR = resolve(import.meta.dirname, "fixtures/constructs");
const SRC_DIR = resolve(import.meta.dirname, "../src");

function sourceFilesUnder(dir: string): string[] {
  const files: string[] = [];
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    const path = resolve(dir, entry.name);
    if (entry.isDirectory()) {
      files.push(...sourceFilesUnder(path));
    } else if (entry.isFile() && entry.name.endsWith(".ts")) {
      files.push(path);
    }
  }
  return files;
}

async function emitFixture(fileName: string, functionName: string): Promise<string> {
  const doc = await buildDocument(
    resolve(CONSTRUCTS_DIR, fileName),
    functionName,
  );
  return emitAndCheck(doc);
}

function translateMutatingSource(
  source: string,
  functionName: string,
): readonly PropResult[] {
  return translateBody({
    sourceFile: createSourceFileFromSource(source),
    functionName,
    strategy: IntStrategy,
  });
}

function extractTranslateMutatingBodySource(sourceText: string): string {
  const start = sourceText.indexOf("function translateMutatingBody(");
  const end = sourceText.indexOf(
    "\nfunction buildSupportedSsaMutatingBody(",
    start,
  );
  assert.notEqual(start, -1, "expected translateMutatingBody in translate-body.ts");
  assert.notEqual(
    end,
    -1,
    "expected buildSupportedSsaMutatingBody to delimit translateMutatingBody",
  );
  return sourceText.slice(start, end);
}

function exportedSymbolNames(sourceText: string): Set<string> {
  const names = new Set<string>();
  const patterns = [
    /export\s+function\s+([A-Za-z0-9_]+)\b/gu,
    /export\s+const\s+([A-Za-z0-9_]+)\b/gu,
    /export\s+type\s+([A-Za-z0-9_]+)\b/gu,
    /export\s+interface\s+([A-Za-z0-9_]+)\b/gu,
    /export\s*\{([^}]+)\}/gu,
  ];

  for (const pattern of patterns) {
    for (const match of sourceText.matchAll(pattern)) {
      if (pattern === patterns[4]) {
        const clause = match[1] ?? "";
        for (const entry of clause.split(",")) {
          const trimmed = entry.trim();
          if (trimmed.length === 0) {
            continue;
          }
          const [left] = trimmed.split(/\s+as\s+/u);
          if (left) {
            names.add(left.trim());
          }
        }
      } else if (match[1]) {
        names.add(match[1]);
      }
    }
  }

  return names;
}

describe("ir1-ssa-ripout", () => {
  it(
    "lowers fallback-era supported mutating fixtures through SSA",
    async () => {
      const fixtures = [
        ["functions-mutating.ts", "deposit"],
        ["expressions-map-mutation.ts", "setAndCopy"],
        ["expressions-set-mutation-field.ts", "tagClearAndAdd"],
        ["functions-mutating-loop.ts", "forEachActivate"],
        ["functions-mutating-loop.ts", "forEachSum"],
      ] as const;

      for (const [fileName, functionName] of fixtures) {
        const output = await emitFixture(fileName, functionName);
        assert.doesNotMatch(
          output,
          /^> UNSUPPORTED:/mu,
          `${fileName} > ${functionName} should keep lowering through SSA`,
        );
        assert.match(
          output,
          /\n---\n\n\S/mu,
          `${fileName} > ${functionName} should still emit body propositions`,
        );
      }
    },
  );

  it(
    "const aliases and state-aware reads survive without symbolic fallback",
    async () => {
      const fixtures = [
        ["functions-mutating-const.ts", "aliasedSequentialWrites"],
        ["expressions-state-aware-reads.ts", "entrySetThenCheck"],
        ["expressions-state-aware-reads.ts", "bumpInBranch"],
        ["expressions-state-aware-reads.ts", "tagThenCheck"],
      ] as const;

      for (const [fileName, functionName] of fixtures) {
        const output = await emitFixture(fileName, functionName);
        assert.doesNotMatch(
          output,
          /^> UNSUPPORTED:/mu,
          `${fileName} > ${functionName} should keep lowering through SSA`,
        );
        assert.match(
          output,
          /\n---\n\n\S/mu,
          `${fileName} > ${functionName} should still emit body propositions`,
        );
      }
    },
  );

  it("unsupported SSA build failure emits no frames", () => {
    const props = translateMutatingSource(
      `
        interface Account {
          balance: number;
          owner: string;
        }

        function unsupportedLoop(a: Account, n: number): void {
          while (n > 0) {
            a.balance = 1;
          }
        }
      `,
      "unsupportedLoop",
    );

    assert.ok(props.length > 0, "expected an explicit diagnostic result");
    assert.equal(
      props.every((prop) => prop.kind === "unsupported"),
      true,
      "unsupported mutating bodies should fail closed with diagnostics only",
    );
    assert.equal(
      props.some((prop) => prop.kind !== "unsupported"),
      false,
      "unsupported mutating bodies must not emit equations, assertions, or frames",
    );
  });

  it("translate-body has no symbolicExecute fallback", () => {
    const sourceText = readFileSync(resolve(SRC_DIR, "translate-body.ts"), "utf8");
    const translateMutatingBodySource =
      extractTranslateMutatingBodySource(sourceText);

    assert.doesNotMatch(
      translateMutatingBodySource,
      /\bsymbolicExecute\s*\(/u,
      "translateMutatingBody should no longer call symbolicExecute",
    );
    assert.doesNotMatch(
      translateMutatingBodySource,
      /\bemitMapEquations\s*\(/u,
      "translateMutatingBody should no longer emit legacy Map equations directly",
    );
    assert.doesNotMatch(
      translateMutatingBodySource,
      /\bemitSetMembershipEquation\s*\(/u,
      "translateMutatingBody should no longer emit legacy Set equations directly",
    );
  });

  it("obsolete symbolic-state helpers are not exported", () => {
    const forbiddenExports = new Set([
      "putWrite",
      "addWrittenKey",
      "mergeOverrides",
      "installMapWrite",
      "installSetWrite",
      "readMapThroughWrites",
      "readSetThroughWrites",
    ]);
    const exported = new Set<string>();

    for (const file of sourceFilesUnder(SRC_DIR)) {
      const sourceText = readFileSync(file, "utf8");
      for (const name of exportedSymbolNames(sourceText)) {
        exported.add(name);
      }
    }

    assert.deepEqual(
      [...forbiddenExports].filter((name) => exported.has(name)),
      [],
      "legacy symbolic-state helpers should be deleted, unexported, or renamed to pure SSA utilities",
    );
  });
});
