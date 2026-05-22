/**
 * Regression tests for the M2 L1 μ-search pipeline (workstream M2).
 *
 * After M2 patch 3 (cutover), the L1 path is the only μ-search path —
 * legacy `translateMuSearchInitLegacy` is deleted and the
 * `TS2PANT_USE_L1_MUSEARCH` flag is gone. These tests assert:
 *
 * 1. **All five `+1` spellings produce byte-identical output** —
 *    `i++`, `++i`, `i += 1`, `i = i + 1`, `i = 1 + i` collapse to
 *    one canonical L1 `Assign(Var(c), BinOp(add, Var(c), Lit(1)))`
 *    that lowers to `min over each j: T, j >= INIT, ¬P(j) | j`.
 *    This is the M2 architectural promise.
 * 2. **Existing μ-search shapes still translate** — `i++` step, post-
 *    loop counter use, const-binding interleave.
 * 3. **Conservative rejections fire** — compound while body,
 *    non-`+1` step, predicate not referencing counter.
 */

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";
import { emitDocument } from "../src/emit.js";
import {
  createSourceFile,
  createSourceFileFromSource,
  getChecker,
} from "../src/extract.js";
import {
  buildL1MuSearchCombTyped,
  isL1Unsupported,
  lowerL1ToOpaque,
} from "../src/ir1-build.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { translateBody } from "../src/translate-body.js";
import { IntStrategy } from "../src/translate-types.js";
import { buildDocumentFromSourceFile } from "./helpers.mts";
import ts from "typescript";

before(async () => {
  await loadAst();
});

function extractEquationRhs(output: string, entryName: string): string {
  const equationLine = output
    .trim()
    .split("\n")
    .find((line) => line.startsWith(`${entryName} `) && line.includes(" = "));
  assert.ok(equationLine, "expected an emitted equation line");
  return equationLine.slice(equationLine.indexOf(" = ") + 3);
}

function translate(
  source: string,
  name: string,
): { unsupported: string | null; pant: string | null } {
  const sourceFile = createSourceFileFromSource(source);
  const props = translateBody({
    sourceFile,
    functionName: name,
    strategy: IntStrategy,
  });
  if (props.length === 0) {
    return { unsupported: "no propositions", pant: null };
  }
  const unsupported = props.find((prop) => prop.kind === "unsupported");
  if (unsupported !== undefined) {
    return { unsupported: unsupported.reason, pant: null };
  }
  const equations = props.filter((prop) => prop.kind === "equation");
  if (equations.length === 0) {
    return { unsupported: "no equation proposition", pant: null };
  }
  return {
    unsupported: null,
    pant: equations.map((p) => getAst().strExpr(p.rhs)).join(" ; "),
  };
}

function buildFirstLetWhileCombTyped(source: string, name: string) {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.getFunctionOrThrow(name).compilerNode;
  assert.ok(fn.body, "expected function body");
  const letStmt = fn.body.statements[0];
  const whileStmt = fn.body.statements[1];
  assert.ok(letStmt && ts.isVariableStatement(letStmt));
  assert.ok(whileStmt && ts.isWhileStatement(whileStmt));
  const decl = letStmt.declarationList.declarations[0];
  assert.ok(decl && ts.isIdentifier(decl.name) && decl.initializer);
  const body = whileStmt.statement;
  const stepStmt = ts.isBlock(body) ? body.statements[0] : body;
  assert.ok(stepStmt && ts.isExpressionStatement(stepStmt));

  const l1 = buildL1MuSearchCombTyped(
    {
      counterName: decl.name.text,
      initTsExpr: decl.initializer,
      predicateTsExpr: whileStmt.expression,
      stepExpr: stepStmt.expression,
    },
    {
      checker,
      strategy: IntStrategy,
      paramNames: new Map(),
      state: undefined,
      supply: { n: 0 },
    },
  );
  if (isL1Unsupported(l1)) {
    throw new Error(l1.unsupported);
  }
  return l1;
}

// ---------------------------------------------------------------------------
// Existing μ-search shapes still translate
// ---------------------------------------------------------------------------

describe("M2 μ-search L1: existing shapes translate", () => {
  it("`i++` step produces canonical min-over-each", () => {
    const source = `
      function firstUnused(used: ReadonlySet<number>): number {
        let i = 1;
        while (used.has(i)) i++;
        return i;
      }
    `;
    const { unsupported, pant } = translate(source, "firstUnused");
    assert.equal(unsupported, null);
    assert.match(pant!, /min over each j\d*: Int/u);
  });

  it("μ-search counter referenced post-loop", () => {
    const source = `
      function nextSlotPlusOne(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) i++;
        return i + 1;
      }
    `;
    const { unsupported, pant } = translate(source, "nextSlotPlusOne");
    assert.equal(unsupported, null);
    assert.match(pant!, /min over each j\d*: Int/u);
    assert.match(pant!, /\+ 1/u);
  });

  it("μ-search interleaved with const binding", () => {
    const source = `
      function offset(used: ReadonlySet<number>, base: number): number {
        const n = base * 2;
        let i = 1;
        while (used.has(i)) i++;
        return n + i;
      }
    `;
    const { unsupported } = translate(source, "offset");
    assert.equal(unsupported, null);
  });
});

// ---------------------------------------------------------------------------
// All five `+1` spellings produce byte-identical canonical output
// ---------------------------------------------------------------------------

describe("M2 μ-search L1: all five +1 spellings produce identical Pant", () => {
  function translateForm(stepSource: string): string {
    const source = `
      function findSuffix(used: ReadonlySet<number>): number {
        let i = 1;
        while (used.has(i)) { ${stepSource}; }
        return i;
      }
    `;
    const { unsupported, pant } = translate(source, "findSuffix");
    if (unsupported) {
      throw new Error(`unsupported: ${unsupported}`);
    }
    return pant!;
  }

  it("all five spellings produce identical L1 output", () => {
    // Each translation gets a fresh `UniqueSupply` and `SourceFile`, so
    // binder allocation is deterministic per call. The five spellings
    // must produce byte-identical Pant — the M2 architectural promise.
    const baseline = translateForm("i++");
    assert.match(baseline, /min over each j\d*: Int/u);
    assert.match(baseline, /~\(j\d* in used\)/u);

    assert.equal(translateForm("++i"), baseline);
    assert.equal(translateForm("i += 1"), baseline);
    assert.equal(translateForm("i = i + 1"), baseline);
    assert.equal(translateForm("i = 1 + i"), baseline);
  });

  it("fixture spellings still emit identical Pant through loop summaries", async () => {
    const filePath = resolve(
      import.meta.dirname,
      "fixtures/constructs/expressions-while-mu-search.ts",
    );
    const sourceFile = createSourceFile(filePath);
    const funcs = [
      "firstUnusedSuffix",
      "unbracedWhileBody",
      "compoundIncrementStep",
      "explicitIncrementStep",
      "explicitIncrementStepFlipped",
    ];
    const outputs: Array<{ funcName: string; output: string }> = [];
    for (const funcName of funcs) {
      const doc = await buildDocumentFromSourceFile(sourceFile, funcName);
      outputs.push({ funcName, output: emitDocument(doc) });
    }

    const rhsOutputs = outputs.map(({ funcName, output }) => {
      const functionRhs = extractEquationRhs(
        output,
        funcName
          .replace(/[A-Z]/gu, (c) => `-${c.toLowerCase()}`)
          .replace(/^-/, ""),
      );
      return functionRhs === "i." ? extractEquationRhs(output, "i") : functionRhs;
    });
    for (const rhs of rhsOutputs) {
      assert.match(rhs, /min over each j\d*: Int/u);
    }
    for (const rhs of rhsOutputs.slice(1)) {
      assert.equal(rhs, rhsOutputs[0]);
    }
  });
});

describe("post-migration recognizer placement", () => {
  it("recognizeLetWhilePair produces L1 comb-typed", () => {
    const l1 = buildFirstLetWhileCombTyped(
      `
        function firstUnused(used: ReadonlySet<number>): number {
          let i = 1;
          while (used.has(i)) i++;
          return i;
        }
      `,
      "firstUnused",
    );

    assert.equal(l1.kind, "comb-typed");
    assert.equal(l1.combiner, "min");
    assert.equal(l1.binderType, "Int");
    assert.equal(l1.guards.length, 2);
    assert.equal(l1.proj.kind, "var");
    assert.equal(l1.proj.name, l1.binder);
  });

  it("canonical post-recognizer shape is comb-typed", () => {
    const l1 = buildFirstLetWhileCombTyped(
      `
        function firstUnused(used: ReadonlySet<number>): number {
          let i = 1;
          while (used.has(i)) { i = 1 + i; }
          return i;
        }
      `,
      "firstUnused",
    );

    const pant = getAst().strExpr(lowerL1ToOpaque(l1));
    assert.equal(l1.kind, "comb-typed");
    assert.match(pant, /^min over each j\d*: Int/u);
    assert.match(pant, /j\d* >= 1/u);
    assert.match(pant, /~\(j\d* in used\)/u);
  });

  it("μ-search fixtures snapshot-equivalent", async () => {
    const filePath = resolve(
      import.meta.dirname,
      "fixtures/constructs/expressions-while-mu-search.ts",
    );
    const sourceFile = createSourceFile(filePath);
    const doc = await buildDocumentFromSourceFile(
      sourceFile,
      "firstUnusedSuffix",
    );
    const output = emitDocument(doc);
    assert.match(output, /i\s+=> Int/u);
    assert.match(
      output,
      /i = \(min over each j: Int, j >= 1, ~\(j in used\) \| j\)\./u,
    );
    assert.match(output, /first-unused-suffix used = i\./u);
  });
});

// ---------------------------------------------------------------------------
// Conservative rejections
// ---------------------------------------------------------------------------

describe("M2 μ-search L1: rejection cases", () => {
  it("compound while body rejects", () => {
    const source = `
      function compound(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) {
          i++;
          i++;
        }
        return i;
      }
    `;
    const { unsupported } = translate(source, "compound");
    assert.notEqual(unsupported, null);
  });

  it("`i += 2` step rejects (non-canonical)", () => {
    const source = `
      function nonUnit(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) {
          i += 2;
        }
        return i;
      }
    `;
    const { unsupported } = translate(source, "nonUnit");
    assert.notEqual(unsupported, null);
    assert.match(unsupported!, /canonical|step/u);
  });

  it("predicate not referencing counter rejects", () => {
    const source = `
      function noRef(used: ReadonlySet<number>, flag: boolean): number {
        let i = 0;
        while (flag) {
          i++;
        }
        return i;
      }
    `;
    const { unsupported } = translate(source, "noRef");
    assert.notEqual(unsupported, null);
  });
});
