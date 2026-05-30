import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";
import ts from "typescript";
import {
  createAssumptionEnv,
  enterFrame,
  type Fact,
  nonNullFactInScope,
  pushFact,
} from "../src/assumption-env.js";
import { renderNullishObligation } from "../src/definedness-obligation.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import {
  negateFact,
  recognizeNullishNarrowing,
} from "../src/narrowing-recognizer.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import {
  buildDocument,
  buildDocumentFromSourceFile,
  emitAndCheck,
} from "./helpers.mjs";

const FIXTURES = resolve(import.meta.dirname, "fixtures/constructs");

before(async () => {
  await loadAst();
});

function parseExpr(source: string): {
  expr: ts.Expression;
  checker: ts.TypeChecker;
} {
  const sf = createSourceFileFromSource(`
    declare const x: number | null | undefined;
    declare const y: number | null | undefined;
    const _ = (${source});
  `);
  const checker = getChecker(sf);
  const stmt = sf.compilerNode.statements.at(-1);
  if (!stmt || !ts.isVariableStatement(stmt)) {
    throw new Error("test helper: expected trailing variable statement");
  }
  const init = stmt.declarationList.declarations[0]?.initializer;
  if (!init) {
    throw new Error("test helper: expected initializer");
  }
  let expr: ts.Expression = init;
  while (ts.isParenthesizedExpression(expr)) {
    expr = expr.expression;
  }
  return { expr, checker };
}

function recognize(source: string): Fact | null {
  const { expr, checker } = parseExpr(source);
  return recognizeNullishNarrowing(expr, checker);
}

describe("nullish narrowing helpers", () => {
  it("recognizeNullishNarrowing maps nullish shapes to a non-null fact", () => {
    for (const source of [
      "x != null",
      "x !== null",
      "x !== undefined",
      "typeof x !== 'undefined'",
      "x !== null && x !== undefined",
    ]) {
      assert.deepEqual(recognize(source), {
        kind: "non-null",
        receiver: "x",
        negated: false,
      });
    }
  });

  it("recognizeNullishNarrowing maps null branches to a negated non-null fact", () => {
    for (const source of [
      "x == null",
      "x === null",
      "x === undefined",
      "typeof x === 'undefined'",
      "x === null || x === undefined",
    ]) {
      assert.deepEqual(recognize(source), {
        kind: "non-null",
        receiver: "x",
        negated: true,
      });
    }
  });

  it("negateFact flips non-null and is-null", () => {
    const fact: Fact = {
      kind: "non-null",
      receiver: "x",
      negated: false,
    };

    assert.deepEqual(negateFact(fact), {
      kind: "non-null",
      receiver: "x",
      negated: true,
    });
    assert.deepEqual(negateFact(negateFact(fact)), fact);
  });

  it("finds matching non-null facts", () => {
    const env = createAssumptionEnv();
    enterFrame(env);
    pushFact(env, { kind: "non-null", receiver: "x", negated: false });
    pushFact(env, { kind: "non-null", receiver: "y", negated: false });
    pushFact(env, { kind: "non-null", receiver: "x", negated: true });

    assert.deepEqual(nonNullFactInScope(env, "x"), [
      { receiver: "x", negated: false },
      { receiver: "x", negated: true },
    ]);
  });

  it("renderNullishObligation renders a discharged cardinality goal", () => {
    const ast = getAst();
    const obligation = renderNullishObligation({
      receiver: ast.var("x"),
      inScope: [{ receiver: "x", negated: false }],
    });

    assert.equal(obligation.text, "true");
  });

  it("renderNullishObligation only fast-paths matching receivers", () => {
    const ast = getAst();
    const obligation = renderNullishObligation({
      receiver: ast.var("x"),
      inScope: [{ receiver: "y", negated: false }],
    });

    assert.notEqual(obligation.text, "true");
  });

  it("x !== null guard emits narrowed optional read", async () => {
    const doc = await buildDocument(
      resolve(FIXTURES, "expressions-nullish-narrowing.ts"),
      "strictNullRead",
    );
    const output = await emitAndCheck(doc);
    assert.match(
      output,
      /strict-null-read x d = \(cond ~\(#x = 0\) => x 1, true => d\)\./u,
    );
  });

  it("optional read in the null branch stays unchanged", async () => {
    const doc = await buildDocument(
      resolve(FIXTURES, "expressions-nullish-narrowing.ts"),
      "nullBranchControl",
    );
    const output = await emitAndCheck(doc);
    assert.match(
      output,
      /null-branch-control x d = \(cond #x = 0 => x, true => d\)\./u,
    );
  });

  it("shadowed block locals do not reuse outer non-null facts", async () => {
    const sourceFile = createSourceFileFromSource(`
      export function shadowedBlockLocal(
        x: number | null,
        y: number | null,
      ): number | null {
        if (x !== null) {
          const x = y;
          return x;
        }
        return y;
      }
    `);
    const doc = await buildDocumentFromSourceFile(
      sourceFile,
      "shadowedBlockLocal",
    );
    const output = await emitAndCheck(doc);

    assert.doesNotMatch(output, /\$\d+/u);
    assert.doesNotMatch(output, /\$\d+ 1/u);
  });
});
