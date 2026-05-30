import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import {
  createAssumptionEnv,
  enterFrame,
  nonNullFactInScope,
  pushFact,
  type Fact,
} from "../src/assumption-env.js";
import { renderNullishObligation } from "../src/definedness-obligation.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import {
  negateFact,
  recognizeNullishNarrowing,
} from "../src/narrowing-recognizer.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

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

  it("nonNullFactInScope finds matching non-null facts", () => {
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

  it("renderNullishObligation renders the cardinality goal", () => {
    const ast = getAst();
    const obligation = renderNullishObligation({
      receiver: ast.var("x"),
      inScope: [{ receiver: "x", negated: false }],
    });

    assert.equal(obligation.text, "#x > 0 -> #x > 0");
  });

  it.skip(
    "x !== null guard discharges optional read (@pant entails) — PENDING: Patch 2",
    () => {},
  );

  it.skip(
    "optional read in the null branch is not entailed (negative control) — PENDING: Patch 2",
    () => {},
  );
});
