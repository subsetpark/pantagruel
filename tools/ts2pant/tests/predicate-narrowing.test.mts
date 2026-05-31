import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";
import ts from "typescript";
import { runCheck } from "../src/emit.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { recognizeTypePredicateNarrowing } from "../src/narrowing-recognizer.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import {
  buildDocument,
  emitAndCheck,
  getPantBin,
  PROJECT_ROOT,
  solverAvailable,
} from "./helpers.mts";

const FIXTURES = resolve(import.meta.dirname, "fixtures/constructs");

before(async () => {
  await loadAst();
});

function parseReturnExpr(source: string): {
  expr: ts.Expression;
  checker: ts.TypeChecker;
} {
  const sf = createSourceFileFromSource(source);
  const checker = getChecker(sf);
  let expr: ts.Expression | undefined;
  function visit(node: ts.Node): void {
    if (ts.isReturnStatement(node) && node.expression) {
      expr = node.expression;
    }
    ts.forEachChild(node, visit);
  }
  ts.forEachChild(sf.compilerNode, visit);
  if (!expr) {
    throw new Error("test helper: expected return expression");
  }
  return { expr, checker };
}

describe("predicate narrowing helpers", () => {
  it("recognizeTypePredicateNarrowing maps an x is T call to a fact", () => {
    const { expr, checker } = parseReturnExpr(`
      interface User { name: string; }
      declare function isUser(x: User | string): x is User;
      function f(x: User | string): boolean {
        return isUser(x);
      }
    `);

    const fact = recognizeTypePredicateNarrowing(expr, checker);

    assert.equal(fact?.kind, "predicate");
    assert.equal(
      fact?.kind === "predicate" ? getAst().strExpr(fact.testExpr) : null,
      "isUser x",
    );
    assert.deepEqual(
      fact?.kind === "predicate" ? fact.typePredicate : undefined,
      { receiver: "x", negated: false, tractable: true },
    );
  });

  it("recognizeTypePredicateNarrowing ignores ordinary boolean calls", () => {
    const { expr, checker } = parseReturnExpr(`
      declare function isReady(x: string): boolean;
      function f(x: string): boolean {
        return isReady(x);
      }
    `);

    assert.equal(recognizeTypePredicateNarrowing(expr, checker), null);
  });

  it("recognizeTypePredicateNarrowing maps an inferred type-predicate call to a fact", () => {
    const { expr, checker } = parseReturnExpr(`
      function isString(x: string | number) {
        return typeof x === "string";
      }
      function f(x: string | number): boolean {
        return isString(x);
      }
    `);

    const fact = recognizeTypePredicateNarrowing(expr, checker);

    assert.equal(fact?.kind, "predicate");
    assert.equal(
      fact?.kind === "predicate" ? getAst().strExpr(fact.testExpr) : null,
      "isString x",
    );
  });

  it("tractable x is T refinement is discharged (@pant entails)", {
    skip: !solverAvailable() ? "z3 not available" : undefined,
  }, async () => {
    const doc = await buildDocument(
      resolve(FIXTURES, "expressions-predicate-narrowing.ts"),
      "predicateValue",
    );
    const output = await emitAndCheck(doc);
    assert.match(output, /^true\.$/mu);
    const result = runCheck(output, {
      projectRoot: PROJECT_ROOT,
      pantBin: getPantBin(),
    });
    assert.equal(result.passed, true, result.output);
    assert.ok(
      result.checks.some((c) => c.message.startsWith("OK: Entailed:")),
      result.output,
    );
  });

  it("cross-call/opaque x is T is soundly not discharged", {
    skip: !solverAvailable() ? "z3 not available" : undefined,
  }, async () => {
    const doc = await buildDocument(
      resolve(FIXTURES, "expressions-predicate-narrowing.ts"),
      "predicateCircleRadius",
    );
    const output = await emitAndCheck(doc);
    assert.doesNotMatch(output, /^true\.$/mu);
    const result = runCheck(output, {
      projectRoot: PROJECT_ROOT,
      pantBin: getPantBin(),
    });
    assert.equal(result.passed, false, result.output);
    assert.ok(
      result.checks.some((c) => c.message.startsWith("FAIL: Not entailed:")),
      result.output,
    );
  });
});
