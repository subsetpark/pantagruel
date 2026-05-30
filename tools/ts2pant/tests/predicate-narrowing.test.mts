import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { recognizeTypePredicateNarrowing } from "../src/narrowing-recognizer.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

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

  it.skip(
    "tractable x is T refinement is discharged (@pant entails) — PENDING: Patch 3",
    () => {},
  );

  it.skip(
    "cross-call/opaque x is T is soundly not discharged — PENDING: Patch 3",
    () => {},
  );
});
