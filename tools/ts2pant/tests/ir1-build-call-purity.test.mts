import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import {
  type L1BuildContext,
  tryBuildL1PureSubExpression,
} from "../src/ir1-build.js";
import { loadAst } from "../src/pant-wasm.js";
import type { UniqueSupply } from "../src/translate-body.js";
import {
  cellRegisterName,
  IntStrategy,
  newSynthCell,
  toPantTermName,
} from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

function setupReturnExpr(source: string): {
  expr: ts.Expression;
  ctx: L1BuildContext;
} {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements.find(
    (stmt): stmt is ts.FunctionDeclaration =>
      ts.isFunctionDeclaration(stmt) && stmt.body !== undefined,
  );
  if (!fn?.body) {
    throw new Error("setup: expected function body");
  }
  const synthCell = newSynthCell();
  const paramNames = new Map<string, string>();
  for (const param of fn.parameters) {
    if (ts.isIdentifier(param.name)) {
      paramNames.set(
        param.name.text,
        cellRegisterName(synthCell, toPantTermName(param.name.text)),
      );
    }
  }
  const stmt = fn.body.statements[0];
  if (!stmt || !ts.isReturnStatement(stmt) || !stmt.expression) {
    throw new Error("setup: expected return expression");
  }
  const supply: UniqueSupply = { n: 0, synthCell };
  return {
    expr: stmt.expression,
    ctx: {
      checker,
      strategy: IntStrategy,
      paramNames,
      state: undefined,
      supply,
    },
  };
}

describe("ir1-build call purity", () => {
  it("does not lower known collection mutations as pure native calls", () => {
    const setAdd = setupReturnExpr(`
      function f(xs: Set<number>, x: number): Set<number> {
        return xs.add(x);
      }
    `);
    const arrayPush = setupReturnExpr(`
      function f(xs: number[], x: number): number {
        return xs.push(x);
      }
    `);
    assert.equal(tryBuildL1PureSubExpression(setAdd.expr, setAdd.ctx), null);
    assert.equal(
      tryBuildL1PureSubExpression(arrayPush.expr, arrayPush.ctx),
      null,
    );
  });

  it("does not lower a member call whose receiver has unknown effects", () => {
    const { expr, ctx } = setupReturnExpr(`
      declare function makeSet(): Set<number>;
      function f(x: number): boolean {
        return makeSet().has(x);
      }
    `);
    assert.equal(tryBuildL1PureSubExpression(expr, ctx), null);
  });
});
