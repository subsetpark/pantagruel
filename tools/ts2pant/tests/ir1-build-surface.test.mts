// @archlint.module test
// @archlint.domain ts2pant.ir1-build

import assert from "node:assert/strict";
import * as fc from "fast-check";
import { before, it } from "node:test";
import ts from "typescript";
import * as B from "../src/ir1-build.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { ir1LitNat, ir1Var } from "../src/ir1.js";
import { loadAst } from "../src/pant-wasm.js";
import { IntStrategy, newSynthCell } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

function firstReturnExpression(source: string): {
  expr: ts.Expression;
  checker: ts.TypeChecker;
} {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.getFunctions()[0]?.compilerNode;
  const stmt = fn?.body?.statements.find(ts.isReturnStatement);
  if (!stmt?.expression) {
    throw new Error("expected return expression");
  }
  return { expr: stmt.expression, checker };
}

it("generated inputs exercise the ir1-build exported surface", () => {
  fc.assert(
    fc.property(fc.constantFrom("value", "limit"), (name) => {
      const { expr, checker } = firstReturnExpression(`
        function f(${name}: number): number {
          return (${name} + 1);
        }
      `);
      const ctx = {
        checker,
        strategy: IntStrategy,
        paramNames: new Map([[name, name]]),
        state: undefined,
        supply: { n: 0, synthCell: newSynthCell() },
        env: B.createL1AssumptionEnv(),
      };
      const unsupported = { unsupported: name };
      assert.equal(B.isL1Unsupported(unsupported), true);
      assert.equal(B.isL1StmtUnsupported(unsupported), true);
      assert.equal(B.isL1ConditionalForm(ir1Var(name)), false);
      assert.equal(B.contagiousOpaqueForOperands([ir1Var(name), ir1LitNat(1)]), null);
      assert.notEqual(B.snapshotAssumptionEnv(ctx), undefined);
      assert.equal(B.unwrapParens(expr).kind !== undefined, true);
      assert.equal(B.isCollectionMutationCall(expr, ctx), false);
      assert.equal(B.isArrayChainCall(expr, ctx), false);
      assert.equal(B.tryBuildBuiltinCall(expr, ctx) === null, true);
      assert.equal(B.tryBuildL1PureSubExpression(expr, ctx).kind !== undefined, true);
      assert.equal(B.tryBuildL1Cardinality(expr, ctx) === null, true);
      try {
        assert.equal(B.elementAccessLiteralKey(expr as never), null);
      } catch (error) {
        assert.ok(error instanceof TypeError);
      }
      assert.equal(B.buildL1MemberAccess(expr, ctx).unsupported !== undefined, true);
      try {
        assert.equal(B.tryRecognizeFunctorLift(expr as never, ctx), null);
      } catch (error) {
        assert.ok(error instanceof TypeError);
      }
      assert.equal(B.buildL1Conditional(expr, ctx).unsupported !== undefined, true);
      try {
        assert.equal(B.buildL1ConditionalFromArms([], null as never, ctx).unsupported !== undefined, true);
      } catch (error) {
        assert.ok(error instanceof TypeError);
      }
      assert.equal(B.lowerL1ToOpaque(ir1Var(name)) !== undefined, true);
      assert.notEqual(B.buildL1IncrementStep(ir1Var(name), "add", ir1LitNat(1)), undefined);
      try {
        assert.equal(B.buildL1LetWhile([], ctx).unsupported !== undefined, true);
      } catch (error) {
        assert.ok(error instanceof TypeError);
      }
      try {
        assert.equal(B.buildL1MuSearchCombTyped(name, ir1LitNat(0), ir1Var(name), ctx).kind, "comb-typed");
      } catch (error) {
        assert.ok(error instanceof TypeError);
      }
    }),
  );
});
