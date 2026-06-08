// @archlint.module test
// @archlint.domain ts2pant.ir1-build-body

import assert from "node:assert/strict";
import * as fc from "fast-check";
import { before, it } from "node:test";
import ts from "typescript";
import * as BB from "../src/ir1-build-body.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { loadAst } from "../src/pant-wasm.js";
import { IntStrategy, newSynthCell } from "../src/translate-types.js";
import { createL1AssumptionEnv } from "../src/ir1-build.js";

before(async () => {
  await loadAst();
});

it("generated inputs exercise the ir1-build-body exported surface", () => {
  fc.assert(
    fc.property(fc.constantFrom("value", "limit"), (name) => {
      const sourceFile = createSourceFileFromSource(`
        function f(${name}: number): number {
          ${name} = ${name} + 1;
          return ${name};
        }
      `);
      const checker = getChecker(sourceFile);
      const fn = sourceFile.getFunctions()[0]!.compilerNode;
      const stmt = fn.body!.statements[0]!;
      const expr = (fn.body!.statements[1] as ts.ReturnStatement).expression!;
      const ctx = {
        checker,
        strategy: IntStrategy,
        paramNames: new Map([[name, name]]),
        state: undefined,
        supply: { n: 0, synthCell: newSynthCell() },
        env: createL1AssumptionEnv(),
      };
      assert.equal(BB.isUnsupported({ unsupported: name }), true);
      assert.equal(BB.buildL1SubExpr(expr, ctx).kind !== undefined, true);
      assert.equal(BB.buildL1AssignStmt(stmt, ctx).kind !== undefined, true);
      assert.equal(BB.buildL1IfMutation(stmt, ctx).unsupported !== undefined, true);
      try {
        assert.equal(BB.buildL1ForOfMutation(stmt as never, ctx).unsupported !== undefined, true);
      } catch (error) {
        assert.ok(error instanceof TypeError);
      }
      try {
        assert.equal(BB.buildL1ForEachCall(expr as never, ctx).unsupported !== undefined, true);
      } catch (error) {
        assert.ok(error instanceof TypeError);
      }
      try {
        assert.equal(BB.buildL1EffectCall(expr as never, ctx).unsupported !== undefined, true);
      } catch (error) {
        assert.ok(error instanceof TypeError);
      }
    }),
  );
});
