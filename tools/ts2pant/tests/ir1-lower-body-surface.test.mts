// @archlint.module test
// @archlint.domain ts2pant.ir1-lower-body

import assert from "node:assert/strict";
import { before, it } from "node:test";
import * as fc from "fast-check";
import { ir1Assign, ir1LitNat, ir1Member, ir1Var } from "../src/ir1.js";
import * as LB from "../src/ir1-lower-body.js";
import { lowerCollectionSsaToProps } from "../src/ir1-ssa-collections.js";
import { lowerForeachShapeAAsGeneralLoop } from "../src/ir1-ssa-foreach.js";
import { lowerScalarSsaToProps } from "../src/ir1-ssa-scalars.js";
import type { OpaqueExpr } from "../src/pant-ast.js";
import { loadAst } from "../src/pant-wasm.js";

before(async () => {
  await loadAst();
});

it("generated inputs exercise the ir1-lower-body exported surface", () => {
  fc.assert(
    fc.property(fc.constantFrom("Account_balance", "Account_limit"), (prop) => {
      const stmt = ir1Assign(ir1Member(ir1Var("a"), prop), ir1LitNat(1));
      const options: LB.SsaBodyLowerOptions = {
        applyConst: (e: OpaqueExpr) => e,
      };
      assert.equal(
        LB.adaptScalarSsaLowerResult(lowerScalarSsaToProps(stmt)).programs
          .length,
        1,
      );
      assert.equal(
        LB.adaptCollectionSsaLowerResult(lowerCollectionSsaToProps(stmt))
          .programs.length,
        1,
      );
      assert.equal(
        LB.adaptLoopSsaLowerResult(
          lowerForeachShapeAAsGeneralLoop({
            binder: "item",
            source: ir1Var("items"),
            body: stmt,
          }),
        ).programs.length,
        1,
      );
      assert.equal(
        LB.lowerL1BodyToSsaProps(stmt, [], options).programs.length,
        1,
      );
      assert.equal(
        LB.lowerScalarL1BodyToSsaResult(stmt, options).programs.length,
        1,
      );
      assert.equal(
        LB.lowerCollectionL1BodyToSsaResult(stmt, options).programs.length,
        1,
      );
    }),
  );
});
