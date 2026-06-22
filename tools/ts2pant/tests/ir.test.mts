// @archlint.module test
// @archlint.domain ts2pant.ir

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import * as fc from "fast-check";
import {
  irComb,
  irCond,
  irEach,
  irExists,
  irForall,
  irLitBool,
  irLitNat,
  irVar,
  isFoldComb,
} from "../src/ir.js";
import { lowerBinop, lowerExpr } from "../src/ir-emit.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

before(async () => {
  await loadAst();
});

describe("ir properties", () => {
  it("binop lowering produces expected Pant text", () => {
    assert.equal(
      getAst().strExpr(
        getAst().binop(
          lowerBinop("add"),
          lowerExpr(irLitNat(1)),
          lowerExpr(irLitNat(2)),
        ),
      ),
      "1 + 2",
    );
  });

  it("generated binder forms preserve binders and lower to Pant text", () => {
    fc.assert(
      fc.property(
        fc.stringMatching(/^[a-z][a-z0-9]{0,6}$/u),
        fc.integer({ min: 0, max: 1000 }),
        (name, value) => {
          const body = irVar(name);
          const each = irEach(name, irVar("xs"), [irLitBool(true)], body);
          const comb = irComb("add", each);
          const cond = irCond(
            [[irLitBool(true), irLitNat(value)]],
            irLitNat(0),
          );
          const forall = irForall(name, "Nat0", body, irLitBool(true));
          const exists = irExists(name, "Nat0", body);

          assert.equal(each.binder, name);
          assert.equal(isFoldComb(comb), true);
          assert.match(getAst().strExpr(lowerExpr(cond)), /cond/u);
          assert.match(
            getAst().strExpr(lowerExpr(forall)),
            new RegExp(name, "u"),
          );
          assert.match(
            getAst().strExpr(lowerExpr(exists)),
            new RegExp(name, "u"),
          );
        },
      ),
    );
  });
});
