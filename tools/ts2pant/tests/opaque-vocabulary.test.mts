// @archlint.module test
// @archlint.domain ts2pant.opaque-vocabulary

import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { irOpaque } from "../src/ir.js";
import { lowerExpr } from "../src/ir-emit.js";
import { ir1LitNat, ir1Opaque, ir1Var } from "../src/ir1.js";
import { lowerL1Expr } from "../src/ir1-lower.js";
import { formatIR1Expr } from "../src/ir1-printer.js";
import {
  freeVarsIR1Expr,
  substituteIR1ExprSubtree,
} from "../src/ir1-substitute.js";
import { OPAQUE_DOMAIN, opaqueValueRuleName } from "../src/opaque.js";
import { loadAst } from "../src/pant-wasm.js";

describe("opaque vocabulary", async () => {
  const ast = await loadAst();

  it("L2 opaque lowers to a single OpaqueExpr reference", () => {
    const id = "src/example.ts:12<$value>";
    const expectedName = opaqueValueRuleName(id);

    assert.equal(OPAQUE_DOMAIN, "Opaque");
    assert.match(expectedName, /^[A-Za-z_][A-Za-z0-9_]*$/u);
    assert.equal(
      ast.strExpr(lowerExpr(irOpaque(OPAQUE_DOMAIN, id))),
      expectedName,
    );
  });

  it("L2 opaque ids determine constant identity", () => {
    const sameA = ast.strExpr(lowerExpr(irOpaque(OPAQUE_DOMAIN, "same.ts:1")));
    const sameB = ast.strExpr(lowerExpr(irOpaque(OPAQUE_DOMAIN, "same.ts:1")));
    const other = ast.strExpr(lowerExpr(irOpaque(OPAQUE_DOMAIN, "other.ts:1")));

    assert.equal(sameA, sameB);
    assert.notEqual(sameA, other);
  });

  it("L1 OpaqueValue is a closed leaf across all walkers and lowers to L2 opaque", () => {
    const expr = ir1Opaque(OPAQUE_DOMAIN, {
      file: "src/example.ts",
      line: 12,
    });
    const other = ir1Opaque(OPAQUE_DOMAIN, {
      file: "src/example.ts",
      line: 13,
    });

    assert.deepEqual([...freeVarsIR1Expr(expr)], []);
    assert.equal(
      substituteIR1ExprSubtree(expr, ir1Var("x"), ir1LitNat(1)),
      expr,
    );
    assert.equal(formatIR1Expr(expr), "<opaque:Opaque>");

    const lowered = lowerL1Expr(expr);
    const loweredOther = lowerL1Expr(other);

    assert.deepEqual(lowered, {
      kind: "opaque",
      sort: OPAQUE_DOMAIN,
      id: "src/example.ts:12",
    });
    assert.deepEqual(loweredOther, {
      kind: "opaque",
      sort: OPAQUE_DOMAIN,
      id: "src/example.ts:13",
    });
    if (lowered.kind !== "opaque" || loweredOther.kind !== "opaque") {
      assert.fail("expected L1 opaque values to lower to L2 opaque values");
    }
    assert.notEqual(lowered.id, loweredOther.id);
  });
});
