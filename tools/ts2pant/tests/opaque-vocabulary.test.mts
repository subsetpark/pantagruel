import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { irOpaque } from "../src/ir.js";
import { lowerExpr } from "../src/ir-emit.js";
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

  it.skip("L1 OpaqueValue is a closed leaf across all walkers and lowers to L2 opaque", () => {
    // TODO(ts2pant-opaque-vocabulary patch 2): implement once L1 OpaqueValue exists.
  });
});
