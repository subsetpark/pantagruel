import assert from "node:assert/strict";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { ir1Binop, ir1LitNat, ir1Opaque, ir1Var } from "../src/ir1.js";
import { OPAQUE_DOMAIN, contagiousOpaque, isOpaqueExpr } from "../src/opaque.js";
import { createSourceFile } from "../src/extract.js";
import { buildDocumentFromSourceFile, emitAndCheck } from "./helpers.mjs";

const OPAQUE_FIXTURE_DIR = resolve(import.meta.dirname, "fixtures/opaque");

describe("opaque opt-in policy", () => {
  const origin = { file: "tests/opaque-opt-in.ts", line: 1 };

  it("isOpaqueExpr identifies the OpaqueValue L1 form", () => {
    assert.equal(isOpaqueExpr(ir1Opaque(OPAQUE_DOMAIN, origin)), true);
    assert.equal(isOpaqueExpr(ir1Var("x")), false);
    assert.equal(isOpaqueExpr(ir1LitNat(1)), false);
  });

  it("contagiousOpaque returns an opaque result iff an operand is opaque", () => {
    const concrete = ir1Binop("add", ir1LitNat(1), ir1LitNat(2));
    const opaque = ir1Opaque(OPAQUE_DOMAIN, origin);
    const resultOrigin = { file: "tests/opaque-opt-in.ts", line: 2 };

    assert.equal(contagiousOpaque([ir1LitNat(1), concrete], resultOrigin), null);
    assert.deepEqual(contagiousOpaque([ir1LitNat(1), opaque], resultOrigin), {
      kind: "opaque",
      sort: OPAQUE_DOMAIN,
      origin: resultOrigin,
    });
  });

  it("signature composite opacity is precision-preserving under policy opaque (@pant checks)", async (t) => {
    const sourceFile = createSourceFile(
      resolve(OPAQUE_FIXTURE_DIR, "signatures.ts"),
    );
    const targets = [
      "scalarUnknown",
      "scalarAny",
      "unknownArray",
      "numberUnknownTuple",
      "stringUnknownMap",
      "numberUnknownUnion",
      "nullableUnknown",
      "anonymousRecord",
    ];

    for (const target of targets) {
      const doc = await buildDocumentFromSourceFile(sourceFile, target, {
        noBody: true,
        policy: "opaque",
      });
      const output = await emitAndCheck(doc);
      t.assert.snapshot(output);
    }
  });

  it("body operation with an opaque operand propagates opacity (contagion; @pant checks)", async (t) => {
    const sourceFile = createSourceFile(
      resolve(OPAQUE_FIXTURE_DIR, "bodies.ts"),
    );
    const targets = [
      "readUnknown",
      "arithmeticContagion",
      "logicalContagion",
      "callContagion",
      "memberContagion",
      "conditionalContagion",
      "mutatingReturnContagion",
    ];

    for (const target of targets) {
      const doc = await buildDocumentFromSourceFile(sourceFile, target, {
        policy: "opaque",
      });
      const output = await emitAndCheck(doc);
      t.assert.snapshot(output);
    }
  });
});
