// @archlint.module test
// @archlint.domain ts2pant.opaque

import assert from "node:assert/strict";
import * as fc from "fast-check";
import { resolve } from "node:path";
import { describe, it } from "node:test";
import { ir1Binop, ir1LitNat, ir1Opaque, ir1Var } from "../src/ir1.js";
import {
  OPAQUE_DOMAIN,
  contagiousOpaque,
  isOpaqueExpr,
  opaqueValueRuleName,
} from "../src/opaque.js";
import { emitDocument } from "../src/emit.js";
import { createSourceFile } from "../src/extract.js";
import { buildDocumentFromSourceFile, emitAndCheck } from "./helpers.mjs";

const OPAQUE_FIXTURE_DIR = resolve(import.meta.dirname, "fixtures/opaque");
const CONSTRUCTS_FIXTURE_DIR = resolve(
  import.meta.dirname,
  "fixtures/constructs",
);

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

  it("opaque rule names are deterministic and Pant-safe", () => {
    fc.assert(
      fc.property(fc.string(), (id) => {
        const name = opaqueValueRuleName(id);

        assert.equal(opaqueValueRuleName(id), name);
        assert.match(name, /^opaque_value_\d+(?:_[0-9a-f]{4})*$/u);
      }),
    );
  });

  it("contagiousOpaque follows generated operand opacity", () => {
    fc.assert(
      fc.property(fc.array(fc.boolean(), { maxLength: 12 }), (opaqueFlags) => {
        const resultOrigin = { file: "tests/opaque-opt-in.ts", line: 3 };
        const operands = opaqueFlags.map((flag, index) =>
          flag
            ? ir1Opaque(OPAQUE_DOMAIN, { file: "tests/opaque-opt-in.ts", line: index })
            : ir1LitNat(index),
        );

        const result = contagiousOpaque(operands, resultOrigin);

        if (opaqueFlags.includes(true)) {
          assert.deepEqual(result, {
            kind: "opaque",
            sort: OPAQUE_DOMAIN,
            origin: resultOrigin,
          });
          assert.equal(isOpaqueExpr(result), true);
        } else {
          assert.equal(result, null);
        }
      }),
    );
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

  it("integer mutating-loop bodies build under policy opaque (regression: no synthetic-node crash)", async () => {
    // Compound-assignment (`+=`) and `++`/`--` desugaring synthesizes
    // binary-expression nodes with no source file. Under policy "opaque" the
    // contagion path used to compute an origin for these unconditionally and
    // crash in `sourceRefForNode`. These fixtures contain no `any`/`unknown`,
    // so opacity never triggers — they must simply translate without throwing.
    const cases: ReadonlyArray<readonly [string, string]> = [
      ["functions-mutating-counter-loop.ts", "sumFirstN"],
      ["functions-mutating-counter-loop.ts", "sumIfPositiveFirstN"],
      ["functions-mutating-bounded-while.ts", "sumFirstNWhile"],
      ["functions-mutating-fixed-point-while.ts", "adjustBalanceTo"],
    ];

    for (const [file, target] of cases) {
      const sourceFile = createSourceFile(
        resolve(CONSTRUCTS_FIXTURE_DIR, file),
      );
      await assert.doesNotReject(
        buildDocumentFromSourceFile(sourceFile, target, { policy: "opaque" }),
      );
    }
  });

  it("opaque operand inside a synthetic loop binop builds without crashing (regression)", async () => {
    // `a.total += extra` (extra: any) is lowered by the generic bare-while
    // mutation path, which synthesizes an `a.total + extra` binop with no
    // source file. Under policy "opaque", `extra` is an OpaqueValue, so the
    // synthetic node reaches `sourceRefForNode` with an opaque operand — the
    // case that used to dereference an undefined source file. The build must
    // not crash, and opacity must propagate (an Opaque domain is emitted).
    const sourceFile = createSourceFile(
      resolve(OPAQUE_FIXTURE_DIR, "functions-mutating-loop-opaque.ts"),
    );
    const doc = await buildDocumentFromSourceFile(
      sourceFile,
      "addDynamicWhile",
      { policy: "opaque" },
    );
    const output = emitDocument(doc);

    assert.match(output, /Opaque/u);
  });
});
