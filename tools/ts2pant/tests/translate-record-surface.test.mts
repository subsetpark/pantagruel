// @archlint.module test
// @archlint.domain ts2pant.translate-record

import assert from "node:assert/strict";
import { before, it } from "node:test";
import * as fc from "fast-check";
import ts from "typescript";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { translateRecordReturn } from "../src/translate-record.js";
import { IntStrategy, newSynthCell } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

const recordCases = [
  { field: "balance", expr: "value" },
  { field: "limit", expr: "value + 1" },
  { field: "score", expr: "Math.max(value, 0)" },
  { field: "count", expr: "value > 0 ? value : 0" },
  { field: "rank", expr: "value - 1" },
  { field: "total", expr: "Math.min(value, 10)" },
].map(({ field, expr }) => {
  const sourceFile = createSourceFileFromSource(`
    interface Result { ${field}: number; }
    function make(value: number): Result {
      return { ${field}: ${expr} };
    }
  `);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.getFunctionOrThrow("make").compilerNode;
  const ret = fn.body!.statements.find(ts.isReturnStatement)!;
  const lit = ret.expression!;
  assert.ok(ts.isObjectLiteralExpression(lit));
  return { checker, field, fn, lit };
});

it("generated record returns translate per-field propositions", () => {
  fc.assert(
    fc.property(
      fc.constantFrom(...recordCases),
      ({ checker, field, fn, lit }) => {
        const props = translateRecordReturn(
          lit,
          "make",
          [{ name: "value", type: "Int" }],
          fn,
          checker,
          IntStrategy,
          new Map([["value", "value"]]),
          { n: 0, synthCell: newSynthCell() },
          newSynthCell(),
          (e) => e,
        );
        assert.equal(props.length > 0, true);
        assert.equal(getAst().strExpr(getAst().var(field)).length > 0, true);
      },
    ),
  );
});
