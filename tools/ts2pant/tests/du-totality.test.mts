import assert from "node:assert/strict";
import { describe, it } from "node:test";
import {
  createSourceFileFromSource,
  extractAllTypes,
  getChecker,
} from "../src/extract.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import {
  buildDiscriminatedUnionTotalityAssertion,
  cellEmitSynth,
  cellRegisterDiscriminatedUnion,
  type DiscriminatedUnionSynthEntry,
  detectDiscriminatedUnion,
  IntStrategy,
  newSynthCell,
} from "../src/translate-types.js";

function extractFirstAlias(source: string) {
  const sourceFile = createSourceFileFromSource(source);
  const extracted = extractAllTypes(sourceFile);
  const checker = getChecker(sourceFile);
  const alias = extracted.aliases[0];
  assert.ok(alias);
  return { alias, checker };
}

describe("du-totality", () => {
  it("builder emits disjunction over all variant literals", async () => {
    await loadAst();
    const ast = getAst();
    const entry: DiscriminatedUnionSynthEntry = {
      domain: "Shape",
      binder: "s",
      discriminant: "kind",
      discriminantType: "String",
      variants: [
        { key: "string:circle", literal: { kind: "string", value: "circle" } },
        { key: "string:square", literal: { kind: "string", value: "square" } },
      ],
      fields: [],
    };

    const assertion = buildDiscriminatedUnionTotalityAssertion(entry, ast);
    if (assertion.kind !== "assertion") {
      throw new Error("expected assertion");
    }
    assert.equal(
      ast.strExpr(ast.forall(assertion.quantifiers, [], assertion.body)),
      'all s: Shape | shape--kind s = "circle" or shape--kind s = "square"',
    );
  });

  it("builder returns unsupported for empty variant lists", async () => {
    await loadAst();
    const assertion = buildDiscriminatedUnionTotalityAssertion({
      domain: "Shape",
      binder: "s",
      discriminant: "kind",
      discriminantType: "String",
      variants: [],
      fields: [],
    });

    assert.deepEqual(assertion, {
      kind: "unsupported",
      reason: "discriminated union Shape has no variants",
    });
  });

  it("builder returns unsupported for unrenderable discriminant literals", async () => {
    await loadAst();
    const assertion = buildDiscriminatedUnionTotalityAssertion({
      domain: "Shape",
      binder: "s",
      discriminant: "kind",
      discriminantType: "Nat0",
      variants: [{ key: "number:-1", literal: { kind: "number", value: -1 } }],
      fields: [],
    });

    assert.deepEqual(assertion, {
      kind: "unsupported",
      reason: "unsupported discriminant literal in Shape",
    });
  });

  it("cellEmitSynth returns one assertion per newly-emitted DU domain", async () => {
    await loadAst();
    const { alias, checker } = extractFirstAlias(`
      type Shape =
        | { kind: "circle"; r: number }
        | { kind: "square"; s: number };
    `);
    const detected = detectDiscriminatedUnion(alias.type, checker);
    assert.ok(detected);
    const cell = newSynthCell();

    assert.equal(
      cellRegisterDiscriminatedUnion(
        cell,
        detected,
        checker,
        IntStrategy,
        "Shape",
      ),
      "Shape",
    );

    const first = cellEmitSynth(cell);
    assert.equal(first.assertions.length, 1);
    assert.equal(first.assertions[0]!.kind, "assertion");
    assert.equal(cellEmitSynth(cell).assertions.length, 0);
  });
});
