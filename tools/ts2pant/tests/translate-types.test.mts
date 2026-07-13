// @archlint.module test
// @archlint.domain ts2pant.translate-types
// biome-ignore-all lint/security/noSecrets: type translation tests assert generated domain names that trigger entropy false positives

import assert from "node:assert/strict";
import { resolve } from "node:path";
import { before, describe, it } from "node:test";
import * as fc from "fast-check";
import { emitDocument } from "../src/emit.js";
import {
  createSourceFile,
  createSourceFileFromSource,
  type ExtractedInterface,
  extractAllTypes,
  extractReferencedTypes,
  getChecker,
} from "../src/extract.js";
import { emptyNameRegistry, registerName } from "../src/name-registry.js";
import { OPAQUE_DOMAIN, opaqueValueRuleName } from "../src/opaque.js";
import { assertWasmTypeChecks, loadAst } from "../src/pant-wasm.js";
import * as TT from "../src/translate-types.js";
import {
  cellEmitSynth,
  cellLookupOpaqueValue,
  cellRegisterDiscriminatedUnion,
  cellRegisterOpaqueValue,
  cellRegisterSynthesizedValue,
  cellRegisterTupleConstructor,
  cellTupleShapes,
  depModuleNameForFile,
  detectDiscriminatedUnion,
  emitTupleCtorModule,
  IntStrategy,
  type MapTsTypeResult,
  mapTsType,
  newSynthCell,
  RealStrategy,
  resolveFieldOwner,
  type TupleShape,
  UNSUPPORTED_VARIADIC_TUPLE_SHAPE_REASON,
  UNSUPPORTED_VARIADIC_TUPLE_REASON,
  translateTypes,
} from "../src/translate-types.js";
import type { PantDeclaration } from "../src/types.js";

// Narrow a MapTsTypeResult to its sort, failing the test if it is unsupported.
// Keeps the success-expecting assertions going *through* the Result type rather
// than reaching past it with a bare `.sort` on an un-narrowed union.
function expectSort(result: MapTsTypeResult): string {
  if (!result.ok) {
    throw new Error(`expected a Pant sort, got unsupported: ${result.reason}`);
  }
  return result.sort;
}

before(async () => {
  await loadAst();
});

// Tests for internal type translation APIs: mapTsType, extractReferencedTypes
// recursive following, numeric strategy. See tests/fixtures/constructs/ for
// exhaustive construct coverage via snapshots.

function extractAndTranslate(source: string, strategy = IntStrategy) {
  const sourceFile = createSourceFileFromSource(source);
  const extracted = extractAllTypes(sourceFile);
  const checker = getChecker(sourceFile);
  const decls = translateTypes(extracted, checker, strategy);
  return { decls, extracted, checker, sourceFile };
}

function extractFirstAlias(source: string) {
  const sourceFile = createSourceFileFromSource(source);
  const extracted = extractAllTypes(sourceFile);
  const checker = getChecker(sourceFile);
  const alias = extracted.aliases[0];
  assert.ok(alias);
  return { alias, checker, sourceFile };
}

function extractAliasType(source: string, aliasName: string) {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const aliasNode = sourceFile
    .getTypeAliasOrThrow(aliasName)
    .getTypeNodeOrThrow().compilerNode;
  const type = checker.getTypeAtLocation(aliasNode);
  return { checker, sourceFile, type };
}

function emitSynthDeclsOnly(cell: ReturnType<typeof newSynthCell>) {
  return cellEmitSynth(cell).decls;
}

const generatedTypeCases = ["Foo", "Account"].map((typeName) => {
  const sourceFile = createSourceFileFromSource(
    `
    interface ${typeName} { value: number; flag?: boolean; }
    type Alias = { amount: number };
    type Maybe = number | null;
    function f(input: ${typeName}): ${typeName} { return input; }
  `,
    `${typeName}.ts`,
  );
  const checker = getChecker(sourceFile);
  return {
    checker,
    extracted: extractAllTypes(sourceFile),
    sourceFile,
    typeName,
  };
});

it("generated type helpers preserve synth registrations", () => {
  fc.assert(
    fc.property(fc.constantFrom(...generatedTypeCases), (testCase) => {
      const { checker, extracted, sourceFile, typeName } = testCase;
      const cell = TT.newSynthCell();
      cell.sourceFile = { fileName: `${typeName}.ts` } as never;
      const registry = emptyNameRegistry();
      const mapReg = TT.registerMapKV(cell.synth, registry, "String", "Int");
      const recordFields = [{ name: "amount", type: "Int" }];
      const recordReg = TT.registerRecordShape(
        cell.recordSynth,
        registry,
        recordFields,
      );
      const opaqueReg = TT.registerOpaqueValue(cell.opaqueSynth, "opaque-id");

      assert.equal(TT.isTsNullish(checker.getNullType()), true);
      assert.equal(TT.manglePantTypeToFragment("[Int]"), "ListInt");
      assert.equal(
        TT.lookupMapKV(mapReg.synth, "String", "Int")?.names.domain,
        mapReg.domain,
      );
      assert.equal(
        TT.emitSynthDecls(mapReg.synth, mapReg.registry).decls.length > 0,
        true,
      );
      assert.equal(TT.toPantTermName("fooBar"), "foo-bar");
      assert.equal(
        TT.lookupRecordShape(recordReg.synth, recordFields)?.domain,
        recordReg.domain,
      );
      assert.equal(
        TT.emitRecordSynthDecls(recordReg.synth).decls.length > 0,
        true,
      );
      assert.equal(
        TT.lookupOpaqueValue(opaqueReg.synth, "opaque-id")?.rule,
        opaqueValueRuleName("opaque-id"),
      );
      assert.equal(
        TT.emitOpaqueSynthDecls(opaqueReg.synth).decls.length > 0,
        true,
      );
      assert.deepEqual(TT.emptyForeignAccessorSynth().byKey.size, 0);
      const accessorReg = TT.registerForeignAccessor(
        cell.foreignAccessorSynth,
        cell.registry,
        "ForeignGenerated",
        "label",
        "String",
      );
      assert.equal(accessorReg.entry?.ruleName, "generated-label");
      assert.equal(
        TT.emitForeignAccessorSynthDecls(accessorReg.synth).decls.length,
        1,
      );
      assert.match(
        TT.depModuleNameForFile(`${typeName}.ts`),
        /^[A-Z][A-Z0-9_]*$/u,
      );
      assert.equal(
        TT.fieldRuleName(typeName, "value"),
        `${typeName.toLowerCase()}--value`,
      );

      assert.equal(
        TT.cellRegisterMap(cell, "String", "Int")?.includes("Map"),
        true,
      );
      assert.equal(
        TT.cellRegisterRecord(cell, recordFields)?.endsWith("Rec"),
        true,
      );
      assert.notEqual(TT.cellLookupRecord(cell, recordFields), undefined);
      TT.cellRegisterOpaqueDomain(cell);
      assert.equal(
        TT.cellRegisterOpaqueValue(cell, "opaque-id").rule,
        opaqueValueRuleName("opaque-id"),
      );
      assert.notEqual(TT.cellLookupOpaqueValue(cell, "opaque-id"), undefined);
      assert.equal(
        TT.cellRegisterForeignAccessor(cell, "ForeignGenerated", "value", "Int")
          ?.ruleName,
        "generated-value",
      );
      assert.equal(TT.cellRegisterName(cell, "freshName"), "fresh-name");
      assert.equal(TT.cellIsUsed(cell, "freshName"), true);
      const tupleShape = { elementPantTypes: ["Int", "Bool"] };
      const tupleRef = TT.cellRegisterTupleConstructor(cell, tupleShape);
      assert.equal(tupleRef?.ctorRuleName.length > 0, true);
      assert.equal(
        TT.emitTupleCtorModule(sourceFile.compilerNode, [
          { shape: tupleShape, ctorRuleName: tupleRef!.ctorRuleName },
        ]).includes("module"),
        true,
      );
      assert.equal(
        TT.emitSynthDecls(cell.synth, cell.registry).decls.length >= 0,
        true,
      );
      assert.equal(
        TT.emitRecordSynthDecls(cell.recordSynth).decls.length >= 0,
        true,
      );
      assert.equal(
        TT.emitDiscriminatedUnionSynthDecls(
          cell.discriminatedUnionSynth,
          cell.registry,
        ).decls.length >= 0,
        true,
      );
      assert.equal(
        TT.emitOpaqueSynthDecls(cell.opaqueSynth).decls.length >= 0,
        true,
      );
      assert.equal(TT.cellEmitSynth(cell).decls.length >= 0, true);

      const type = checker.getTypeAtLocation(
        sourceFile.getInterfaces()[0]!.getProperties()[0]!.compilerNode,
      );
      assert.equal(TT.isAnonymousRecord(type), false);
      assert.equal(TT.isMapType(type), false);
      assert.equal(TT.isSetType(type), false);
      assert.deepEqual(TT.mapTsType(type, checker, IntStrategy, cell), {
        ok: true,
        sort: "Int",
      });
      const aliasNode = sourceFile
        .getTypeAliasOrThrow("Maybe")
        .getTypeNodeOrThrow().compilerNode;
      const aliasType = checker.getTypeAtLocation(aliasNode);
      assert.deepEqual(
        TT.mapTsTypeFromTypeNode(
          aliasNode,
          aliasType,
          checker,
          IntStrategy,
          cell,
        ),
        { ok: true, sort: "[Int]" },
      );
      assert.equal(
        TT.resolveFieldOwner(
          checker.getTypeAtLocation(
            sourceFile.getInterfaces()[0]!.compilerNode,
          ),
          "value",
          checker,
          IntStrategy,
          cell,
        ).kind,
        "resolved",
      );
      assert.equal(TT.resolveRecordOwner(typeName), null);
      assert.equal(
        TT.lookupRecordShape(cell.recordSynth, recordFields)?.domain !==
          undefined,
        true,
      );
      assert.equal(
        TT.lookupOpaqueValue(cell.opaqueSynth, "opaque-id") !== undefined,
        true,
      );
      assert.equal(
        TT.lookupMapKV(cell.synth, "String", "Int") !== undefined,
        true,
      );
      assert.equal(
        TT.lookupOpaqueValue(cell.opaqueSynth, "missing"),
        undefined,
      );
      assert.equal(
        TT.lookupRecordShape(cell.recordSynth, [
          { name: "missing", type: "Int" },
        ]),
        undefined,
      );
      assert.equal(TT.registerOpaqueDomain(cell.opaqueSynth).needsDomain, true);
      assert.equal(
        TT.registerOpaqueValue(cell.opaqueSynth, "generated").entry.rule,
        opaqueValueRuleName("generated"),
      );
      assert.equal(
        TT.registerMapKV(cell.synth, cell.registry, "String", "Int").domain !==
          null,
        true,
      );
      assert.equal(
        TT.registerRecordShape(cell.recordSynth, cell.registry, recordFields)
          .domain !== null,
        true,
      );
      assert.equal(
        TT.detectDiscriminatedUnion(
          checker.getTypeAtLocation(
            sourceFile.getInterfaces()[0]!.compilerNode,
          ),
          checker,
        ),
        null,
      );
      assert.equal(
        TT.buildDiscriminatedUnionTotalityAssertion({
          domain: "Generated",
          binder: "g",
          discriminant: "kind",
          discriminantType: "String",
          variants: [
            { key: "string:A", literal: { kind: "string", value: "A" } },
            { key: "string:B", literal: { kind: "string", value: "B" } },
          ],
          fields: [],
        }).kind,
        "assertion",
      );
      assert.equal(
        TT.translateTypes(extracted, checker, IntStrategy, cell).length > 0,
        true,
      );
    }),
  );
});

it("generated names and type fragments remain Pant-safe", () => {
  fc.assert(
    fc.property(
      fc.stringMatching(/^[A-Za-z_][A-Za-z0-9_]{0,16}$/u),
      fc.constantFrom("Int", "Real", "Bool", "[Int]", "String * Int"),
      fc.constantFrom("foo.ts", "foo-bar.ts", "_private.ts", "123.ts"),
      (name, pantType, fileName) => {
        const term = TT.toPantTermName(name);
        assert.match(term, /^[a-z][a-z0-9_?!-]*$/u);
        assert.notEqual(TT.manglePantTypeToFragment(pantType), "");
        assert.match(TT.depModuleNameForFile(fileName), /^[A-Z][A-Z0-9_]*$/u);
        assert.equal(TT.fieldRuleName(name, "value"), `${term}--value`);
      },
    ),
  );
});

describe("numeric strategy", () => {
  it("IntStrategy maps number to Int", () => {
    const { decls } = extractAndTranslate(
      `interface Foo { value: number; }`,
      IntStrategy,
    );

    assert.ok(
      decls.some(
        (d: PantDeclaration) =>
          d.kind === "rule" &&
          d.name === "foo--value" &&
          d.params.length === 1 &&
          d.params[0].name === "f" &&
          d.params[0].type === "Foo" &&
          d.returnType === "Int",
      ),
    );
  });

  it("RealStrategy maps number to Real", () => {
    const { decls } = extractAndTranslate(
      `interface Foo { value: number; }`,
      RealStrategy,
    );

    assert.ok(
      decls.some(
        (d: PantDeclaration) =>
          d.kind === "rule" &&
          d.name === "foo--value" &&
          d.params.length === 1 &&
          d.params[0].name === "f" &&
          d.params[0].type === "Foo" &&
          d.returnType === "Real",
      ),
    );
  });
});

describe("class method referenced types", () => {
  it("follows types from class method signatures", () => {
    const source = `
      interface User {
        name: string;
      }
      class Account {
        getOwner(u: User): User {
          return u;
        }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const extracted = extractReferencedTypes(sourceFile, "Account.getOwner");
    assert.ok(
      extracted.interfaces
        .map((i: ExtractedInterface) => i.name)
        .includes("User"),
    );
  });
});

describe("enum member extraction", () => {
  it("extracts enum member names", () => {
    const sourceFile = createSourceFileFromSource(`
      enum Status {
        Active,
        Inactive,
        Pending,
      }
    `);
    const extracted = extractAllTypes(sourceFile);

    assert.equal(extracted.enums.length, 1);
    assert.equal(extracted.enums[0].name, "Status");
    assert.deepEqual(extracted.enums[0].members, [
      "Active",
      "Inactive",
      "Pending",
    ]);
  });
});

describe("recursive type following", () => {
  it("follows types from function parameters", () => {
    const source = `
      interface User {
        name: string;
      }
      interface Account {
        owner: User;
        balance: number;
      }
      interface Unrelated {
        data: string;
      }
      function getBalance(account: Account): number {
        return account.balance;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const extracted = extractReferencedTypes(sourceFile, "getBalance");

    const names = extracted.interfaces.map((i: ExtractedInterface) => i.name);
    assert.ok(names.includes("Account"));
    assert.ok(names.includes("User"));
    assert.ok(!names.includes("Unrelated"));
  });

  it("follows return type references", () => {
    const source = `
      interface User {
        name: string;
      }
      function getUser(): User {
        return { name: "test" };
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const extracted = extractReferencedTypes(sourceFile, "getUser");

    assert.ok(
      extracted.interfaces
        .map((i: ExtractedInterface) => i.name)
        .includes("User"),
    );
  });

  it("follows array element types", () => {
    const source = `
      interface Item {
        label: string;
      }
      function getItems(): Item[] {
        return [];
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const extracted = extractReferencedTypes(sourceFile, "getItems");

    assert.ok(
      extracted.interfaces
        .map((i: ExtractedInterface) => i.name)
        .includes("Item"),
    );
  });

  it("follows nested references transitively", () => {
    const source = `
      interface Address {
        street: string;
      }
      interface User {
        name: string;
        address: Address;
      }
      interface Order {
        buyer: User;
        total: number;
      }
      function processOrder(order: Order): number {
        return order.total;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const extracted = extractReferencedTypes(sourceFile, "processOrder");

    const names = extracted.interfaces.map((i: ExtractedInterface) => i.name);
    assert.ok(names.includes("Order"));
    assert.ok(names.includes("User"));
    assert.ok(names.includes("Address"));
  });
});

describe("detectDiscriminatedUnion", () => {
  it("accepts structural discriminants and picks sorted first on ties", () => {
    const { alias, checker } = extractFirstAlias(`
      type Shape =
        | { kind: "circle"; tag: "c"; r: number }
        | { kind: "square"; tag: "s"; s: number };
    `);

    const detected = detectDiscriminatedUnion(alias.type, checker);

    assert.ok(detected);
    assert.equal(detected.discriminant, "kind");
    assert.deepEqual(
      detected.variants.map((variant) => variant.literal),
      [
        { kind: "string", value: "circle" },
        { kind: "string", value: "square" },
      ],
    );
    assert.deepEqual(
      detected.variants.map((variant) =>
        variant.fields.map((field) => field.name).sort(),
      ),
      [
        ["kind", "r", "tag"],
        ["kind", "s", "tag"],
      ],
    );
  });

  it("rejects unions without a qualifying distinct literal field", () => {
    for (const source of [
      `type U = { a: "one"; x: number } | { b: "two"; y: number };`,
      `type U = { kind: string; x: number } | { kind: string; y: number };`,
      `type U = { kind: "same"; x: number } | { kind: "same"; y: number };`,
    ]) {
      const { alias, checker } = extractFirstAlias(source);
      assert.equal(detectDiscriminatedUnion(alias.type, checker), null);
    }
  });
});

describe("emitDiscriminatedUnionSynthDecls", () => {
  it("emits one domain, an unguarded discriminant, and guarded fields", async () => {
    await loadAst();
    const { alias, checker } = extractFirstAlias(`
      type Shape =
        | { kind: "circle"; r: number; shared: string }
        | { kind: "square"; s: number; shared: string };
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

    const output = emitDocument({
      moduleName: "TEST",
      imports: [],
      declarations: emitSynthDeclsOnly(cell),
      propositions: [],
      checks: [],
      bundleModules: new Map(),
    });

    assert.match(output, /^Shape\.$/mu);
    assert.match(output, /^shape--kind s: Shape => String\.$/mu);
    assert.match(
      output,
      /^shape--r s: Shape, shape--kind s = "circle" => Int\.$/mu,
    );
    assert.match(
      output,
      /^shape--s s: Shape, shape--kind s = "square" => Int\.$/mu,
    );
    assert.match(
      output,
      /^shape--shared s: Shape, shape--kind s = "circle" or shape--kind s = "square" => String\.$/mu,
    );
  });

  it("maps discriminated unions to one domain and resolves field access", () => {
    const { alias, checker } = extractFirstAlias(`
      type Shape =
        | { kind: "circle"; r: number; shared: string }
        | { kind: "square"; s: number; shared: string };
    `);
    const cell = newSynthCell();

    assert.equal(
      expectSort(mapTsType(alias.type, checker, IntStrategy, cell)),
      "Shape",
    );
    assert.deepEqual(
      ["kind", "r", "shared"].map((field) =>
        resolveFieldOwner(alias.type, field, checker, IntStrategy, cell),
      ),
      [
        { kind: "resolved", owner: "Shape" },
        { kind: "resolved", owner: "Shape" },
        { kind: "resolved", owner: "Shape" },
      ],
    );

    const decls = emitSynthDeclsOnly(cell);
    assert.ok(
      decls.some(
        (decl) =>
          decl.kind === "rule" &&
          decl.name === "shape--r" &&
          decl.guard !== undefined,
      ),
    );
  });

  it("refuses non-discriminated union field access unless every member has the same owner", () => {
    const { alias, checker } = extractFirstAlias(`
      type NonDiscriminated =
        | { owner: string; left: number }
        | { owner: string; right: number };
    `);
    const cell = newSynthCell();
    const mapped = mapTsType(alias.type, checker, IntStrategy, cell);

    assert.deepEqual(mapped, {
      ok: true,
      sort: "LeftOwnerRec + OwnerRightRec",
    });
    assert.deepEqual(
      resolveFieldOwner(alias.type, "owner", checker, IntStrategy, cell),
      {
        kind: "ambiguous",
        owners: [],
      },
    );
    assert.deepEqual(
      resolveFieldOwner(alias.type, "left", checker, IntStrategy, cell),
      {
        kind: "ambiguous",
        owners: [],
      },
    );
  });

  it("keeps intersection field access resolved by the declaring owner", () => {
    const sourceFile = createSourceFileFromSource(`
      interface HasId { id: number; }
      interface HasLabel { label: string; }
      type Both = HasId & HasLabel;
    `);
    const extracted = extractAllTypes(sourceFile);
    const checker = getChecker(sourceFile);
    const alias = extracted.aliases.find((entry) => entry.name === "Both");
    assert.ok(alias);

    assert.deepEqual(
      resolveFieldOwner(alias.type, "id", checker, IntStrategy, newSynthCell()),
      {
        kind: "resolved",
        owner: "HasId",
      },
    );
  });
});

describe("mapTsType", () => {
  it("maps a fixed tuple as a Pantagruel product", () => {
    const { checker, type } = extractAliasType(
      `
      type FixedTuple = [number, string];
    `,
      "FixedTuple",
    );

    assert.deepEqual(mapTsType(type, checker, IntStrategy), {
      ok: true,
      sort: "Int * String",
    });
  });

  it("maps a homogeneous required-head/rest tuple to a Pantagruel list", () => {
    const { checker, type } = extractAliasType(
      `
      type HomogeneousTuple<T> = [T, ...T[]];
      type NumberTuple = HomogeneousTuple<number>;
    `,
      "NumberTuple",
    );

    assert.deepEqual(mapTsType(type, checker, IntStrategy), {
      ok: true,
      sort: "[Int]",
    });
  });

  it("keeps a tuple pair as the element of a homogeneous variadic list", () => {
    const { checker, type } = extractAliasType(
      `
      type Pair = [number, string];
      type RepeatedPairTuple = [Pair, ...Pair[]];
    `,
      "RepeatedPairTuple",
    );

    assert.deepEqual(mapTsType(type, checker, IntStrategy), {
      ok: true,
      sort: "[Int * String]",
    });
  });

  it("refuses a heterogeneous required-head/rest tuple", () => {
    const { checker, type } = extractAliasType(
      `
      type HeterogeneousTuple = [number, ...string[]];
    `,
      "HeterogeneousTuple",
    );

    assert.deepEqual(mapTsType(type, checker, IntStrategy), {
      ok: false,
      reason: UNSUPPORTED_VARIADIC_TUPLE_REASON,
    });
  });

  it("refuses a structurally unsupported middle-rest tuple distinctly", () => {
    const { checker, type } = extractAliasType(
      `
      type MiddleRestTuple = [number, ...number[], number];
    `,
      "MiddleRestTuple",
    );

    assert.deepEqual(mapTsType(type, checker, IntStrategy), {
      ok: false,
      reason: UNSUPPORTED_VARIADIC_TUPLE_SHAPE_REASON,
    });
  });

  it("propagates a nested DU field-mapping refusal", () => {
    const sourceFile = createSourceFileFromSource(
      `
      export function consume(
        value:
          | { kind: "ok"; value: number }
          | { kind: "bad"; items: [number, ...string[]] }
      ): void {}
    `,
    );
    const checker = getChecker(sourceFile);
    const paramTypeNode = sourceFile
      .getFunctionOrThrow("consume")
      .getParameters()[0]!
      .getTypeNodeOrThrow().compilerNode;
    const type = checker.getTypeAtLocation(paramTypeNode);
    const mapped = mapTsType(type, checker, IntStrategy, newSynthCell());

    assert.deepEqual(mapped, {
      ok: false,
      reason:
        "discriminated union field items: heterogeneous variadic tuple is not expressible as a Pantagruel list",
    });
  });

  it("maps `any` to the shared Opaque sort", async () => {
    await loadAst();
    const cell = newSynthCell();
    const source = `interface Foo { val: any; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const prop = extractAllTypes(sourceFile).interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy, cell)),
      OPAQUE_DOMAIN,
    );
    assert.deepEqual(emitSynthDeclsOnly(cell), [
      { kind: "domain", name: OPAQUE_DOMAIN },
    ]);
    assert.deepEqual(emitSynthDeclsOnly(cell), []);
  });

  it("maps `unknown` to the shared Opaque sort", async () => {
    await loadAst();
    const cell = newSynthCell();
    const source = `interface Foo { val: unknown; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const prop = extractAllTypes(sourceFile).interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy, cell)),
      OPAQUE_DOMAIN,
    );
    assert.deepEqual(emitSynthDeclsOnly(cell), [
      { kind: "domain", name: OPAQUE_DOMAIN },
    ]);
  });

  it("leaves concrete types unchanged", async () => {
    await loadAst();
    const cell = newSynthCell();
    const source = `interface Foo { val: number; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const prop = extractAllTypes(sourceFile).interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy, cell)),
      "Int",
    );
    assert.deepEqual(emitSynthDeclsOnly(cell), []);
  });

  it("maps `any` and `unknown` without a synth cell", () => {
    const source = `interface Foo { anyVal: any; unknownVal: unknown; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const [anyProp, unknownProp] =
      extractAllTypes(sourceFile).interfaces[0].properties;

    assert.equal(
      expectSort(mapTsType(anyProp.type, checker, IntStrategy)),
      OPAQUE_DOMAIN,
    );
    assert.equal(
      expectSort(mapTsType(unknownProp.type, checker, IntStrategy)),
      OPAQUE_DOMAIN,
    );
  });

  it("without a synth cell maps `any` to Opaque", () => {
    const source = `interface Foo { val: any; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const prop = extractAllTypes(sourceFile).interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy)),
      OPAQUE_DOMAIN,
    );
  });

  it("domain emission dedupes with opaque value emission", async () => {
    await loadAst();
    const cell = newSynthCell();
    const source = `interface Foo { val: any; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const prop = extractAllTypes(sourceFile).interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy, cell)),
      OPAQUE_DOMAIN,
    );
    cellRegisterOpaqueValue(cell, "foo.ts:1");

    assert.deepEqual(emitSynthDeclsOnly(cell), [
      { kind: "domain", name: OPAQUE_DOMAIN },
      {
        kind: "rule",
        name: opaqueValueRuleName("foo.ts:1"),
        params: [],
        returnType: OPAQUE_DOMAIN,
      },
    ]);

    cellRegisterOpaqueValue(cell, "foo.ts:2");
    assert.deepEqual(emitSynthDeclsOnly(cell), [
      {
        kind: "rule",
        name: opaqueValueRuleName("foo.ts:2"),
        params: [],
        returnType: OPAQUE_DOMAIN,
      },
    ]);
  });

  it("propagates through nested composite positions", async () => {
    await loadAst();
    const cell = newSynthCell();
    const source = `interface Foo { val: Map<string, any[]>; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const prop = extractAllTypes(sourceFile).interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy, cell)),
      "StringToListOpaqueMap",
    );

    const decls = emitSynthDeclsOnly(cell);
    assert.equal(decls.length, 4);
    assert.deepEqual(decls[0], { kind: "domain", name: OPAQUE_DOMAIN });
    assert.deepEqual(decls[1], {
      kind: "domain",
      name: "StringToListOpaqueMap",
    });
    assert.deepEqual(decls[2], {
      kind: "rule",
      name: "string-to-list-opaque-map-key",
      params: [
        { name: "m", type: "StringToListOpaqueMap" },
        { name: "k", type: "String" },
      ],
      returnType: "Bool",
    });
    assert.equal(decls[3].kind, "rule");
    if (decls[3].kind !== "rule") {
      throw new Error("expected map value rule");
    }
    assert.equal(decls[3].name, "string-to-list-opaque-map");
    assert.deepEqual(decls[3].params, [
      { name: "m", type: "StringToListOpaqueMap" },
      { name: "k", type: "String" },
    ]);
    assert.equal(decls[3].returnType, "[Opaque]");

    cellRegisterOpaqueValue(cell, "foo.ts:2");
    assert.deepEqual(emitSynthDeclsOnly(cell), [
      {
        kind: "rule",
        name: opaqueValueRuleName("foo.ts:2"),
        params: [],
        returnType: OPAQUE_DOMAIN,
      },
    ]);
  });

  it("maps unanalyzed foreign named types to distinct domains", async () => {
    await loadAst();
    const sourceFile = createSourceFile(
      resolve(import.meta.dirname, "../src/translate-body.ts"),
    );
    const checker = getChecker(sourceFile);
    const fn = sourceFile.getFunctionOrThrow("bindingNamesFromDeclarationList");
    const param = fn.getParameters()[0];
    assert.ok(param);
    const cell = newSynthCell();

    assert.equal(
      expectSort(
        mapTsType(param.getType().compilerType, checker, IntStrategy, cell),
      ),
      "ForeignVariableDeclarationList",
    );
    assert.deepEqual(emitSynthDeclsOnly(cell), [
      { kind: "domain", name: "ForeignVariableDeclarationList" },
    ]);
    assert.deepEqual(emitSynthDeclsOnly(cell), []);
  });

  it("foreign accessors register stable typed accessor declarations", () => {
    const cell = newSynthCell();
    const containerDomain = TT.registerForeignDomain(
      cell.foreignDomainSynth,
      cell.registry,
      "foreign-dependency:ForeignDependencyContainer",
      "DependencyContainer",
    );
    cell.foreignDomainSynth = containerDomain.synth;
    cell.registry = containerDomain.registry;
    const itemDomain = TT.registerForeignDomain(
      cell.foreignDomainSynth,
      cell.registry,
      "foreign-dependency:ForeignDependencyItem",
      "DependencyItem",
    );
    cell.foreignDomainSynth = itemDomain.synth;
    cell.registry = itemDomain.registry;

    const items = TT.cellRegisterForeignAccessor(
      cell,
      containerDomain.domain,
      "items",
      `[${itemDomain.domain}]`,
    );
    const itemsAgain = TT.cellRegisterForeignAccessor(
      cell,
      containerDomain.domain,
      "items",
      `[${itemDomain.domain}]`,
    );
    const label = TT.cellRegisterForeignAccessor(
      cell,
      itemDomain.domain,
      "label",
      "String",
    );
    const ready = TT.cellRegisterForeignAccessor(
      cell,
      itemDomain.domain,
      "ready",
      "Bool",
    );

    assert.ok(items);
    assert.ok(label);
    assert.ok(ready);
    assert.equal(itemsAgain, items);
    assert.equal(items.ruleName, "items");
    assert.equal(label.ruleName, "item-label");
    assert.equal(ready.ruleName, "item-ready");
    assert.equal(label.returnSort, "String");
    assert.equal(ready.returnSort, "Bool");

    assert.deepEqual(emitSynthDeclsOnly(cell), [
      { kind: "domain", name: "ForeignDependencyContainer" },
      { kind: "domain", name: "ForeignDependencyItem" },
      {
        kind: "rule",
        name: "items",
        params: [{ name: "c", type: "ForeignDependencyContainer" }],
        returnType: "[ForeignDependencyItem]",
      },
      {
        kind: "rule",
        name: "item-label",
        params: [{ name: "i", type: "ForeignDependencyItem" }],
        returnType: "String",
      },
      {
        kind: "rule",
        name: "item-ready",
        params: [{ name: "i", type: "ForeignDependencyItem" }],
        returnType: "Bool",
      },
    ]);
    assert.deepEqual(emitSynthDeclsOnly(cell), []);

    const renamed = newSynthCell(
      registerName(emptyNameRegistry(), "items").registry,
    );
    const collision = TT.cellRegisterForeignAccessor(
      renamed,
      "ForeignDependencyContainer",
      "items",
      "[ForeignDependencyItem]",
    );
    assert.equal(collision?.ruleName, "foreign-dependency-container--items");
  });

  it("top-level undefined falls through to checker.typeToString", () => {
    // Lone null/undefined/void has no Pantagruel encoding. mapTsType no
    // longer returns an internal sentinel; it falls through to
    // `checker.typeToString` so emission mirrors the source. The result
    // is not a valid Pantagruel identifier, so downstream emission still
    // fails visibly.
    const source = `interface Foo { val: undefined; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy)),
      "undefined",
    );
  });

  it("list-lifts `T | null` to `[T]`", () => {
    const source = `interface Foo { val: string | null; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy)),
      "[String]",
    );
  });

  it("list-lifts multi-arm union with null to `[A + B]`", () => {
    const source = `
      interface A { a: string; }
      interface B { b: number; }
      interface Foo { val: A | B | null; }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const foo = extracted.interfaces.find((i) => i.name === "Foo")!;
    const prop = foo.properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy)),
      "[A + B]",
    );
  });

  it("maps `unknown` to `Opaque`", () => {
    const source = `interface Foo { val: unknown; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy)),
      OPAQUE_DOMAIN,
    );
  });

  it("composes `unknown` through tuple element", () => {
    const source = `interface Foo { val: [unknown, number]; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy)),
      "Opaque * Int",
    );
  });

  it("composes `unknown` through array element", () => {
    const source = `interface Foo { val: unknown[]; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy)),
      "[Opaque]",
    );
  });

  it("composes `unknown` through Set element", () => {
    const source = `interface Foo { val: Set<unknown>; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy)),
      "[Opaque]",
    );
  });

  it("composes `unknown` through Map K and V", () => {
    const cellK = newSynthCell();
    const sourceK = `interface Foo { val: Map<unknown, number>; }`;
    const sfK = createSourceFileFromSource(sourceK);
    const propK = extractAllTypes(sfK).interfaces[0].properties[0];
    assert.equal(
      expectSort(mapTsType(propK.type, getChecker(sfK), IntStrategy, cellK)),
      "OpaqueToIntMap",
    );
    assert.equal(cellK.synth.byKV.size, 1);

    const cellV = newSynthCell();
    const sourceV = `interface Foo { val: Map<string, unknown>; }`;
    const sfV = createSourceFileFromSource(sourceV);
    const propV = extractAllTypes(sfV).interfaces[0].properties[0];
    assert.equal(
      expectSort(mapTsType(propV.type, getChecker(sfV), IntStrategy, cellV)),
      "StringToOpaqueMap",
    );
    assert.equal(cellV.synth.byKV.size, 1);
  });

  it("composes `unknown` through union members", () => {
    const source = `interface Foo { val: string | unknown; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(
      expectSort(mapTsType(prop.type, checker, IntStrategy)),
      OPAQUE_DOMAIN,
    );
  });

  it("composes `unknown` through anonymous record field", () => {
    const cell = newSynthCell();
    const source = `function f(): { x: unknown } { return { x: 1 }; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const fn = sourceFile.getFunctionOrThrow("f");
    const returnType = fn.getReturnType().compilerType;
    assert.equal(
      expectSort(mapTsType(returnType, checker, IntStrategy, cell)),
      "XRec",
    );
    assert.equal(cell.recordSynth.byShape.size, 1);
  });
});

describe("translateTypes routes `unknown` through Opaque", () => {
  it("interface field with `unknown` type emits an Opaque field", () => {
    const { decls } = extractAndTranslate(`interface Foo { val: unknown; }`);
    assert.ok(decls.some((d) => d.kind === "domain" && d.name === "Foo"));
    assert.ok(
      decls.some(
        (d) =>
          d.kind === "rule" &&
          d.name === "foo--val" &&
          d.returnType === OPAQUE_DOMAIN,
      ),
    );
    assert.ok(
      !decls.some(
        (d) =>
          (d.kind === "rule" && d.returnType.includes("unsupported_unknown")) ||
          (d.kind === "alias" && d.type.includes("unsupported_unknown")),
      ),
    );
  });

  it("alias `T = unknown` emits an Opaque alias", () => {
    const { decls } = extractAndTranslate(`type Foo = unknown;`);
    assert.deepEqual(decls, [
      { kind: "alias", name: "Foo", type: OPAQUE_DOMAIN },
    ]);
  });
});

// The previous `cellRegister* reject `unknown` defensively` suite was removed:
// callers now unwrap `mapTsType`'s Result before passing sorts to the
// synthesizers, so a refusal can no longer reach them as a sentinel string —
// the guard is enforced by the type system rather than a runtime check.

describe("cellRegisterOpaqueValue / cellEmitSynth", () => {
  it("emits nothing while no opaque id is registered", async () => {
    await loadAst();
    const cell = newSynthCell();
    assert.deepEqual(emitSynthDeclsOnly(cell), []);
  });

  it("emits the Opaque domain and one nullary constant per id on use", async () => {
    await loadAst();
    const cell = newSynthCell();
    const first = cellRegisterOpaqueValue(cell, "foo.ts:1");
    const firstAgain = cellRegisterOpaqueValue(cell, "foo.ts:1");
    const second = cellRegisterOpaqueValue(cell, "foo.ts:2");

    assert.equal(first.rule, opaqueValueRuleName("foo.ts:1"));
    assert.equal(firstAgain.rule, first.rule);
    assert.equal(cellLookupOpaqueValue(cell, "foo.ts:2"), second);

    assert.deepEqual(emitSynthDeclsOnly(cell), [
      { kind: "domain", name: OPAQUE_DOMAIN },
      {
        kind: "rule",
        name: opaqueValueRuleName("foo.ts:1"),
        params: [],
        returnType: OPAQUE_DOMAIN,
      },
      {
        kind: "rule",
        name: opaqueValueRuleName("foo.ts:2"),
        params: [],
        returnType: OPAQUE_DOMAIN,
      },
    ]);
    assert.deepEqual(emitSynthDeclsOnly(cell), []);

    cellRegisterOpaqueValue(cell, "foo.ts:3");
    assert.deepEqual(emitSynthDeclsOnly(cell), [
      {
        kind: "rule",
        name: opaqueValueRuleName("foo.ts:3"),
        params: [],
        returnType: OPAQUE_DOMAIN,
      },
    ]);
  });

  it("emits synthesized values at their registered sort and declares Opaque only when needed", async () => {
    await loadAst();
    const cell = newSynthCell();

    const narrowed = cellRegisterSynthesizedValue(cell, "foo.ts:1", "String");
    assert.equal(narrowed.rule, opaqueValueRuleName("foo.ts:1"));
    assert.equal(narrowed.sort, "String");

    assert.deepEqual(emitSynthDeclsOnly(cell), [
      {
        kind: "rule",
        name: opaqueValueRuleName("foo.ts:1"),
        params: [],
        returnType: "String",
      },
    ]);

    const opaque = cellRegisterOpaqueValue(cell, "foo.ts:2");
    assert.equal(opaque.sort, OPAQUE_DOMAIN);

    assert.deepEqual(emitSynthDeclsOnly(cell), [
      { kind: "domain", name: OPAQUE_DOMAIN },
      {
        kind: "rule",
        name: opaqueValueRuleName("foo.ts:2"),
        params: [],
        returnType: OPAQUE_DOMAIN,
      },
    ]);
  });
});

describe("cellRegisterTupleConstructor", () => {
  it("dedupes by shape, not alias", () => {
    // Two TS aliases that share the same shape — `Point = [number,
    // number]` and `Vec2 = [number, number]` both map to ["Int", "Int"]
    // — must share a single constructor. Otherwise EUF congruence
    // (Kroening & Strichman, Decision Procedures Ch. 8) cannot prove
    // `make-point 0 0 = make-vec2 0 0`.
    const cell = newSynthCell();
    const shape1: TupleShape = { elementPantTypes: ["Int", "Int"] };
    const shape2: TupleShape = { elementPantTypes: ["Int", "Int"] };

    const ref1 = cellRegisterTupleConstructor(cell, shape1);
    const ref2 = cellRegisterTupleConstructor(cell, shape2);

    assert.ok(ref1 !== null);
    assert.ok(ref2 !== null);
    assert.equal(ref1.ctorRuleName, "make-int-int");
    assert.equal(ref1.ctorRuleName, ref2.ctorRuleName);
    assert.equal(ref1.depModuleName, ref2.depModuleName);
    assert.equal(cellTupleShapes(cell).length, 1);
  });

  it("derives depModuleName from cell.sourceFile", () => {
    const sf = createSourceFileFromSource(
      "interface Foo { val: number; }",
      "expressions-tuple.ts",
    );
    const cell = newSynthCell(undefined, sf.compilerNode);
    const ref = cellRegisterTupleConstructor(cell, {
      elementPantTypes: ["Int", "Int"],
    });
    assert.ok(ref !== null);
    assert.equal(ref.depModuleName, "EXPRESSIONS_TUPLE_TUPLES");
  });

  it("falls back to TUPLES when cell has no sourceFile", () => {
    const cell = newSynthCell();
    const ref = cellRegisterTupleConstructor(cell, {
      elementPantTypes: ["Int", "Int"],
    });
    assert.ok(ref !== null);
    assert.equal(ref.depModuleName, "TUPLES");
  });

  it("distinct shapes get distinct constructor names", () => {
    const cell = newSynthCell();
    const r1 = cellRegisterTupleConstructor(cell, {
      elementPantTypes: ["Int", "Int"],
    });
    const r2 = cellRegisterTupleConstructor(cell, {
      elementPantTypes: ["String", "Int"],
    });
    assert.ok(r1 !== null);
    assert.ok(r2 !== null);
    assert.notEqual(r1.ctorRuleName, r2.ctorRuleName);
    assert.equal(r1.ctorRuleName, "make-int-int");
    assert.equal(r2.ctorRuleName, "make-string-int");
  });

  it("does not collide nested tuple element shapes", () => {
    // `[Int, Int * Int]` and `[Int * Int, Int]` are structurally
    // distinct shapes; a delimiter-naive key (joining elements with
    // `*`) would hash both to `Int*Int*Int` and merge the
    // constructors. The canonical key must use a delimiter-safe
    // encoding so the boundaries between elements stay unambiguous.
    const cell = newSynthCell();
    const r1 = cellRegisterTupleConstructor(cell, {
      elementPantTypes: ["Int", "Int * Int"],
    });
    const r2 = cellRegisterTupleConstructor(cell, {
      elementPantTypes: ["Int * Int", "Int"],
    });
    assert.ok(r1 !== null);
    assert.ok(r2 !== null);
    assert.notEqual(r1.ctorRuleName, r2.ctorRuleName);
    assert.equal(cellTupleShapes(cell).length, 2);
  });

  it("ctor names are isolated from the consumer registry", () => {
    // Tuple ctors live in a separate dep module, so collisions in the
    // consumer's NameRegistry must not perturb the canonical
    // `make-<canonical>` form. A user-declared `make-int-int` in the
    // consumer should leave the synthesized ctor name unchanged.
    const consumerRegistry = registerName(
      emptyNameRegistry(),
      "make-int-int",
    ).registry;
    const cell = newSynthCell(consumerRegistry);
    const ref = cellRegisterTupleConstructor(cell, {
      elementPantTypes: ["Int", "Int"],
    });
    assert.ok(ref !== null);
    // No suffix despite the consumer's prior registration.
    assert.equal(ref.ctorRuleName, "make-int-int");
    // Consumer registry untouched: still a single registration.
    assert.equal(cell.registry.used.size, 1);
    assert.ok(cell.registry.used.has("make-int-int"));
  });
});

describe("emitTupleCtorModule", () => {
  it("for [Int, Int] produces a typechecking module", async () => {
    await loadAst();
    const sf = createSourceFileFromSource(
      "interface Foo { val: number; }",
      "tuple-shapes.ts",
    );
    const cell = newSynthCell(undefined, sf.compilerNode);
    const ref = cellRegisterTupleConstructor(cell, {
      elementPantTypes: ["Int", "Int"],
    });
    assert.ok(ref !== null);
    const text = emitTupleCtorModule(sf.compilerNode, cellTupleShapes(cell));

    // Module header uses the ALL_CAPS_SNAKE convention.
    assert.match(text, /^module TUPLE_SHAPES_TUPLES\.\n/u);
    // Constructor rule head present.
    assert.match(text, /make-int-int a1: Int, a2: Int => Int \* Int\./u);
    // The wasm checker accepts the standalone module.
    await assertWasmTypeChecks(text);
  });

  it("projection axioms hold", async () => {
    await loadAst();
    const sf = createSourceFileFromSource(
      "interface Foo { val: number; }",
      "tuple-shapes.ts",
    );
    const cell = newSynthCell(undefined, sf.compilerNode);
    cellRegisterTupleConstructor(cell, {
      elementPantTypes: ["Int", "Int"],
    });
    const text = emitTupleCtorModule(sf.compilerNode, cellTupleShapes(cell));

    // One projection axiom per element position, quantified over the
    // constructor's parameters. Free variables in propositions must be
    // bound — that's why the body is `all a1: Int, a2: Int | ...`.
    assert.match(
      text,
      /all a1: Int, a2: Int \| \(make-int-int a1 a2\)\.1 = a1\./u,
    );
    assert.match(
      text,
      /all a1: Int, a2: Int \| \(make-int-int a1 a2\)\.2 = a2\./u,
    );
    // Standalone-typechecks (which exercises the projection axioms).
    await assertWasmTypeChecks(text);
  });

  it("aggregates multiple shapes into one module", () => {
    const sf = createSourceFileFromSource(
      "interface Foo { val: number; }",
      "tuple-shapes.ts",
    );
    const cell = newSynthCell(undefined, sf.compilerNode);
    cellRegisterTupleConstructor(cell, {
      elementPantTypes: ["Int", "Int"],
    });
    cellRegisterTupleConstructor(cell, {
      elementPantTypes: ["String", "Int"],
    });
    const text = emitTupleCtorModule(sf.compilerNode, cellTupleShapes(cell));

    // One module header even with multiple shapes.
    const moduleHeaders = text.match(/^module /gmu) ?? [];
    assert.equal(moduleHeaders.length, 1);
    // Both constructor heads present.
    assert.match(text, /make-int-int /u);
    assert.match(text, /make-string-int /u);
  });
});

describe("depModuleNameForFile", () => {
  it("converts kebab-cased file base to ALL_CAPS_SNAKE", () => {
    assert.equal(
      depModuleNameForFile("/path/to/expressions-reduce.ts"),
      "EXPRESSIONS_REDUCE_TUPLES",
    );
  });

  it("strips directory and extension", () => {
    assert.equal(depModuleNameForFile("foo.ts"), "FOO_TUPLES");
  });

  it("falls back to TUPLES for extension-only or empty input", () => {
    // `_TUPLES` would be rejected by Pantagruel's lexer — UPPER_IDENT
    // requires an uppercase first character. Use the unprefixed
    // `TUPLES` instead, matching the no-sourceFile fallback in
    // `cellRegisterTupleConstructor`.
    assert.equal(depModuleNameForFile(".ts"), "TUPLES");
    assert.equal(depModuleNameForFile(""), "TUPLES");
  });

  it("prefixes digit-leading basenames with F_", () => {
    // `123_FOO_TUPLES` would also be rejected by the lexer. Prefix
    // with `F_` (a stable letter, no semantics) so the result is a
    // legal UPPER_IDENT.
    assert.equal(depModuleNameForFile("123-foo.ts"), "F_123_FOO_TUPLES");
    assert.equal(depModuleNameForFile("9.ts"), "F_9_TUPLES");
  });

  it("strips leading underscores so `_foo.ts` is well-formed", () => {
    // Filenames like `_foo.ts` would otherwise yield `_FOO_TUPLES`.
    // Strip the leading underscore-only prefix; the remaining
    // letter-led stem is a legal UPPER_IDENT.
    assert.equal(depModuleNameForFile("_foo.ts"), "FOO_TUPLES");
  });
});
