import assert from "node:assert/strict";
import { describe, it } from "node:test";
import {
  createSourceFileFromSource,
  type ExtractedInterface,
  extractAllTypes,
  extractReferencedTypes,
  getChecker,
} from "../src/extract.js";
import { assertWasmTypeChecks, loadAst } from "../src/pant-wasm.js";
import {
  cellRegisterTupleConstructor,
  cellTupleShapes,
  depModuleNameForFile,
  emitTupleCtorModule,
  IntStrategy,
  mapTsType,
  newSynthCell,
  RealStrategy,
  translateTypes,
  type TupleShape,
  UNSUPPORTED_UNKNOWN,
} from "../src/translate-types.js";
import type { PantDeclaration } from "../src/types.js";

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

describe("mapTsType", () => {
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

    assert.equal(mapTsType(prop.type, checker, IntStrategy), "undefined");
  });

  it("list-lifts `T | null` to `[T]`", () => {
    const source = `interface Foo { val: string | null; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(mapTsType(prop.type, checker, IntStrategy), "[String]");
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
    const foo = extracted.interfaces.find((i: any) => i.name === "Foo")!;
    const prop = foo.properties[0];

    assert.equal(mapTsType(prop.type, checker, IntStrategy), "[A + B]");
  });

  it("rejects `unknown`", () => {
    // Pantagruel is monomorphic over named domains; there is no top type
    // to absorb `unknown` into. mapTsType returns the dedicated
    // UNSUPPORTED_UNKNOWN sentinel so downstream UNSUPPORTED-skip
    // infrastructure can surface a clear diagnostic to the user.
    const source = `interface Foo { val: unknown; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(
      mapTsType(prop.type, checker, IntStrategy),
      UNSUPPORTED_UNKNOWN,
    );
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
    assert.match(
      text,
      /make-int-int a1: Int, a2: Int => Int \* Int\./u,
    );
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
});
