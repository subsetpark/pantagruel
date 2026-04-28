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
import { emptyNameRegistry, registerName } from "../src/name-registry.js";
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

  it("propagates `unknown` through tuple element", () => {
    const source = `interface Foo { val: [unknown, number]; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(
      mapTsType(prop.type, checker, IntStrategy),
      UNSUPPORTED_UNKNOWN,
    );
  });

  it("propagates `unknown` through array element", () => {
    const source = `interface Foo { val: unknown[]; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(
      mapTsType(prop.type, checker, IntStrategy),
      UNSUPPORTED_UNKNOWN,
    );
  });

  it("propagates `unknown` through Set element", () => {
    const source = `interface Foo { val: Set<unknown>; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    assert.equal(
      mapTsType(prop.type, checker, IntStrategy),
      UNSUPPORTED_UNKNOWN,
    );
  });

  it("propagates `unknown` through Map K and V", () => {
    // `Map<unknown, Int>` previously synthesized a domain like
    // `__unsupported_unknown__ToIntMap` because the all-underscore
    // sentinel happens to satisfy `manglePantTypeToFragment`'s
    // identifier check. Reject before the synth registers.
    const cellK = newSynthCell();
    const sourceK = `interface Foo { val: Map<unknown, number>; }`;
    const sfK = createSourceFileFromSource(sourceK);
    const propK = extractAllTypes(sfK).interfaces[0].properties[0];
    assert.equal(
      mapTsType(propK.type, getChecker(sfK), IntStrategy, cellK),
      UNSUPPORTED_UNKNOWN,
    );
    // No partial Map domain leaked into the synth state.
    assert.equal(cellK.synth.byKV.size, 0);

    const cellV = newSynthCell();
    const sourceV = `interface Foo { val: Map<string, unknown>; }`;
    const sfV = createSourceFileFromSource(sourceV);
    const propV = extractAllTypes(sfV).interfaces[0].properties[0];
    assert.equal(
      mapTsType(propV.type, getChecker(sfV), IntStrategy, cellV),
      UNSUPPORTED_UNKNOWN,
    );
    assert.equal(cellV.synth.byKV.size, 0);
  });

  it("propagates `unknown` through union members", () => {
    const source = `interface Foo { val: string | unknown; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    // `string | unknown` collapses to `unknown` at the TS-checker
    // layer, but the union branch must propagate the sentinel either
    // way. Either pre-collapse (the union check fires) or post-collapse
    // (the unknown short-circuit fires) — both surface the sentinel.
    assert.equal(
      mapTsType(prop.type, checker, IntStrategy),
      UNSUPPORTED_UNKNOWN,
    );
  });

  it("propagates `unknown` through anonymous record field", () => {
    const cell = newSynthCell();
    const source = `function f(): { x: unknown } { return { x: 1 }; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const fn = sourceFile.getFunctionOrThrow("f");
    const returnType = fn.getReturnType().compilerType;
    assert.equal(
      mapTsType(returnType, checker, IntStrategy, cell),
      UNSUPPORTED_UNKNOWN,
    );
    // No partial record domain leaked into the synth state.
    assert.equal(cell.recordSynth.byShape.size, 0);
  });
});

describe("translateTypes routes `unknown` through `unsupported` declaration", () => {
  it("interface field with `unknown` type emits an unsupported decl", () => {
    const { decls } = extractAndTranslate(
      `interface Foo { val: unknown; }`,
    );
    // The `Foo` domain decl is still pushed; the field accessor rule
    // is replaced by an `unsupported` decl carrying the user-facing
    // reason, so the emitted Pant text never contains the sentinel.
    assert.ok(decls.some((d) => d.kind === "domain" && d.name === "Foo"));
    const unsupported = decls.find((d) => d.kind === "unsupported");
    assert.ok(unsupported);
    if (unsupported.kind !== "unsupported") {
      throw new Error("expected unsupported decl");
    }
    assert.match(unsupported.reason, /Foo\.val/u);
    assert.match(unsupported.reason, /TS unknown is not expressible/u);
    // No rule decl referencing the sentinel string leaks through.
    assert.ok(
      !decls.some(
        (d) =>
          (d.kind === "rule" && d.returnType.includes("unsupported_unknown")) ||
          (d.kind === "alias" && d.type.includes("unsupported_unknown")),
      ),
    );
  });

  it("alias `T = unknown` emits an unsupported decl", () => {
    const { decls } = extractAndTranslate(`type Foo = unknown;`);
    const unsupported = decls.find((d) => d.kind === "unsupported");
    assert.ok(unsupported);
    if (unsupported.kind !== "unsupported") {
      throw new Error("expected unsupported decl");
    }
    assert.match(unsupported.reason, /alias Foo/u);
  });
});

describe("cellRegisterMap / cellRegisterRecord / cellRegisterTupleConstructor reject `unknown` defensively", () => {
  it("cellRegisterMap returns null when K is the sentinel", async () => {
    const { cellRegisterMap, newSynthCell, UNSUPPORTED_UNKNOWN } = await import(
      "../src/translate-types.js"
    );
    const cell = newSynthCell();
    assert.equal(
      cellRegisterMap(cell, UNSUPPORTED_UNKNOWN, "Int"),
      null,
    );
    assert.equal(cell.synth.byKV.size, 0);
  });

  it("cellRegisterRecord returns null when a field type is the sentinel", async () => {
    const { cellRegisterRecord, newSynthCell, UNSUPPORTED_UNKNOWN } =
      await import("../src/translate-types.js");
    const cell = newSynthCell();
    assert.equal(
      cellRegisterRecord(cell, [{ name: "x", type: UNSUPPORTED_UNKNOWN }]),
      null,
    );
    assert.equal(cell.recordSynth.byShape.size, 0);
  });

  it("cellRegisterTupleConstructor returns null when an element is the sentinel", async () => {
    const { UNSUPPORTED_UNKNOWN } = await import("../src/translate-types.js");
    const cell = newSynthCell();
    assert.equal(
      cellRegisterTupleConstructor(cell, {
        elementPantTypes: [UNSUPPORTED_UNKNOWN, "Int"],
      }),
      null,
    );
    assert.equal(cellTupleShapes(cell).length, 0);
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

  it("prefixes digit-leading basenames with F_", () => {
    // `123_FOO_TUPLES` would also be rejected by the lexer. Prefix
    // with `F_` (a stable letter, no semantics) so the result is a
    // legal UPPER_IDENT.
    assert.equal(
      depModuleNameForFile("123-foo.ts"),
      "F_123_FOO_TUPLES",
    );
    assert.equal(depModuleNameForFile("9.ts"), "F_9_TUPLES");
  });

  it("strips leading underscores so `_foo.ts` is well-formed", () => {
    // Filenames like `_foo.ts` would otherwise yield `_FOO_TUPLES`.
    // Strip the leading underscore-only prefix; the remaining
    // letter-led stem is a legal UPPER_IDENT.
    assert.equal(depModuleNameForFile("_foo.ts"), "FOO_TUPLES");
  });
});
