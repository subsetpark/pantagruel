import { describe, it } from "node:test";
import assert from "node:assert/strict";
import {
  createSourceFileFromSource,
  extractAllTypes,
  extractReferencedTypes,
  getChecker,
} from "../src/extract.js";
import {
  IntStrategy,
  mapTsType,
  RealStrategy,
  translateTypes,
} from "../src/translate-types.js";

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

    assert.ok(decls.some((d: any) =>
      d.kind === "rule" && d.name === "value" &&
      d.params.length === 1 && d.params[0].name === "f" && d.params[0].type === "Foo" &&
      d.returnType === "Int"
    ));
  });

  it("RealStrategy maps number to Real", () => {
    const { decls } = extractAndTranslate(
      `interface Foo { value: number; }`,
      RealStrategy,
    );

    assert.ok(decls.some((d: any) =>
      d.kind === "rule" && d.name === "value" &&
      d.params.length === 1 && d.params[0].name === "f" && d.params[0].type === "Foo" &&
      d.returnType === "Real"
    ));
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
    assert.ok(extracted.interfaces.map((i: any) => i.name).includes("User"));
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

    const names = extracted.interfaces.map((i: any) => i.name);
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

    assert.ok(extracted.interfaces.map((i: any) => i.name).includes("User"));
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

    assert.ok(extracted.interfaces.map((i: any) => i.name).includes("Item"));
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

    const names = extracted.interfaces.map((i: any) => i.name);
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
});
