import { describe, it, expect } from "vitest";
import {
  createSourceFileFromSource,
  extractAllTypes,
  extractReferencedTypes,
  getChecker,
} from "../src/extract.js";
import {
  translateTypes,
  mapTsType,
  IntStrategy,
  RealStrategy,
} from "../src/translate-types.js";

// Tests for internal type translation APIs: mapTsType, extractReferencedTypes
// recursive following, numeric strategy. See tests/fixtures/constructs/ for
// exhaustive construct coverage via snapshots.

function extractAndTranslate(
  source: string,
  strategy = IntStrategy,
) {
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

    expect(decls).toContainEqual({
      kind: "rule",
      name: "value",
      params: [{ name: "f", type: "Foo" }],
      returnType: "Int",
    });
  });

  it("RealStrategy maps number to Real", () => {
    const { decls } = extractAndTranslate(
      `interface Foo { value: number; }`,
      RealStrategy,
    );

    expect(decls).toContainEqual({
      kind: "rule",
      name: "value",
      params: [{ name: "f", type: "Foo" }],
      returnType: "Real",
    });
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

    expect(extracted.enums).toHaveLength(1);
    expect(extracted.enums[0].name).toBe("Status");
    expect(extracted.enums[0].members).toEqual([
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

    const names = extracted.interfaces.map((i) => i.name);
    expect(names).toContain("Account");
    expect(names).toContain("User");
    expect(names).not.toContain("Unrelated");
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

    expect(extracted.interfaces.map((i) => i.name)).toContain("User");
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

    expect(extracted.interfaces.map((i) => i.name)).toContain("Item");
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

    const names = extracted.interfaces.map((i) => i.name);
    expect(names).toContain("Order");
    expect(names).toContain("User");
    expect(names).toContain("Address");
  });
});

describe("mapTsType", () => {
  it("handles undefined/void as Nothing", () => {
    const source = `interface Foo { val: undefined; }`;
    const sourceFile = createSourceFileFromSource(source);
    const checker = getChecker(sourceFile);
    const extracted = extractAllTypes(sourceFile);
    const prop = extracted.interfaces[0].properties[0];

    expect(mapTsType(prop.type, checker, IntStrategy)).toBe("Nothing");
  });
});
