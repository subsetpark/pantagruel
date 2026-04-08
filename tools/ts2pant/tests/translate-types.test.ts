import { describe, it, expect } from "vitest";
import {
  createProgramFromSource,
  extractAllTypes,
  extractReferencedTypes,
} from "../src/extract.js";
import {
  translateTypes,
  mapTsType,
  IntStrategy,
  RealStrategy,
} from "../src/translate-types.js";

function extractAndTranslate(
  source: string,
  strategy = IntStrategy,
  fileName = "test.ts",
) {
  const program = createProgramFromSource(source, fileName);
  const extracted = extractAllTypes(program, fileName);
  const checker = program.getTypeChecker();
  const decls = translateTypes(extracted, checker, strategy);
  return { decls, extracted, checker, program };
}

describe("interface -> domain + rules", () => {
  it("extracts interface as domain plus one rule per property", () => {
    const { decls } = extractAndTranslate(`
      interface Account {
        balance: number;
        owner: string;
      }
    `);

    expect(decls).toContainEqual({ kind: "domain", name: "Account" });
    expect(decls).toContainEqual({
      kind: "rule",
      name: "balance",
      params: [{ name: "a", type: "Account" }],
      returnType: "Int",
    });
    expect(decls).toContainEqual({
      kind: "rule",
      name: "owner",
      params: [{ name: "a", type: "Account" }],
      returnType: "String",
    });
  });

  it("maps boolean properties", () => {
    const { decls } = extractAndTranslate(`
      interface User {
        active: boolean;
      }
    `);

    expect(decls).toContainEqual({
      kind: "rule",
      name: "active",
      params: [{ name: "u", type: "User" }],
      returnType: "Bool",
    });
  });

  it("maps array properties to list types", () => {
    const { decls } = extractAndTranslate(`
      interface Library {
        books: string[];
      }
    `);

    expect(decls).toContainEqual({
      kind: "rule",
      name: "books",
      params: [{ name: "l", type: "Library" }],
      returnType: "[String]",
    });
  });

  it("maps properties referencing other interfaces", () => {
    const { decls } = extractAndTranslate(`
      interface User {
        name: string;
      }
      interface Account {
        owner: User;
      }
    `);

    expect(decls).toContainEqual({ kind: "domain", name: "User" });
    expect(decls).toContainEqual({ kind: "domain", name: "Account" });
    expect(decls).toContainEqual({
      kind: "rule",
      name: "owner",
      params: [{ name: "a", type: "Account" }],
      returnType: "User",
    });
  });

  it("maps null union to sum with Nothing", () => {
    const { decls } = extractAndTranslate(`
      interface Task {
        assignee: string | null;
      }
    `);

    expect(decls).toContainEqual({
      kind: "rule",
      name: "assignee",
      params: [{ name: "t", type: "Task" }],
      returnType: "String + Nothing",
    });
  });
});

describe("type alias -> alias declaration", () => {
  it("maps tuple alias", () => {
    const { decls } = extractAndTranslate(`
      type Point = [number, number];
    `);

    expect(decls).toContainEqual({
      kind: "alias",
      name: "Point",
      type: "Int * Int",
    });
  });

  it("maps union alias", () => {
    const { decls } = extractAndTranslate(`
      interface Value { data: string; }
      type Result = Value | null;
    `);

    const alias = decls.find((d) => d.kind === "alias" && d.name === "Result");
    expect(alias).toBeDefined();
    expect(alias!.kind).toBe("alias");
    if (alias!.kind === "alias") {
      expect(alias!.type).toBe("Value + Nothing");
    }
  });

  it("maps simple alias to named type", () => {
    const { decls } = extractAndTranslate(`
      type Name = string;
    `);

    expect(decls).toContainEqual({
      kind: "alias",
      name: "Name",
      type: "String",
    });
  });
});

describe("enum -> domain", () => {
  it("maps enum to domain declaration", () => {
    const { decls } = extractAndTranslate(`
      enum Color {
        Red,
        Green,
        Blue,
      }
    `);

    expect(decls).toContainEqual({ kind: "domain", name: "Color" });
  });

  it("extracts enum member names", () => {
    const program = createProgramFromSource(`
      enum Status {
        Active,
        Inactive,
        Pending,
      }
    `);
    const extracted = extractAllTypes(program, "test.ts");

    expect(extracted.enums).toHaveLength(1);
    expect(extracted.enums[0].name).toBe("Status");
    expect(extracted.enums[0].members).toEqual([
      "Active",
      "Inactive",
      "Pending",
    ]);
  });
});

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
    const program = createProgramFromSource(source);
    const extracted = extractReferencedTypes(program, "test.ts", "getBalance");

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
    const program = createProgramFromSource(source);
    const extracted = extractReferencedTypes(program, "test.ts", "getUser");

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
    const program = createProgramFromSource(source);
    const extracted = extractReferencedTypes(program, "test.ts", "getItems");

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
    const program = createProgramFromSource(source);
    const extracted = extractReferencedTypes(
      program,
      "test.ts",
      "processOrder",
    );

    const names = extracted.interfaces.map((i) => i.name);
    expect(names).toContain("Order");
    expect(names).toContain("User");
    expect(names).toContain("Address");
  });
});

describe("mapTsType", () => {
  it("handles undefined/void as Nothing", () => {
    const source = `interface Foo { val: undefined; }`;
    const program = createProgramFromSource(source);
    const checker = program.getTypeChecker();
    const extracted = extractAllTypes(program, "test.ts");
    const prop = extracted.interfaces[0].properties[0];

    expect(mapTsType(prop.type, checker, IntStrategy)).toBe("Nothing");
  });
});
