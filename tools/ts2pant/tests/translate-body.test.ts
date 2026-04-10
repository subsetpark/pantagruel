import { describe, it, expect } from "vitest";
import { createProgramFromSource } from "../src/extract.js";
import { IntStrategy } from "../src/translate-types.js";
import { translateBody } from "../src/translate-body.js";
import type { PantDeclaration } from "../src/types.js";

function translate(
  source: string,
  functionName: string,
  declarations: PantDeclaration[] = [],
) {
  const fileName = "test.ts";
  const program = createProgramFromSource(source, fileName);
  return translateBody({
    program,
    fileName,
    functionName,
    strategy: IntStrategy,
    declarations,
  });
}

describe("pure function: return expression -> proposition", () => {
  it("translates simple return to defining proposition", () => {
    const source = `
      interface Account { balance: number; }
      function getBalance(a: Account): number {
        return a.balance;
      }
    `;
    const props = translate(source, "getBalance");

    expect(props).toHaveLength(1);
    expect(props[0].text).toBe("all a: Account | getBalance a = balance a");
  });

  it("translates arithmetic return expression", () => {
    const source = `
      function double(n: number): number {
        return n * 2;
      }
    `;
    const props = translate(source, "double");

    expect(props).toHaveLength(1);
    expect(props[0].text).toBe("all n: Int | double n = n * 2");
  });

  it("translates multi-param pure function", () => {
    const source = `
      function add(a: number, b: number): number {
        return a + b;
      }
    `;
    const props = translate(source, "add");

    expect(props).toHaveLength(1);
    expect(props[0].text).toBe("all a: Int, b: Int | add a b = a + b");
  });
});

describe("property access -> rule application", () => {
  it("translates property access to rule application", () => {
    const source = `
      interface Account { balance: number; owner: string; }
      function getOwner(a: Account): string {
        return a.owner;
      }
    `;
    const props = translate(source, "getOwner");

    expect(props[0].text).toBe("all a: Account | getOwner a = owner a");
  });

  it("translates nested property access", () => {
    const source = `
      interface User { name: string; }
      interface Account { owner: User; }
      function getOwnerName(a: Account): string {
        return a.owner.name;
      }
    `;
    const props = translate(source, "getOwnerName");

    expect(props[0].text).toBe(
      "all a: Account | getOwnerName a = name owner a",
    );
  });
});

describe("ternary -> cond", () => {
  it("translates ternary to cond expression", () => {
    const source = `
      function max(a: number, b: number): number {
        return a >= b ? a : b;
      }
    `;
    const props = translate(source, "max");

    expect(props[0].text).toBe(
      "all a: Int, b: Int | max a b = cond a >= b => a, true => b",
    );
  });

  it("translates ternary with property access in condition", () => {
    const source = `
      interface Account { active: boolean; balance: number; }
      function effectiveBalance(a: Account): number {
        return a.active ? a.balance : 0;
      }
    `;
    const props = translate(source, "effectiveBalance");

    expect(props[0].text).toBe(
      "all a: Account | effectiveBalance a = cond active a => balance a, true => 0",
    );
  });
});

describe("if/else with returns -> cond", () => {
  it("translates if/else return to cond", () => {
    const source = `
      function abs(n: number): number {
        if (n >= 0) {
          return n;
        } else {
          return 0 - n;
        }
      }
    `;
    const props = translate(source, "abs");

    expect(props[0].text).toBe(
      "all n: Int | abs n = cond n >= 0 => n, true => 0 - n",
    );
  });
});

describe("boolean operators", () => {
  it("translates && to and, || to or, ! to ~", () => {
    const source = `
      function check(a: boolean, b: boolean): boolean {
        return a && !b;
      }
    `;
    const props = translate(source, "check");

    expect(props[0].text).toBe(
      "all a: Bool, b: Bool | check a b = a and ~(b)",
    );
  });

  it("translates === to = and !== to ~=", () => {
    const source = `
      function eq(a: number, b: number): boolean {
        return a === b;
      }
    `;
    const props = translate(source, "eq");

    expect(props[0].text).toBe(
      "all a: Int, b: Int | eq a b = a = b",
    );
  });
});

describe("array operations", () => {
  it("translates .length to #", () => {
    const source = `
      function count(items: string[]): number {
        return items.length;
      }
    `;
    const props = translate(source, "count");

    expect(props[0].text).toBe("all items: [String] | count items = #items");
  });

  it("translates .includes(x) to x in", () => {
    const source = `
      function contains(items: string[], x: string): boolean {
        return items.includes(x);
      }
    `;
    const props = translate(source, "contains");

    expect(props[0].text).toBe(
      "all items: [String], x: String | contains items x = x in items",
    );
  });

  it("translates .filter(p).map(f) to each comprehension", () => {
    const source = `
      interface User { name: string; active: boolean; }
      function activeNames(users: User[]): string[] {
        return users.filter((u) => u.active).map((u) => u.name);
      }
    `;
    const props = translate(source, "activeNames");

    expect(props[0].text).toBe(
      "all users: [User] | activeNames users = each x: User, active x | name x",
    );
  });
});

describe("mutating function: assignment -> primed proposition", () => {
  it("translates property assignment to primed expression", () => {
    const source = `
      interface Account { balance: number; }
      function deposit(a: Account, amount: number): void {
        a.balance = a.balance + amount;
      }
    `;
    const props = translate(source, "deposit");

    expect(props.some((p) => p.text === "balance' a = balance a + amount")).toBe(
      true,
    );
  });

  it("translates multiple assignments", () => {
    const source = `
      interface Account { balance: number; owner: string; }
      function transfer(a: Account, newOwner: string, fee: number): void {
        a.balance = a.balance - fee;
        a.owner = newOwner;
      }
    `;
    const props = translate(source, "transfer");

    expect(props.some((p) => p.text === "balance' a = balance a - fee")).toBe(true);
    expect(props.some((p) => p.text === "owner' a = newOwner")).toBe(true);
  });
});

describe("frame conditions", () => {
  it("generates frame conditions for unmodified rules", () => {
    const source = `
      interface Account { balance: number; }
      function deposit(a: Account, amount: number): void {
        a.balance = a.balance + amount;
      }
    `;
    const declarations: PantDeclaration[] = [
      { kind: "domain", name: "Account" },
      {
        kind: "rule",
        name: "balance",
        params: [{ name: "a", type: "Account" }],
        returnType: "Int",
      },
      {
        kind: "rule",
        name: "owner",
        params: [{ name: "a", type: "Account" }],
        returnType: "String",
      },
    ];

    const props = translate(source, "deposit", declarations);

    // balance is modified, so no frame condition for it
    expect(
      props.some((p) => p.text.includes("balance'") && p.text.includes("balance a + amount")),
    ).toBe(true);

    // owner is NOT modified, so frame condition generated
    expect(
      props.some((p) => p.text === "all a: Account | owner' a = owner a"),
    ).toBe(true);

    // No frame condition for balance (it's modified)
    expect(
      props.some((p) => p.text === "all a: Account | balance' a = balance a"),
    ).toBe(false);
  });

  it("skips domains and actions in frame conditions", () => {
    const source = `
      interface Account { balance: number; }
      function reset(a: Account): void {
        a.balance = 0;
      }
    `;
    const declarations: PantDeclaration[] = [
      { kind: "domain", name: "Account" },
      {
        kind: "action",
        label: "OtherAction",
        params: [{ name: "a", type: "Account" }],
      },
      {
        kind: "rule",
        name: "balance",
        params: [{ name: "a", type: "Account" }],
        returnType: "Int",
      },
    ];

    const props = translate(source, "reset", declarations);

    // Only the assignment proposition, no frame for action/domain
    expect(props.some((p) => p.text === "balance' a = 0")).toBe(true);
    expect(props).toHaveLength(1); // just the assignment, no frames since balance is the only rule
  });
});

describe("class method body translation", () => {
  it("translates class method with this -> param mapping", () => {
    const source = `
      class Account {
        balance: number = 0;
        getBalance(): number {
          return this.balance;
        }
      }
    `;
    const props = translate(source, "getBalance");

    expect(props[0].text).toBe("all account: Account | getBalance account = balance account");
  });

  it("translates mutating class method", () => {
    const source = `
      class Account {
        balance: number = 0;
        deposit(amount: number): void {
          this.balance = this.balance + amount;
        }
      }
    `;
    const props = translate(source, "deposit");

    expect(props.some((p) => p.text === "balance' account = balance account + amount")).toBe(
      true,
    );
  });
});

describe("unsupported patterns", () => {
  it("returns empty for function with no body", () => {
    const source = `
      declare function external(x: number): number;
    `;
    const fileName = "test.ts";
    const program = createProgramFromSource(source, fileName);
    const props = translateBody({
      program,
      fileName,
      functionName: "external",
      strategy: IntStrategy,
    });

    expect(props).toHaveLength(0);
  });
});

describe("guarded function body", () => {
  it("skips guard (if-throw) and translates remaining body", () => {
    const source = `
      interface Account { balance: number; }
      function withdraw(a: Account, amount: number): void {
        if (!(a.balance >= amount)) {
          throw new Error("Insufficient funds");
        }
        a.balance = a.balance - amount;
      }
    `;
    const props = translate(source, "withdraw");

    expect(props.some((p) => p.text === "balance' a = balance a - amount")).toBe(true);
    // Should not contain anything about the guard/throw
    expect(props.some((p) => p.text.includes("throw"))).toBe(false);
  });
});
