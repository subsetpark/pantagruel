import { describe, it, expect } from "vitest";
import { createProgramFromSource } from "../src/extract.js";
import { IntStrategy, RealStrategy } from "../src/translate-types.js";
import {
  translateSignature,
  classifyFunction,
  findFunction,
} from "../src/translate-signature.js";
import { renderExpr } from "../src/pant-expr.js";

function translate(source: string, functionName: string, strategy = IntStrategy) {
  const fileName = "test.ts";
  const program = createProgramFromSource(source, fileName);
  return translateSignature(program, fileName, functionName, strategy);
}

describe("classifyFunction", () => {
  it("classifies pure function (returns value, no property assignments)", () => {
    const source = `
      function getBalance(account: { balance: number }): number {
        return account.balance;
      }
    `;
    const program = createProgramFromSource(source);
    const { node } = findFunction(program, "test.ts", "getBalance");
    const checker = program.getTypeChecker();
    expect(classifyFunction(node, checker)).toBe("pure");
  });

  it("classifies void function as mutating", () => {
    const source = `
      function reset(account: { balance: number }): void {
        account.balance = 0;
      }
    `;
    const program = createProgramFromSource(source);
    const { node } = findFunction(program, "test.ts", "reset");
    const checker = program.getTypeChecker();
    expect(classifyFunction(node, checker)).toBe("mutating");
  });

  it("classifies function with property assignment as mutating", () => {
    const source = `
      function setName(user: { name: string }, name: string): string {
        user.name = name;
        return name;
      }
    `;
    const program = createProgramFromSource(source);
    const { node } = findFunction(program, "test.ts", "setName");
    const checker = program.getTypeChecker();
    expect(classifyFunction(node, checker)).toBe("mutating");
  });
});

describe("pure function -> rule", () => {
  it("translates simple pure function to rule", () => {
    const source = `
      interface Account { balance: number; }
      function getBalance(a: Account): number {
        return a.balance;
      }
    `;
    const result = translate(source, "getBalance");

    expect(result.classification).toBe("pure");
    expect(result.declaration).toEqual({
      kind: "rule",
      name: "getBalance",
      params: [{ name: "a", type: "Account" }],
      returnType: "Int",
    });
  });

  it("translates multi-param pure function", () => {
    const source = `
      interface Account { balance: number; owner: string; }
      function foo(a: Account, b: number): string {
        return a.owner;
      }
    `;
    const result = translate(source, "foo");

    expect(result.classification).toBe("pure");
    expect(result.declaration).toEqual({
      kind: "rule",
      name: "foo",
      params: [
        { name: "a", type: "Account" },
        { name: "b", type: "Int" },
      ],
      returnType: "String",
    });
  });

  it("respects numeric strategy", () => {
    const source = `
      function double(n: number): number {
        return n * 2;
      }
    `;
    const result = translate(source, "double", RealStrategy);

    expect(result.declaration).toEqual({
      kind: "rule",
      name: "double",
      params: [{ name: "n", type: "Real" }],
      returnType: "Real",
    });
  });

  it("translates boolean return type", () => {
    const source = `
      function isActive(active: boolean): boolean {
        return active;
      }
    `;
    const result = translate(source, "isActive");

    expect(result.declaration.kind).toBe("rule");
    if (result.declaration.kind === "rule") {
      expect(result.declaration.returnType).toBe("Bool");
    }
  });

  it("translates array return type", () => {
    const source = `
      function getNames(names: string[]): string[] {
        return names;
      }
    `;
    const result = translate(source, "getNames");

    expect(result.declaration.kind).toBe("rule");
    if (result.declaration.kind === "rule") {
      expect(result.declaration.returnType).toBe("[String]");
    }
  });
});

describe("void mutator -> action", () => {
  it("translates void function to action", () => {
    const source = `
      interface Account { balance: number; }
      function deposit(a: Account, amount: number): void {
        a.balance = a.balance + amount;
      }
    `;
    const result = translate(source, "deposit");

    expect(result.classification).toBe("mutating");
    expect(result.declaration).toEqual({
      kind: "action",
      label: "Deposit",
      params: [
        { name: "a", type: "Account" },
        { name: "amount", type: "Int" },
      ],
    });
  });

  it("capitalizes action label from function name", () => {
    const source = `
      function doSomething(): void {}
    `;
    const result = translate(source, "doSomething");

    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.label).toBe("DoSomething");
    }
  });
});

describe("guarded mutator -> action with guard", () => {
  it("detects if/else-throw guard pattern", () => {
    const source = `
      interface Account { balance: number; }
      function withdraw(a: Account, amount: number): void {
        if (a.balance >= amount) {
          a.balance = a.balance - amount;
        } else {
          throw new Error("Insufficient funds");
        }
      }
    `;
    const result = translate(source, "withdraw");

    expect(result.classification).toBe("mutating");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.label).toBe("Withdraw");
      expect(renderExpr(result.declaration.guard!)).toBe("balance a >= amount");
    }
  });

  it("ignores non-unconditional throw in else branch", () => {
    const source = `
      interface Account { balance: number; }
      function withdraw(a: Account, amount: number, retry: boolean): void {
        if (a.balance >= amount) {
          a.balance = a.balance - amount;
        } else {
          if (retry) throw new Error("Insufficient funds");
        }
      }
    `;
    const result = translate(source, "withdraw");

    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBeUndefined();
    }
  });

  it("detects early-throw guard pattern with negation", () => {
    const source = `
      interface Account { balance: number; }
      function withdraw(a: Account, amount: number): void {
        if (!(a.balance >= amount)) {
          throw new Error("Insufficient funds");
        }
        a.balance = a.balance - amount;
      }
    `;
    const result = translate(source, "withdraw");

    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(renderExpr(result.declaration.guard!)).toBe("balance a >= amount");
    }
  });
});

describe("overloaded functions", () => {
  it("prefers implementation over overload signature", () => {
    const source = `
      function add(a: number, b: number): number;
      function add(a: string, b: string): string;
      function add(a: any, b: any): any {
        return a + b;
      }
    `;
    const program = createProgramFromSource(source);
    const { node } = findFunction(program, "test.ts", "add");
    // Should find the implementation (has body), not an overload signature
    expect(node.body).toBeDefined();
  });
});

describe("nested closure property assignment", () => {
  it("does not classify outer function as mutating due to nested closure", () => {
    const source = `
      interface Account { balance: number; }
      function makeResetter(a: Account): () => void {
        return () => { a.balance = 0; };
      }
    `;
    const program = createProgramFromSource(source);
    const { node } = findFunction(program, "test.ts", "makeResetter");
    const checker = program.getTypeChecker();
    expect(classifyFunction(node, checker)).toBe("pure");
  });
});

describe("guard detection stops at non-if statements", () => {
  it("does not detect guard after non-guard statements", () => {
    const source = `
      interface Account { balance: number; }
      function process(a: Account, amount: number): void {
        a.balance = a.balance + 1;
        if (a.balance >= amount) {
          a.balance = a.balance - amount;
        } else {
          throw new Error("Insufficient funds");
        }
      }
    `;
    const result = translate(source, "process");

    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBeUndefined();
    }
  });
});

describe("asserts function guard detection", () => {
  it("extracts guard from assert() with asserts return type", () => {
    const source = `
      function assert(condition: unknown, msg?: string): asserts condition {
        if (!condition) throw new Error(msg ?? "Assertion failed");
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        assert(amount > 0, "Amount must be positive");
        account.balance = account.balance + amount;
      }
    `;
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBe("amount > 0");
    }
  });

  it("extracts multiple assertion guards", () => {
    const source = `
      function assert(condition: unknown, msg?: string): asserts condition {
        if (!condition) throw new Error(msg ?? "Assertion failed");
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        assert(amount > 0);
        assert(account.balance >= 0);
        account.balance = account.balance + amount;
      }
    `;
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBe("amount > 0, balance account >= 0");
    }
  });

  it("combines assertion guards with if-throw guards", () => {
    const source = `
      function assert(condition: unknown): asserts condition {
        if (!condition) throw new Error();
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        if (amount <= 0) { throw new Error("bad amount"); }
        assert(account.balance >= 0);
        account.balance = account.balance + amount;
      }
    `;
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBe("~(amount <= 0), balance account >= 0");
    }
  });

  it("ignores non-assertion calls (stops scanning)", () => {
    const source = `
      function log(msg: string): void { console.log(msg); }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        log("depositing");
        account.balance = account.balance + amount;
      }
    `;
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBeUndefined();
    }
  });
});

describe("call-graph following for guard extraction", () => {
  it("follows a direct call to extract guards", () => {
    const source = `
      function validateAmount(amount: number): void {
        if (amount <= 0) { throw new Error("bad"); }
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        validateAmount(amount);
        account.balance = account.balance + amount;
      }
    `;
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBe("~(amount <= 0)");
    }
  });

  it("substitutes formal params with actual args", () => {
    const source = `
      function requirePositive(n: number): void {
        if (n <= 0) { throw new Error(); }
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        requirePositive(account.balance);
        account.balance = account.balance + amount;
      }
    `;
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBe("~(balance account <= 0)");
    }
  });

  it("follows multiple calls and combines guards", () => {
    const source = `
      function validateAmount(amount: number): void {
        if (amount <= 0) { throw new Error(); }
      }
      function requireNonNeg(n: number): void {
        if (n < 0) { throw new Error(); }
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        validateAmount(amount);
        requireNonNeg(account.balance);
        account.balance = account.balance + amount;
      }
    `;
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBe("~(amount <= 0), ~(balance account < 0)");
    }
  });

  it("follows calls two levels deep", () => {
    const source = `
      function requirePositive(n: number): void {
        if (n <= 0) { throw new Error(); }
      }
      function validateAmount(amount: number): void {
        requirePositive(amount);
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        validateAmount(amount);
        account.balance = account.balance + amount;
      }
    `;
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBe("~(amount <= 0)");
    }
  });

  it("follows const-bound arrow functions", () => {
    const source = `
      const checkPositive = (n: number): void => {
        if (n <= 0) { throw new Error(); }
      };
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        checkPositive(amount);
        account.balance = account.balance + amount;
      }
    `;
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBe("~(amount <= 0)");
    }
  });

  it("bails on non-guard function calls", () => {
    const source = `
      function log(msg: string): void { console.log(msg); }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        log("depositing");
        account.balance = account.balance + amount;
      }
    `;
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBeUndefined();
    }
  });

  it("bails on recursive calls", () => {
    const source = `
      function validate(n: number): void {
        if (n > 100) validate(n - 1);
        if (n <= 0) { throw new Error(); }
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        validate(amount);
        account.balance = account.balance + amount;
      }
    `;
    // Should not hang or crash — bails on recursion
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
  });

  it("combines followed guards with inline if-throw", () => {
    const source = `
      function requirePositive(n: number): void {
        if (n <= 0) { throw new Error(); }
      }
      interface Account { balance: number; }
      function deposit(account: Account, amount: number): void {
        if (amount > 1000) { throw new Error("too much"); }
        requirePositive(amount);
        account.balance = account.balance + amount;
      }
    `;
    const result = translate(source, "deposit");
    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.guard).toBe("~(amount > 1000), ~(amount <= 0)");
    }
  });
});

describe("class method -> declaration with this param", () => {
  it("translates pure method with this as first param", () => {
    const source = `
      class Account {
        balance: number = 0;
        getBalance(): number {
          return this.balance;
        }
      }
    `;
    const result = translate(source, "getBalance");

    expect(result.classification).toBe("pure");
    expect(result.declaration).toEqual({
      kind: "rule",
      name: "getBalance",
      params: [{ name: "a", type: "Account" }],
      returnType: "Int",
    });
  });

  it("translates mutating method with this as first param", () => {
    const source = `
      class Account {
        balance: number = 0;
        deposit(amount: number): void {
          this.balance = this.balance + amount;
        }
      }
    `;
    const result = translate(source, "deposit");

    expect(result.classification).toBe("mutating");
    expect(result.declaration).toEqual({
      kind: "action",
      label: "Deposit",
      params: [
        { name: "a", type: "Account" },
        { name: "amount", type: "Int" },
      ],
    });
  });

  it("translates guarded method", () => {
    const source = `
      class Account {
        balance: number = 0;
        withdraw(amount: number): void {
          if (this.balance >= amount) {
            this.balance = this.balance - amount;
          } else {
            throw new Error("Insufficient funds");
          }
        }
      }
    `;
    const result = translate(source, "withdraw");

    expect(result.declaration.kind).toBe("action");
    if (result.declaration.kind === "action") {
      expect(result.declaration.label).toBe("Withdraw");
      expect(renderExpr(result.declaration.guard!)).toBe("balance a >= amount");
      expect(result.declaration.params[0]).toEqual({
        name: "a",
        type: "Account",
      });
    }
  });
});
