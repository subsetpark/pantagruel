import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import {
  classifyFunction,
  detectOptionalParamDefault,
  findFunction,
  translateSignature,
} from "../src/translate-signature.js";
import { IntStrategy, RealStrategy } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

// Tests for internal translateSignature APIs: classifyFunction, guard
// expression structure, edge cases. See tests/fixtures/constructs/ for
// exhaustive construct coverage via snapshots.

function translate(
  source: string,
  functionName: string,
  strategy = IntStrategy,
) {
  const sourceFile = createSourceFileFromSource(source);
  return translateSignature(sourceFile, functionName, strategy);
}

describe("classifyFunction", () => {
  it("classifies pure function (returns value, no property assignments)", () => {
    const source = `
      function getBalance(account: { balance: number }): number {
        return account.balance;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const { node } = findFunction(sourceFile, "getBalance");
    const checker = getChecker(sourceFile);
    assert.equal(classifyFunction(node, checker), "pure");
  });

  it("classifies void function as mutating", () => {
    const source = `
      function reset(account: { balance: number }): void {
        account.balance = 0;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const { node } = findFunction(sourceFile, "reset");
    const checker = getChecker(sourceFile);
    assert.equal(classifyFunction(node, checker), "mutating");
  });

  it("classifies function with property assignment as mutating", () => {
    const source = `
      function setName(user: { name: string }, name: string): string {
        user.name = name;
        return name;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const { node } = findFunction(sourceFile, "setName");
    const checker = getChecker(sourceFile);
    assert.equal(classifyFunction(node, checker), "mutating");
  });
});

describe("numeric strategy", () => {
  it("respects RealStrategy for number types", () => {
    const source = `
      function double(n: number): number {
        return n * 2;
      }
    `;
    const result = translate(source, "double", RealStrategy);

    assert.deepEqual(result.declaration, {
      kind: "rule",
      name: "double",
      params: [{ name: "n", type: "Real" }],
      returnType: "Real",
    });
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
    const sourceFile = createSourceFileFromSource(source);
    const { node } = findFunction(sourceFile, "add");
    assert.ok(node.body !== undefined);
    // Verify translateSignature uses the implementation (any params), not the first overload
    const result = translateSignature(sourceFile, "add", IntStrategy);
    assert.equal(result.declaration.params.length, 2);
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
    const sourceFile = createSourceFileFromSource(source);
    const { node } = findFunction(sourceFile, "makeResetter");
    const checker = getChecker(sourceFile);
    assert.equal(classifyFunction(node, checker), "pure");
  });
});

describe("guard expression structure", () => {
  it("if/else-throw produces correct guard expression", () => {
    const source = `
      interface Account { balance: number; }
      function withdraw(a: Account, amount: number): void {
        if (a.balance >= amount) {
        } else {
          throw new Error("Insufficient funds");
        }
        a.balance = a.balance - amount;
      }
    `;
    const result = translate(source, "withdraw");
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(
      getAst().strExpr(result.declaration.guard!),
      "balance a >= amount",
    );
  });

  it("early-throw with negation produces correct guard expression", () => {
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
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(
      getAst().strExpr(result.declaration.guard!),
      "balance a >= amount",
    );
  });

  it("skips guard when if-condition has side effects (call)", () => {
    const source = `
      interface Account { balance: number; }
      function audit(): boolean { return true; }
      function withdraw(a: Account, amount: number): void {
        if (audit()) {
        } else {
          throw new Error("Audit failed");
        }
        a.balance = a.balance - amount;
      }
    `;
    const result = translate(source, "withdraw");
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(result.declaration.guard, undefined);
  });

  it("skips guard when negated if-condition has side effects", () => {
    const source = `
      interface Account { balance: number; }
      function check(): boolean { return true; }
      function withdraw(a: Account, amount: number): void {
        if (!check()) {
          throw new Error("Check failed");
        }
        a.balance = a.balance - amount;
      }
    `;
    const result = translate(source, "withdraw");
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(result.declaration.guard, undefined);
  });

  it("guard detection stops at non-if statements", () => {
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
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(result.declaration.guard, undefined);
  });

  it("assert guard produces correct expression", () => {
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
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(getAst().strExpr(result.declaration.guard!), "amount > 0");
  });

  it("multiple assertion guards are combined with and", () => {
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
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(
      getAst().strExpr(result.declaration.guard!),
      "amount > 0 and balance account >= 0",
    );
  });

  it("combines if-throw + assertion guards", () => {
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
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(
      getAst().strExpr(result.declaration.guard!),
      "~(amount <= 0) and balance account >= 0",
    );
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
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(result.declaration.guard, undefined);
  });
});

describe("call-graph following guard expressions", () => {
  it("follows a direct call", () => {
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
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(getAst().strExpr(result.declaration.guard!), "~(amount <= 0)");
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
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(
      getAst().strExpr(result.declaration.guard!),
      "~((balance account) <= 0)",
    );
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
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(getAst().strExpr(result.declaration.guard!), "~(amount <= 0)");
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
    const result = translate(source, "deposit");
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(result.declaration.guard, undefined);
  });
});

describe("class method declarations", () => {
  it("translates guarded class method", () => {
    const source = `
      class Account {
        balance: number = 0;
        withdraw(amount: number): void {
          if (this.balance >= amount) {
          } else {
            throw new Error("Insufficient funds");
          }
          this.balance = this.balance - amount;
        }
      }
    `;
    const result = translate(source, "withdraw");
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(
      getAst().strExpr(result.declaration.guard!),
      "balance a >= amount",
    );
    assert.deepEqual(result.declaration.params[0], {
      name: "a",
      type: "Account",
    });
  });

  it("does not extract guard when then-branch has side effects", () => {
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
    assert.equal(result.declaration.kind, "action");
    if (result.declaration.kind !== "action") {
      return;
    }
    assert.equal(result.declaration.guard, undefined);
  });
});

describe("@pant-type override", () => {
  it("overrides parameter type with the annotated Pantagruel type", () => {
    const source = `
      /**
       * @pant-type amount: Nat
       */
      function withdraw(amount: number): number {
        return amount;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const overrides = new Map([["amount", "Nat"]]);
    const result = translateSignature(
      sourceFile,
      "withdraw",
      IntStrategy,
      undefined,
      overrides,
    );
    assert.equal(result.declaration.kind, "rule");
    if (result.declaration.kind !== "rule") {
      return;
    }
    assert.equal(result.declaration.params[0]?.name, "amount");
    assert.equal(result.declaration.params[0]?.type, "Nat");
  });

  it("leaves non-overridden params at the default strategy type", () => {
    const source = `
      function pay(amount: number, memo: number): number {
        return amount;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const overrides = new Map([["amount", "Nat"]]);
    const result = translateSignature(
      sourceFile,
      "pay",
      IntStrategy,
      undefined,
      overrides,
    );
    assert.equal(result.declaration.kind, "rule");
    if (result.declaration.kind !== "rule") {
      return;
    }
    assert.equal(result.declaration.params[0]?.type, "Nat");
    assert.equal(result.declaration.params[1]?.type, "Int");
  });

  it("ignores overrides for parameter names that don't exist", () => {
    const source = `
      function pay(amount: number): number {
        return amount;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const overrides = new Map([["nonexistent", "Nat"]]);
    const result = translateSignature(
      sourceFile,
      "pay",
      IntStrategy,
      undefined,
      overrides,
    );
    assert.equal(result.declaration.kind, "rule");
    if (result.declaration.kind !== "rule") {
      return;
    }
    assert.equal(result.declaration.params[0]?.type, "Int");
  });
});

describe("detectOptionalParamDefault", () => {
  it("matches the ??-default idiom", () => {
    const source = `
      function makePoint(initial?: number): { x: number; y: number } {
        return { x: initial ?? 0, y: 0 };
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const { node } = findFunction(sourceFile, "makePoint");
    const result = detectOptionalParamDefault(node);
    assert.notEqual(result, null);
    assert.equal(result?.paramName, "initial");
    assert.equal(result?.defaultExpr.getText(), "0");
  });

  it("rejects an optional param used outside ??", () => {
    const source = `
      function f(value?: number): number {
        return value ?? 1 + value;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const { node } = findFunction(sourceFile, "f");
    // The trailing bare \`value\` reference disqualifies the split.
    assert.equal(detectOptionalParamDefault(node), null);
  });

  it("rejects when two ??-uses disagree on the default expression", () => {
    const source = `
      function f(value?: number): number {
        return (value ?? 0) + (value ?? 1);
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const { node } = findFunction(sourceFile, "f");
    assert.equal(detectOptionalParamDefault(node), null);
  });

  it("rejects when no parameter is optional", () => {
    const source = `
      function f(value: number): number {
        return value;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const { node } = findFunction(sourceFile, "f");
    assert.equal(detectOptionalParamDefault(node), null);
  });

  it("rejects when multiple parameters are optional", () => {
    const source = `
      function f(a?: number, b?: number): number {
        return (a ?? 0) + (b ?? 0);
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const { node } = findFunction(sourceFile, "f");
    assert.equal(detectOptionalParamDefault(node), null);
  });

  it("ignores same-named record-literal keys", () => {
    // The key `value:` in `{ value: value ?? 0 }` is lexically the param
    // name but is a property key, not a reference. The detection must not
    // mistake it for an out-of-?? use.
    const source = `
      function f(value?: number): { value: number } {
        return { value: value ?? 0 };
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const { node } = findFunction(sourceFile, "f");
    const result = detectOptionalParamDefault(node);
    assert.notEqual(result, null);
    assert.equal(result?.paramName, "value");
  });
});
