import { before, describe, it } from "node:test";
import assert from "node:assert/strict";
import { createSourceFileFromSource } from "../src/extract.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { translateBody } from "../src/translate-body.js";
import { IntStrategy } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

// Tests for internal translateBody API edge cases not coverable via
// exported fixture functions (see tests/fixtures/constructs/ for
// exhaustive construct coverage).

describe("unsupported patterns", () => {
  it("returns empty for function with no body", () => {
    const source = `
      declare function external(x: number): number;
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "external",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 0);
  });

  it("translates function with leading const bindings via inline substitution", () => {
    const source = `
      function multi(x: number): number {
        const a = x + 1;
        const b = a * 2;
        return b;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "multi",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "(x + 1) * 2");
    }
  });

  it("rejects forward const reference (TDZ)", () => {
    // In TypeScript, `const a = b; const b = 1;` throws a ReferenceError
    // due to the Temporal Dead Zone. Verify we reject rather than silently
    // inlining b into a's initializer.
    const source = `
      function fwd(x: number): number {
        const a = b;
        const b = 1;
        return a;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "fwd",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });

  it("returns empty for bare return with no expression", () => {
    const source = `
      function noop(x: number): void {
        return;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "noop",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 0);
  });

  it("returns unsupported for single non-translatable statement", () => {
    const source = `
      function loop(x: number): number {
        while (x > 0) { x--; }
        return x;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "loop",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });
});

describe("const binding substitution in pure functions", () => {
  it("substitutes single const binding into return expression", () => {
    // `const x = a + b; return x;` should inline to `a + b`
    const source = `
      function add(a: number, b: number): number {
        const x = a + b;
        return x;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "add",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "a + b");
    }
  });

  it("substitutes const binding used in property access (hygiene check)", () => {
    // Verifies hygienic $N naming doesn't collide with property names.
    // `const b = a.balance; return b + 1;` should inline to `balance a + 1`
    const source = `
      interface Account { balance: number; }
      function getBalancePlusOne(a: Account): number {
        const b = a.balance;
        return b + 1;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "getBalancePlusOne",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "balance a + 1");
    }
  });

  it("substitutes const binding used twice in ternary return", () => {
    // `const x = a + b; return x > 0 ? x : 0;`
    // Both occurrences of `x` in the conditional should be substituted.
    const source = `
      function clampPositive(a: number, b: number): number {
        const x = a + b;
        return x > 0 ? x : 0;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "clampPositive",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "cond a + b > 0 => a + b, true => 0");
    }
  });

  it("substitutes const binding used in if/else return", () => {
    // `const x = a + b; if (x > 0) { return x; } else { return 0; }`
    const source = `
      function clampIf(a: number, b: number): number {
        const x = a + b;
        if (x > 0) { return x; } else { return 0; }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "clampIf",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "cond a + b > 0 => a + b, true => 0");
    }
  });

  it("rejects self-referencing const (TDZ self-reference)", () => {
    // `const x = x + 1;` is a TDZ error in JS/TS at runtime.
    const source = `
      function selfRef(n: number): number {
        const x = x + 1;
        return x;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "selfRef",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "unsupported");
    if (prop.kind === "unsupported") {
      assert.ok(
        prop.reason.includes("later binding"),
        `Expected TDZ error message, got: ${prop.reason}`,
      );
    }
  });

  it("rejects let binding with specific error message", () => {
    const source = `
      function withLet(n: number): number {
        let x = n + 1;
        return x;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "withLet",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "unsupported");
    if (prop.kind === "unsupported") {
      assert.ok(
        prop.reason.includes("let/var bindings not supported"),
        `Expected let/var error message, got: ${prop.reason}`,
      );
    }
  });

  it("rejects effectful const initializer with specific error message", () => {
    // A call expression is side-effectful (externally observable).
    const source = `
      declare function sideEffect(): number;
      function withEffectfulConst(): number {
        const x = sideEffect();
        return x;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "withEffectfulConst",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "unsupported");
    if (prop.kind === "unsupported") {
      assert.ok(
        prop.reason.includes("side-effectful"),
        `Expected side-effectful error message, got: ${prop.reason}`,
      );
    }
  });

  it("function with only a return (no consts) still works", () => {
    // Regression: zero-binding path must not be broken by the new logic.
    const source = `
      function double(x: number): number {
        return x * 2;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "double",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "x * 2");
    }
  });
});

describe("const binding substitution in mutating functions", () => {
  it("substitutes const binding into property assignment rhs", () => {
    // Mirrors the `depositWithConst` fixture.
    // `const newBal = a.balance + amount; a.balance = newBal;`
    // → `balance' a = balance a + amount`
    const source = `
      interface Account { balance: number; }
      function depositWithConst(a: Account, amount: number): void {
        const newBal = a.balance + amount;
        a.balance = newBal;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "depositWithConst",
      strategy: IntStrategy,
    });

    const eqProp = props.find((p) => p.kind === "equation");
    assert.ok(eqProp, "Expected at least one equation proposition");
    if (eqProp && eqProp.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(eqProp.lhs), "balance' a");
      assert.equal(ast.strExpr(eqProp.rhs), "balance a + amount");
    }
  });

  it("substitutes chained const bindings in mutating function", () => {
    // Mirrors the `multiConstMutating` fixture.
    // `const fee = amount * rate; a.balance = a.balance - fee;`
    // → `balance' a = balance a - amount * rate`
    const source = `
      interface Account { balance: number; }
      function multiConstMutating(a: Account, amount: number, rate: number): void {
        const fee = amount * rate;
        a.balance = a.balance - fee;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "multiConstMutating",
      strategy: IntStrategy,
    });

    const eqProp = props.find((p) => p.kind === "equation");
    assert.ok(eqProp, "Expected at least one equation proposition");
    if (eqProp && eqProp.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(eqProp.lhs), "balance' a");
      assert.equal(ast.strExpr(eqProp.rhs), "balance a - amount * rate");
    }
  });

  it("rejects forward const reference in mutating function", () => {
    // `const a = b; const b = 1; obj.x = a;` — TDZ in mutating body
    const source = `
      interface Box { x: number; }
      function forwardRefMutating(box: Box): void {
        const a = b;
        const b = 1;
        box.x = a;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "forwardRefMutating",
      strategy: IntStrategy,
    });

    const unsupported = props.find((p) => p.kind === "unsupported");
    assert.ok(unsupported, "Expected an unsupported proposition");
    if (unsupported && unsupported.kind === "unsupported") {
      assert.ok(
        unsupported.reason.includes("later binding"),
        `Expected TDZ error, got: ${unsupported.reason}`,
      );
    }
  });

  it("rejects let/var declaration in mutating function", () => {
    const source = `
      interface Box { x: number; }
      function letMutating(box: Box, n: number): void {
        let tmp = n + 1;
        box.x = tmp;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "letMutating",
      strategy: IntStrategy,
    });

    const unsupported = props.find((p) => p.kind === "unsupported");
    assert.ok(unsupported, "Expected an unsupported proposition for let");
    if (unsupported && unsupported.kind === "unsupported") {
      assert.ok(
        unsupported.reason.includes("local variable declaration"),
        `Expected local variable error, got: ${unsupported.reason}`,
      );
    }
  });
});