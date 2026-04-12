import { before, describe, it } from "node:test";
import assert from "node:assert/strict";
import { createSourceFileFromSource } from "../src/extract.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { translateBody } from "../src/translate-body.js";
import { IntStrategy } from "../src/translate-types.js";
import type { PantDeclaration } from "../src/types.js";

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

describe("const binding inline substitution — pure functions", () => {
  it("substitutes single const binding into return expression", () => {
    const source = `
      function simpleConst(a: number, b: number): number {
        const x = a + b;
        return x;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "simpleConst",
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

  it("substitutes chained const bindings fully into return expression", () => {
    // chainedConst: const x = a; const y = x + 1; return y → a + 1
    const source = `
      function chainedConst(a: number): number {
        const x = a;
        const y = x + 1;
        return y;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "chainedConst",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "a + 1");
    }
  });

  it("substitutes const binding referencing property access", () => {
    // const b = a.balance; return b + 1 → balance a + 1
    const source = `
      interface Account { balance: number; }
      function constWithPropAccess(a: Account): number {
        const b = a.balance;
        return b + 1;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "constWithPropAccess",
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

  it("substitutes const binding used in ternary condition and both branches", () => {
    // const x = a + b; return x > 0 ? x : 0 → cond (a+b > 0 => a+b, true => 0)
    const source = `
      function constInTernary(a: number, b: number): number {
        const x = a + b;
        return x > 0 ? x : 0;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "constInTernary",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "(cond a + b > 0 => a + b, true => 0)");
    }
  });

  it("rejects let binding with specific error message", () => {
    const source = `
      function letRejected(): number {
        let x = 1;
        return x;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "letRejected",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "unsupported");
    if (prop.kind === "unsupported") {
      assert.match(prop.reason, /let\/var bindings not supported/);
    }
  });

  it("rejects effectful const initializer with specific error message", () => {
    const source = `
      declare function sideEffect(): number;
      function effectfulConst(): number {
        const x = sideEffect();
        return x;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "effectfulConst",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "unsupported");
    if (prop.kind === "unsupported") {
      assert.match(prop.reason, /const binding with side-effectful initializer/);
    }
  });

  it("rejects self-referencing const initializer (TDZ)", () => {
    // const a = a + 1 would be a TDZ error at runtime
    const source = `
      function selfRef(x: number): number {
        const a = a + 1;
        return a;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "selfRef",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
    if (props[0]?.kind === "unsupported") {
      assert.match(props[0].reason, /const initializer references a later binding/);
    }
  });

  it("substitutes const binding into if/else return branches", () => {
    const source = `
      function constIfElse(a: number, b: number): number {
        const x = a + b;
        if (x > 0) {
          return x;
        } else {
          return 0;
        }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "constIfElse",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      // x is replaced by (a + b) throughout the if/else
      assert.equal(
        ast.strExpr(prop.rhs),
        "(cond a + b > 0 => a + b, true => 0)",
      );
    }
  });

  it("function with no const bindings still produces correct equation", () => {
    // Regression: zero-binding path should still work correctly after refactor
    const source = `
      function noConst(a: number, b: number): number {
        return a + b;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "noConst",
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
});

describe("const binding inline substitution — mutating functions", () => {
  const accountDeclarations: PantDeclaration[] = [
    {
      kind: "rule",
      name: "balance",
      params: [{ name: "a1", type: "Account" }],
      returnType: "Int",
    },
  ];

  it("inlines single const binding into property assignment rhs", () => {
    // const newBal = a.balance + amount; a.balance = newBal;
    // → balance' a = balance a + amount
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
      declarations: accountDeclarations,
    });

    const equations = props.filter((p) => p.kind === "equation");
    assert.ok(equations.length >= 1, "expected at least one equation");
    const balanceEq = equations.find(
      (p) =>
        p.kind === "equation" &&
        getAst().strExpr(p.lhs).startsWith("balance'"),
    );
    assert.ok(balanceEq, "expected a balance' equation");
    if (balanceEq?.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(balanceEq.rhs), "balance a + amount");
    }
  });

  it("inlines chained const bindings into mutating assignment", () => {
    // const fee = amount * rate; a.balance = a.balance - fee;
    // → balance' a = balance a - amount * rate
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
      declarations: accountDeclarations,
    });

    const equations = props.filter((p) => p.kind === "equation");
    assert.ok(equations.length >= 1, "expected at least one equation");
    const balanceEq = equations.find(
      (p) =>
        p.kind === "equation" &&
        getAst().strExpr(p.lhs).startsWith("balance'"),
    );
    assert.ok(balanceEq, "expected a balance' equation");
    if (balanceEq?.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(balanceEq.rhs), "balance a - amount * rate");
    }
  });

  it("rejects let binding in mutating function body", () => {
    const source = `
      interface Account { balance: number; }
      function mutatingLetRejected(a: Account): void {
        let temp = 1;
        a.balance = temp;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "mutatingLetRejected",
      strategy: IntStrategy,
      declarations: accountDeclarations,
    });

    const unsupported = props.filter((p) => p.kind === "unsupported");
    assert.ok(unsupported.length >= 1, "expected at least one unsupported proposition");
    if (unsupported[0]?.kind === "unsupported") {
      assert.match(
        unsupported[0].reason,
        /local variable declaration \(let\/var or effectful const\)/,
      );
    }
  });

  it("rejects effectful const in mutating function body", () => {
    const source = `
      interface Account { balance: number; }
      declare function compute(): number;
      function mutatingEffectfulConst(a: Account): void {
        const val = compute();
        a.balance = val;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "mutatingEffectfulConst",
      strategy: IntStrategy,
      declarations: accountDeclarations,
    });

    const unsupported = props.filter((p) => p.kind === "unsupported");
    assert.ok(unsupported.length >= 1, "expected at least one unsupported proposition");
    if (unsupported[0]?.kind === "unsupported") {
      assert.match(
        unsupported[0].reason,
        /local variable declaration \(let\/var or effectful const\)/,
      );
    }
  });

  it("rejects forward const reference in mutating function body", () => {
    const source = `
      interface Account { balance: number; }
      function mutatingForwardRef(a: Account): void {
        const x = y;
        const y = 1;
        a.balance = x;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "mutatingForwardRef",
      strategy: IntStrategy,
      declarations: accountDeclarations,
    });

    const unsupported = props.filter((p) => p.kind === "unsupported");
    assert.ok(unsupported.length >= 1, "expected at least one unsupported proposition");
    if (unsupported[0]?.kind === "unsupported") {
      assert.match(unsupported[0].reason, /const initializer references a later binding/);
    }
  });
});