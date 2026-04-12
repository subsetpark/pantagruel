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

  it("inlines triple-chained const bindings via right-fold", () => {
    const source = `
      function triple(x: number): number {
        const a = x;
        const b = a + 1;
        const c = b * a;
        return c;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "triple",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "(x + 1) * x");
    }
  });

  it("rejects self-referencing const (TDZ)", () => {
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
  });

  it("hygienic names don't collide with property accessors", () => {
    // Regression: const named `balance` must not collide with the
    // property accessor head `balance` in `a.balance`.
    const source = `
      interface Account { balance: number }
      function addBonus(a: Account): number {
        const balance = a.balance;
        return balance + 10;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "addBonus",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "balance a + 10");
    }
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

describe("translateCallExpr", () => {
  it("should translate free function call as uninterpreted application", () => {
    const source = `
      declare function max(a: number, b: number): number;
      function f(a: number, b: number): number {
        return max(a, b);
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "max a b");
    }
  });

  it("should translate method call with receiver as first argument", () => {
    const source = `
      function f(s: string): string {
        return s.toUpperCase();
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "toUpperCase s");
    }
  });

  it("should translate zero-arity call as variable reference", () => {
    const source = `
      declare function now(): number;
      function f(): number {
        return now();
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.equal(ast.strExpr(prop.rhs), "now");
    }
  });

  it("should reject spread arguments with UNSUPPORTED", () => {
    const source = `
      declare function max(...args: number[]): number;
      function f(args: number[]): number {
        return max(...args);
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });
});
