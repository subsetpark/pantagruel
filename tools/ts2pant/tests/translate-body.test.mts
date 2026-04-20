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

describe("conditional mutations (symbolic last-write)", () => {
  it("rejects conditional mutation when if-condition is impure", () => {
    const source = `
      interface Account { balance: number; }
      declare function check(): boolean;
      function impure(a: Account): void {
        if (check()) { a.balance = 1; }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "impure",
      strategy: IntStrategy,
    });

    const unsupported = props.find((p) => p.kind === "unsupported");
    assert.ok(unsupported, "expected at least one unsupported proposition");
    assert.equal(props.filter((p) => p.kind === "equation").length, 0);
  });

  it("rejects return in a branch that isn't the bare early-exit shape", () => {
    // The then-branch does something *before* returning — not a pure early
    // exit, so if-conversion can't lift it.
    const source = `
      interface Account { balance: number; }
      function mixed(a: Account, g: boolean): void {
        if (g) { a.balance = 0; return; }
        a.balance = 1;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "mixed",
      strategy: IntStrategy,
    });

    const unsupported = props.find((p) => p.kind === "unsupported");
    assert.ok(unsupported);
    assert.equal(props.filter((p) => p.kind === "equation").length, 0);
  });

  it("early-exit if-conversion: `if (g) return;` guards subsequent writes under !g", () => {
    const source = `
      interface Account { balance: number; }
      function earlyReturn(a: Account, g: boolean): void {
        if (g) { return; }
        a.balance = 1;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "earlyReturn",
      strategy: IntStrategy,
    });

    const equations = props.filter((p) => p.kind === "equation");
    assert.equal(equations.length, 1);
    const eq = equations[0]!;
    if (eq.kind !== "equation") return;
    const ast = getAst();
    assert.equal(ast.strExpr(eq.lhs), "balance' a");
    // Early return path keeps pre-state identity; fall-through path writes 1.
    assert.equal(ast.strExpr(eq.rhs), "cond g => balance a, true => 1");
  });

  it("sequential composition: later conditional reads earlier unconditional write", () => {
    const source = `
      interface Account { balance: number; }
      function compose(a: Account, g: boolean): void {
        a.balance = 10;
        if (g) { a.balance = a.balance + 5; }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "compose",
      strategy: IntStrategy,
    });

    const equations = props.filter((p) => p.kind === "equation");
    assert.equal(equations.length, 1);
    const eq = equations[0]!;
    if (eq.kind !== "equation") return;
    const ast = getAst();
    assert.equal(ast.strExpr(eq.lhs), "balance' a");
    // Conditional branch sees the prior write (10) rather than the pre-state `balance a`.
    assert.equal(ast.strExpr(eq.rhs), "cond g => 10 + 5, true => 10");
  });

  it("both branches writing same prop merge into a single cond equation", () => {
    const source = `
      interface User { active: boolean; }
      function toggle(u: User): void {
        if (u.active) { u.active = false; }
        else { u.active = true; }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "toggle",
      strategy: IntStrategy,
    });

    const equations = props.filter((p) => p.kind === "equation");
    assert.equal(equations.length, 1);
    const eq = equations[0]!;
    if (eq.kind !== "equation") return;
    const ast = getAst();
    assert.equal(ast.strExpr(eq.lhs), "active' u");
    assert.equal(
      ast.strExpr(eq.rhs),
      "cond active u => false, true => true",
    );
  });

  it("compound assignment += desugars to read-modify-write", () => {
    const source = `
      interface Account { balance: number; }
      function addAmount(a: Account, amount: number): void {
        a.balance += amount;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "addAmount",
      strategy: IntStrategy,
    });

    const equations = props.filter((p) => p.kind === "equation");
    assert.equal(equations.length, 1);
    const eq = equations[0]!;
    if (eq.kind !== "equation") return;
    const ast = getAst();
    assert.equal(ast.strExpr(eq.lhs), "balance' a");
    assert.equal(ast.strExpr(eq.rhs), "balance a + amount");
  });

  it("compound assignment after unconditional write reads through prior write", () => {
    const source = `
      interface Account { balance: number; }
      function writeThenBump(a: Account, amount: number): void {
        a.balance = 10;
        a.balance += amount;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "writeThenBump",
      strategy: IntStrategy,
    });

    const equations = props.filter((p) => p.kind === "equation");
    assert.equal(equations.length, 1);
    const eq = equations[0]!;
    if (eq.kind !== "equation") return;
    const ast = getAst();
    assert.equal(ast.strExpr(eq.lhs), "balance' a");
    // compound-assign rhs reads the prior write (10) via symbolic state.
    assert.equal(ast.strExpr(eq.rhs), "10 + amount");
  });

  it("else-branch early exit: `if (c) { X } else { return; }` gates X under c", () => {
    const source = `
      interface Account { balance: number; }
      function elseReturn(a: Account, g: boolean, v: number): void {
        if (g) { a.balance = v; } else { return; }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "elseReturn",
      strategy: IntStrategy,
    });

    const equations = props.filter((p) => p.kind === "equation");
    assert.equal(equations.length, 1);
    const eq = equations[0]!;
    if (eq.kind !== "equation") return;
    const ast = getAst();
    assert.equal(ast.strExpr(eq.lhs), "balance' a");
    // Continuation-path (g true) writes v; early-exit (g false) preserves pre-state.
    assert.equal(ast.strExpr(eq.rhs), "cond g => v, true => balance a");
  });

  it("asymmetric writes produce separate per-prop cond equations", () => {
    const source = `
      interface Account { balance: number; owner: string; }
      function asym(a: Account, g: boolean, newOwner: string): void {
        if (g) { a.balance = 0; }
        else { a.owner = newOwner; }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "asym",
      strategy: IntStrategy,
    });

    const equations = props.filter((p) => p.kind === "equation");
    assert.equal(equations.length, 2);
    const ast = getAst();
    const lhsStrings = equations.map((e) =>
      e.kind === "equation" ? ast.strExpr(e.lhs) : "",
    );
    assert.deepEqual(lhsStrings.sort(), ["balance' a", "owner' a"]);
    // Each branch should use the pre-state identity in its untouched arm.
    const balanceEq = equations.find(
      (e) => e.kind === "equation" && ast.strExpr(e.lhs) === "balance' a",
    );
    const ownerEq = equations.find(
      (e) => e.kind === "equation" && ast.strExpr(e.lhs) === "owner' a",
    );
    if (balanceEq?.kind === "equation") {
      assert.equal(
        ast.strExpr(balanceEq.rhs),
        "cond g => 0, true => balance a",
      );
    }
    if (ownerEq?.kind === "equation") {
      assert.equal(
        ast.strExpr(ownerEq.rhs),
        "cond g => owner a, true => newOwner",
      );
    }
  });
});
