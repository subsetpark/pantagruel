import { before, describe, it } from "node:test";
import assert from "node:assert/strict";
import { createSourceFileFromSource } from "../src/extract.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import { translateBody } from "../src/translate-body.js";
import { IntStrategy, RealStrategy } from "../src/translate-types.js";

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
      assert.equal(ast.strExpr(prop.rhs), "account--balance a + 10");
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

describe("if-early-return prelude arms", () => {
  it("translates a single early-return arm followed by a return", () => {
    const source = `
      export function f(n: number): number {
        if (n < 0) return 0;
        return n + 1;
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
      assert.equal(ast.strExpr(prop.rhs), "cond n < 0 => 0, true => n + 1");
    }
  });

  it("threads const bindings through both the arm and the catch-all", () => {
    const source = `
      export function f(n: number): number {
        const doubled = n + n;
        if (doubled < 0) return 0;
        return doubled;
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
      assert.equal(
        ast.strExpr(prop.rhs),
        "cond n + n < 0 => 0, true => n + n",
      );
    }
  });

  it("supports an arm between two const bindings", () => {
    const source = `
      export function f(n: number): number {
        const a = n + 1;
        if (a < 0) return 0;
        const b = a + a;
        return b;
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
      assert.equal(
        ast.strExpr(prop.rhs),
        "cond n + 1 < 0 => 0, true => n + 1 + (n + 1)",
      );
    }
  });

  it("supports multiple arms — first match wins", () => {
    const source = `
      export function f(n: number): number {
        if (n < 0) return -1;
        if (n === 0) return 0;
        return n + 1;
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
      assert.equal(
        ast.strExpr(prop.rhs),
        "cond n < 0 => -1, n = 0 => 0, true => n + 1",
      );
    }
  });

  it("composes arms with a μ-search prelude", () => {
    const source = `
      export function f(n: number, used: ReadonlySet<number>): number {
        if (n < 0) return 0;
        let j = 1;
        while (used.has(j)) { j++; }
        return j;
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
      assert.match(ast.strExpr(prop.rhs), /^cond n < 0 => 0, true => /u);
      assert.match(ast.strExpr(prop.rhs), /min over each j\d*: Int/u);
    }
  });

  it("rejects an if-with-multi-statement body in prelude position", () => {
    const source = `
      export function f(n: number): number {
        if (n < 0) {
          const x = 1;
          return x;
        }
        return n;
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
    if (props[0]?.kind === "unsupported") {
      assert.match(props[0].reason, /single return statement/u);
    }
  });

  it("rejects an if-with-else in non-final position", () => {
    const source = `
      export function f(n: number): number {
        if (n < 0) {
          return 0;
        } else {
          return -n;
        }
        return n;
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
    if (props[0]?.kind === "unsupported") {
      assert.match(props[0].reason, /if-with-else only supported as the final/u);
    }
  });

  it("rejects an arm whose value references a later binding (TDZ)", () => {
    const source = `
      export function f(n: number): number {
        if (n < 0) return later;
        const later = n + 1;
        return later;
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

  it("rejects early-return + record return (per-field cond decomposition not yet supported)", () => {
    const source = `
      interface Pair { a: number; b: number }
      export function f(n: number): Pair {
        if (n < 0) return { a: 0, b: 0 };
        return { a: n, b: n + 1 };
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
    if (props[0]?.kind === "unsupported") {
      assert.match(
        props[0].reason,
        /record return combined with early-return arms or if\/else branches/u,
      );
    }
  });

  it("rejects record-shaped arm value with non-literal terminal", () => {
    const source = `
      interface Pair { a: number; b: number }
      declare function makePair(n: number): Pair;
      export function f(n: number): Pair {
        if (n < 0) return { a: 0, b: 0 };
        return makePair(n);
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
    if (props[0]?.kind === "unsupported") {
      assert.match(
        props[0].reason,
        /record return combined with early-return arms or if\/else branches/u,
      );
    }
  });

  it("rejects record-shaped if/else terminal branches", () => {
    const source = `
      interface Pair { a: number; b: number }
      export function f(n: number): Pair {
        if (n < 0) {
          return { a: 0, b: 0 };
        } else {
          return { a: n, b: n + 1 };
        }
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
    if (props[0]?.kind === "unsupported") {
      assert.match(
        props[0].reason,
        /record return combined with early-return arms or if\/else branches/u,
      );
    }
  });

  it("emits trailing-if diagnostic for single `if (P) return E;` body", () => {
    const source = `
      export function f(n: number): number {
        if (n < 0) return 0;
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
    if (props[0]?.kind === "unsupported") {
      assert.match(props[0].reason, /if-without-else as final statement/u);
    }
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
      assert.equal(ast.strExpr(prop.rhs), "to-upper-case s");
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
    assert.equal(ast.strExpr(eq.lhs), "account--balance' a");
    // Early return path keeps pre-state identity; fall-through path writes 1.
    assert.equal(ast.strExpr(eq.rhs), "cond g => account--balance a, true => 1");
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
    assert.equal(ast.strExpr(eq.lhs), "account--balance' a");
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
    assert.equal(ast.strExpr(eq.lhs), "user--active' u");
    assert.equal(
      ast.strExpr(eq.rhs),
      "cond user--active u => false, true => true",
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
    assert.equal(ast.strExpr(eq.lhs), "account--balance' a");
    assert.equal(ast.strExpr(eq.rhs), "account--balance a + amount");
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
    assert.equal(ast.strExpr(eq.lhs), "account--balance' a");
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
    assert.equal(ast.strExpr(eq.lhs), "account--balance' a");
    // Continuation-path (g true) writes v; early-exit (g false) preserves pre-state.
    assert.equal(ast.strExpr(eq.rhs), "cond g => v, true => account--balance a");
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
    assert.deepEqual(lhsStrings.sort(), ["account--balance' a", "account--owner' a"]);
    // Each branch should use the pre-state identity in its untouched arm.
    const balanceEq = equations.find(
      (e) => e.kind === "equation" && ast.strExpr(e.lhs) === "account--balance' a",
    );
    const ownerEq = equations.find(
      (e) => e.kind === "equation" && ast.strExpr(e.lhs) === "account--owner' a",
    );
    if (balanceEq?.kind === "equation") {
      assert.equal(
        ast.strExpr(balanceEq.rhs),
        "cond g => 0, true => account--balance a",
      );
    }
    if (ownerEq?.kind === "equation") {
      assert.equal(
        ast.strExpr(ownerEq.rhs),
        "cond g => account--owner a, true => new-owner",
      );
    }
  });
});

describe("structured iteration (for-of, forEach, reduce)", () => {
  it("Shape A: uniform iterator write emits `all x in arr | p' x = v`", () => {
    const source = `
      interface User { active: boolean; }
      function f(us: User[]): void {
        for (const u of us) { u.active = true; }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const eqs = props.filter((p) => p.kind === "equation");
    const loopEq = eqs.find(
      (e) => e.kind === "equation" && (e.guards?.length ?? 0) > 0,
    );
    assert.ok(loopEq, "expected one loop equation with a guard");
    if (loopEq?.kind === "equation") {
      const ast = getAst();
      const rendered = ast.strExpr(
        ast.forall([], loopEq.guards ?? [], ast.var("__body__")),
      );
      assert.match(rendered, /all u in us \| /);
      assert.equal(ast.strExpr(loopEq.lhs), "user--active' u");
      assert.equal(ast.strExpr(loopEq.rhs), "true");
    }
  });

  it("Shape A: conditional iterator write delegates to symbolic-execute", () => {
    const source = `
      interface User { active: boolean; score: number; }
      function f(us: User[]): void {
        for (const u of us) {
          if (u.score > 0) { u.active = true; }
        }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const eqs = props.filter((p) => p.kind === "equation");
    const loopEq = eqs.find(
      (e) => e.kind === "equation" && (e.guards?.length ?? 0) > 0,
    );
    assert.ok(loopEq);
    if (loopEq?.kind === "equation") {
      const ast = getAst();
      assert.equal(
        ast.strExpr(loopEq.rhs),
        "cond user--score u > 0 => true, true => user--active u",
      );
    }
  });

  it("Shape B: += emits `p a + (+ over each x in arr | rhs)`", () => {
    const source = `
      interface Account { total: number; }
      interface Item { value: number; }
      function f(a: Account, xs: Item[]): void {
        for (const x of xs) { a.total += x.value; }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const eqs = props.filter((p) => p.kind === "equation");
    const totalEq = eqs.find(
      (e) =>
        e.kind === "equation" && getAst().strExpr(e.lhs) === "account--total' a",
    );
    assert.ok(totalEq);
    if (totalEq?.kind === "equation") {
      const ast = getAst();
      assert.equal(
        ast.strExpr(totalEq.rhs),
        "account--total a + (+ over each x in xs | item--value x)",
      );
    }
  });

  it("Shape B: guard folds into the comprehension", () => {
    const source = `
      interface Account { total: number; }
      interface Item { value: number; }
      function f(a: Account, xs: Item[]): void {
        for (const x of xs) {
          if (x.value > 0) { a.total += x.value; }
        }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const eqs = props.filter((p) => p.kind === "equation");
    const totalEq = eqs.find(
      (e) =>
        e.kind === "equation" && getAst().strExpr(e.lhs) === "account--total' a",
    );
    assert.ok(totalEq);
    if (totalEq?.kind === "equation") {
      const ast = getAst();
      assert.equal(
        ast.strExpr(totalEq.rhs),
        "account--total a + (+ over each x in xs, item--value x > 0 | item--value x)",
      );
    }
  });

  it("Mixed body emits one Shape A equation and one Shape B equation", () => {
    const source = `
      interface Account { total: number; }
      interface Item { value: number; tagged: boolean; }
      function f(a: Account, xs: Item[]): void {
        for (const x of xs) {
          x.tagged = true;
          a.total += x.value;
        }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const eqs = props.filter((p) => p.kind === "equation");
    const ast = getAst();
    const lhsStrings = eqs.map((e) =>
      e.kind === "equation" ? ast.strExpr(e.lhs) : "",
    );
    assert.ok(lhsStrings.includes("item--tagged' x"));
    assert.ok(lhsStrings.includes("account--total' a"));
  });

  it("rejects simple-assign fold (requires compound assignment)", () => {
    const source = `
      interface Account { total: number; }
      interface Item { value: number; }
      function f(a: Account, xs: Item[]): void {
        for (const x of xs) { a.total = a.total + x.value; }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    assert.ok(props.some((p) => p.kind === "unsupported"));
  });

  it("forEach dispatches to the same loop-body translator", () => {
    const source = `
      interface Account { total: number; }
      interface Item { value: number; }
      function f(a: Account, xs: Item[]): void {
        xs.forEach((x) => { a.total += x.value; });
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const eqs = props.filter((p) => p.kind === "equation");
    const totalEq = eqs.find(
      (e) =>
        e.kind === "equation" && getAst().strExpr(e.lhs) === "account--total' a",
    );
    assert.ok(totalEq);
    if (totalEq?.kind === "equation") {
      const ast = getAst();
      assert.equal(
        ast.strExpr(totalEq.rhs),
        "account--total a + (+ over each x in xs | item--value x)",
      );
    }
  });

  it("Shape C: reduce elides init when it equals combiner identity", () => {
    const source = `
      interface Item { value: number; }
      function f(xs: Item[]): number {
        return xs.reduce((a, x) => a + x.value, 0);
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const eqs = props.filter((p) => p.kind === "equation");
    assert.equal(eqs.length, 1);
    const ast = getAst();
    if (eqs[0]?.kind === "equation") {
      assert.equal(
        ast.strExpr(eqs[0].rhs),
        "+ over each $0 in xs | item--value $0",
      );
    }
  });

  it("Shape C: reduce preserves non-identity init", () => {
    const source = `
      interface Item { value: number; }
      function f(xs: Item[]): number {
        return xs.reduce((a, x) => a + x.value, 100);
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const eqs = props.filter((p) => p.kind === "equation");
    const ast = getAst();
    if (eqs[0]?.kind === "equation") {
      assert.equal(
        ast.strExpr(eqs[0].rhs),
        "100 + (+ over each $0 in xs | item--value $0)",
      );
    }
  });

  it("Shape C: reduce rejects implicit-init form", () => {
    const source = `
      interface Item { value: number; }
      function f(xs: Item[]): number {
        return xs.reduce((a, x) => a + x.value);
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    assert.ok(props.some((p) => p.kind === "unsupported"));
  });

  it("Shape C: reduceRight rejects non-commutative operators", () => {
    const source = `
      interface Item { value: number; }
      function f(xs: Item[]): number {
        return xs.reduceRight((a, x) => a - x.value, 0);
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    assert.ok(props.some((p) => p.kind === "unsupported"));
  });

  it("rejects Shape A loop in an early-exit continuation", () => {
    // Shape A equations emit directly into `propositions`, not `state.writes`.
    // The early-exit merge path only threads guards through `state.writes`,
    // so accepting this would silently drop the loop equation while leaving
    // `state.modifiedProps` populated (suppressing the frame). Reject instead.
    const source = `
      interface User { active: boolean; }
      function f(g: boolean, us: User[]): void {
        if (g) return;
        for (const u of us) { u.active = true; }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    assert.ok(
      props.some((p) => p.kind === "unsupported"),
      "expected an unsupported marker",
    );
  });

  it("Shape B reads observe Shape A writes in the same iteration", () => {
    const source = `
      interface Account { total: number; }
      interface Item { value: number; }
      function f(a: Account, xs: Item[]): void {
        for (const x of xs) {
          x.value = x.value + 1;
          a.total += x.value;
        }
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const ast = getAst();
    const eqs = props.filter((p) => p.kind === "equation");
    const totalEq = eqs.find(
      (e) => e.kind === "equation" && ast.strExpr(e.lhs) === "account--total' a",
    );
    assert.ok(totalEq, "expected a Shape B equation for total");
    if (totalEq?.kind === "equation") {
      // Shape B's `x.value` read must resolve through the in-iteration
      // Shape A write `value' x = value x + 1`, not the pre-state `value x`.
      assert.equal(
        ast.strExpr(totalEq.rhs),
        "account--total a + (+ over each x in xs | item--value x + 1)",
      );
    }
  });

  it("Shape C: reduce elides parenthesized/signed-zero init variants", () => {
    const variants = ["(0)", "0.0", "+0", "-0", " 0 "];
    for (const initText of variants) {
      const source = `
        interface Item { value: number; }
        function f(xs: Item[]): number {
          return xs.reduce((a, x) => a + x.value, ${initText});
        }
      `;
      const sourceFile = createSourceFileFromSource(source);
      const props = translateBody({
        sourceFile,
        functionName: "f",
        strategy: IntStrategy,
      });
      const eqs = props.filter((p) => p.kind === "equation");
      assert.equal(eqs.length, 1, `variant ${initText}: expected 1 equation`);
      const ast = getAst();
      if (eqs[0]?.kind === "equation") {
        assert.equal(
          ast.strExpr(eqs[0].rhs),
          "+ over each $0 in xs | item--value $0",
          `variant ${initText}: init should have been elided`,
        );
      }
    }
  });
});

describe("Kleene μ-search (while-loop minimum)", () => {
  it("translates `let i = INIT; while (P(i)) i++` as `min over each j: Int, j >= INIT, ~P(j) | j`", () => {
    const source = `
      export function firstUnused(used: ReadonlySet<number>): number {
        let i = 1;
        while (used.has(i)) {
          i++;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "firstUnused",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      // Match any `jN` binder and assert consistent use throughout the
      // comprehension. The specific `N` depends on UniqueSupply slot
      // consumption and shouldn't break hygiene refactors.
      assert.match(
        ast.strExpr(prop.rhs),
        /^min over each (j\d+): Int, \1 >= 1, ~\(\1 in used\) \| \1$/,
      );
    }
  });

  it("inlines the μ-result into downstream expressions", () => {
    const source = `
      export function nextSlot(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) {
          i++;
        }
        return i + 1;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "nextSlot",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.match(
        ast.strExpr(prop.rhs),
        /^\(min over each (j\d+): Int, \1 >= 0, ~\(\1 in used\) \| \1\) \+ 1$/,
      );
    }
  });

  it("composes μ-search with leading const bindings via shared inlining", () => {
    const source = `
      export function offset(used: ReadonlySet<number>, k: number): number {
        const base = k;
        let i = 1;
        while (used.has(i)) {
          i++;
        }
        return base + i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "offset",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.match(
        ast.strExpr(prop.rhs),
        /^k \+ \(min over each (j\d+): Int, \1 >= 1, ~\(\1 in used\) \| \1\)$/,
      );
    }
  });

  it("rejects while bodies with more than one statement", () => {
    const source = `
      export function compound(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) {
          i++;
          i++;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "compound",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });

  it("rejects when the loop body increments a different variable", () => {
    const source = `
      export function aliased(used: ReadonlySet<number>): number {
        let i = 0;
        let j = 0;
        while (used.has(i)) {
          j++;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "aliased",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });

  it("rejects when the counter is `const` instead of `let`", () => {
    // Body contains a canonical `i++` so the rejection is specifically
    // attributable to the `const` declarator, not to the body shape.
    const source = `
      export function constCounter(used: ReadonlySet<number>): number {
        const i = 0;
        while (used.has(i)) {
          i++;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "constCounter",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });

  it("rejects when no `let` precedes the while", () => {
    const source = `
      export function bareWhile(used: ReadonlySet<number>): number {
        while (used.has(0)) {}
        return 0;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "bareWhile",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });

  it("accepts prefix `++counter` as well as postfix `counter++`", () => {
    const source = `
      export function prefixInc(used: ReadonlySet<number>): number {
        let i = 1;
        while (used.has(i)) {
          ++i;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "prefixInc",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    const prop = props[0]!;
    assert.equal(prop.kind, "equation");
    if (prop.kind === "equation") {
      const ast = getAst();
      assert.match(
        ast.strExpr(prop.rhs),
        /^min over each (j\d+): Int, \1 >= 1, ~\(\1 in used\) \| \1$/,
      );
    }
  });

  it("rejects when the initializer has side effects", () => {
    // `start++` in the init would otherwise lower to a bogus var since
    // `translateBodyExpr` has no handler for bare `++`/`--`. The TDZ
    // phase in inlineConstBindings catches this explicitly.
    const source = `
      export function bogusInit(used: ReadonlySet<number>, start: number): number {
        let i = start++;
        while (used.has(i)) {
          i++;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "bogusInit",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });

  it("rejects when the predicate has side effects", () => {
    // `used.has(i++)` embeds a `++` inside the predicate — the TDZ
    // side-effect screen short-circuits before translation rather than
    // silently lowering `i++` via the `ast.var(getText())` fallback.
    const source = `
      export function bogusPred(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i++)) {
          i++;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "bogusPred",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });

  it("rejects when the predicate does not reference the counter", () => {
    // `while (Q) i++` with Q free of i is not a μ-search: it is a no-op
    // (Q false) or a divergence (Q true). Lowering it as
    // `min over each j | ~Q` would change behavior in the divergent case.
    const source = `
      export function bogus(used: ReadonlySet<number>, flag: boolean): number {
        let i = 1;
        while (flag) {
          i++;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "bogus",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });

  it("rejects RealStrategy because the dense domain breaks `counter++`", () => {
    // μ-search enumerates `INIT, INIT+1, …` discretely. Under
    // RealStrategy the comprehension would range over a dense domain
    // and could return a value the loop never visits (e.g. √2 when
    // `while (i * i < 2) i++` should terminate at 2).
    const source = `
      export function overReal(used: ReadonlySet<number>): number {
        let i = 0;
        while (used.has(i)) {
          i++;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "overReal",
      strategy: RealStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });

  it("counter references inside shadowing arrow params do not count as free refs", () => {
    // `[1].some(i => ...)` shadows the outer counter `i`. The predicate
    // does NOT reference the outer counter, so recognizeMuSearch rejects
    // the shape rather than falsely lowering it to a μ-search whose
    // search-domain guard is free of `j`.
    const source = `
      export function shadowed(): number {
        let i = 0;
        while ([1, 2, 3].some(i => i > 0)) {
          i++;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "shadowed",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
  });

  it("destructured arrow params also shadow the outer counter", () => {
    // Object/array patterns bind identifiers just as plain params do.
    // The scope-aware check must descend into binding patterns to
    // discover `i` is shadowed by `({ i })` or `([i])`.
    const objSource = `
      export function shadowedObj(): number {
        let i = 0;
        while ([{ i: 0 }].some(({ i }) => i > 0)) {
          i++;
        }
        return i;
      }
    `;
    const arrSource = `
      export function shadowedArr(): number {
        let i = 0;
        while ([[0]].some(([i]) => i > 0)) {
          i++;
        }
        return i;
      }
    `;
    for (const [name, src] of [
      ["shadowedObj", objSource],
      ["shadowedArr", arrSource],
    ] as const) {
      const sourceFile = createSourceFileFromSource(src);
      const props = translateBody({
        sourceFile,
        functionName: name,
        strategy: IntStrategy,
      });
      assert.equal(props.length, 1);
      assert.equal(props[0]?.kind, "unsupported", `${name} should be rejected`);
    }
  });

  it("computed destructuring key in a nested param sees the outer counter", () => {
    // `({ [i]: x }) => …` — the computed key `[i]` is evaluated in the
    // outer scope before the `x` binding takes effect, so it IS a real
    // free reference to the outer counter `i`. The recognizer must
    // accept this as a valid μ-search body. After the reshape, `used`
    // is declared-but-unused, so the key also supplies the counter
    // reference that the predicate-must-reference-counter check looks
    // for.
    const source = `
      export function computedKeyRef(): number {
        let i = 0;
        while ([{}].some(({ [i]: x }: { [k: number]: number }) => x === 0)) {
          i++;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "computedKeyRef",
      strategy: IntStrategy,
    });

    // A free-ref to `i` via the computed key makes this a recognized
    // μ-search shape; regardless of whether downstream translation of
    // the indexed-record predicate succeeds, the distinguishing
    // property is that the recognizer does NOT reject it for "predicate
    // does not reference counter".
    assert.equal(props.length, 1);
    if (props[0]?.kind === "unsupported") {
      assert.doesNotMatch(
        props[0].reason,
        /predicate does not reference the counter|not a recognized μ-search/,
      );
    }
  });

  it("named function expression's own name shadows the outer counter", () => {
    // `function i() {}` binds `i` inside its own body (for recursive
    // self-reference), so the reference to `i` inside the body refers
    // to the function value, not to the outer counter. The predicate
    // therefore has no free reference to the outer `i`. After M2's
    // cutover the TS-AST recognizer is purely structural; the
    // shadowing rejection comes from either the L1 predicate-
    // references-counter check or the legacy purity check (the IIFE
    // call is conservatively classified as effectful).
    const source = `
      export function shadowedFnName(): number {
        let i = 0;
        while ((function i(): boolean { return i.length > 0; })()) {
          i++;
        }
        return i;
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "shadowedFnName",
      strategy: IntStrategy,
    });

    assert.equal(props.length, 1);
    assert.equal(props[0]?.kind, "unsupported");
    if (props[0]?.kind === "unsupported") {
      // The rejection must be attributable to the shadowing — either
      // via the predicate-not-referencing-counter check or via the
      // purity oracle classifying the IIFE call as effectful. Both
      // are valid signals that the loop is not a clean μ-search.
      assert.match(
        props[0].reason,
        /predicate does not reference the counter|predicate has side effects/,
      );
    }
  });

  it("object/class method scopes shadow outer bindings", () => {
    // `({ test(i) { return i > 0; } }).test(0)` — the method's
    // parameter `i` shadows the outer counter. The predicate has no
    // free reference to the outer `i`, so the recognizer rejects.
    const methodSource = `
      export function shadowedMethod(): number {
        let i = 0;
        while (({ test(i: number): boolean { return i > 0; } }).test(0)) {
          i++;
        }
        return i;
      }
    `;
    // `({ i() { return 0; } }).i()` — the method name `i` is a
    // syntactic key, not a free reference.
    const methodNameSource = `
      export function shadowedMethodName(): number {
        let i = 0;
        while (({ i(): number { return 0; } }).i()) {
          i++;
        }
        return i;
      }
    `;
    for (const [name, src] of [
      ["shadowedMethod", methodSource],
      ["shadowedMethodName", methodNameSource],
    ] as const) {
      const sourceFile = createSourceFileFromSource(src);
      const props = translateBody({
        sourceFile,
        functionName: name,
        strategy: IntStrategy,
      });
      assert.equal(props.length, 1);
      assert.equal(props[0]?.kind, "unsupported", `${name} should be rejected`);
      if (props[0]?.kind === "unsupported") {
        assert.match(
          props[0].reason,
          /predicate does not reference the counter|predicate has side effects/,
          `${name} should reject on shadowing or purity path`,
        );
      }
    }
  });
});

describe("Set mutation (Stage A: interface-field .add / .delete / .clear)", () => {
  it("later-wins: add(x) then delete(x) drops the add arm", () => {
    const source = `
      interface Tagged { tags: Set<string>; }
      function f(c: Tagged, x: string): void {
        c.tags.add(x);
        c.tags.delete(x);
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const assertions = props.filter((p) => p.kind === "assertion");
    assert.equal(assertions.length, 1);
    const ast = getAst();
    const a = assertions[0];
    if (a?.kind === "assertion") {
      // Only the final `.delete(x)` arm remains — the prior add was
      // filtered by installSetWrite's later-wins per-element dedupe.
      // Binder name varies by supply (`$0` without synthCell, `y` with);
      // match shape with a regex.
      const bodyText = ast.strExpr(a.body);
      assert.match(
        bodyText,
        /^\S+ in tagged--tags' c <-> \(cond \S+ = x => false, true => \S+ in tagged--tags c\)$/,
        `unexpected body shape: ${bodyText}`,
      );
    }
  });

  it("clear resets overrides and drops pre-state fallthrough", () => {
    const source = `
      interface Tagged { tags: Set<string>; }
      function f(c: Tagged): void {
        c.tags.clear();
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const assertions = props.filter((p) => p.kind === "assertion");
    assert.equal(assertions.length, 1);
    const ast = getAst();
    const a = assertions[0];
    if (a?.kind === "assertion") {
      // `.clear()` alone → all y | y in tags' c <-> false
      // (semantically the empty Set, matching the empty-Set-initializer
      // form from translate-record.ts but via the mutation pipeline).
      const bodyText = ast.strExpr(a.body);
      assert.match(
        bodyText,
        /^\S+ in tagged--tags' c <-> false$/,
        `unexpected body shape: ${bodyText}`,
      );
    }
  });

  it("parameter-level Set mutation is rejected with specific reason", () => {
    const source = `
      function f(s: Set<string>, x: string): void {
        s.add(x);
      }
    `;
    const sourceFile = createSourceFileFromSource(source);
    const props = translateBody({
      sourceFile,
      functionName: "f",
      strategy: IntStrategy,
    });
    const unsupported = props.find((p) => p.kind === "unsupported");
    assert.ok(unsupported);
    if (unsupported?.kind === "unsupported") {
      assert.equal(unsupported.reason, "parameter-level Set mutation");
    }
  });
});
