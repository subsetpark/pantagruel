import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import { buildIR, isBuildUnsupported } from "../src/ir-build.js";
import type { IRExpr } from "../src/ir.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { loadAst } from "../src/pant-wasm.js";
import { IntStrategy, newSynthCell } from "../src/translate-types.js";
import type { UniqueSupply } from "../src/translate-body.js";

before(async () => {
  await loadAst();
});

interface ExprSetup {
  expr: ts.Expression;
  checker: ts.TypeChecker;
  paramNames: ReadonlyMap<string, string>;
  supply: UniqueSupply;
}

function setupReturnExpr(source: string, functionName = "f"): ExprSetup {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements.find(
    (stmt): stmt is ts.FunctionDeclaration =>
      ts.isFunctionDeclaration(stmt) && stmt.name?.text === functionName,
  );
  if (!fn?.body) {
    throw new Error(`expected function ${functionName} with a body`);
  }
  const stmt = fn.body.statements.find(ts.isReturnStatement);
  if (!stmt?.expression) {
    throw new Error(
      `expected function ${functionName} to return an expression`,
    );
  }
  const paramNames = new Map<string, string>();
  for (const param of fn.parameters) {
    if (ts.isIdentifier(param.name)) {
      paramNames.set(param.name.text, param.name.text);
    }
  }
  return {
    expr: stmt.expression,
    checker,
    paramNames,
    supply: { n: 0, synthCell: newSynthCell() },
  };
}

function buildFromSource(source: string): IRExpr | { unsupported: string } {
  const { expr, checker, paramNames, supply } = setupReturnExpr(source);
  return buildIR(expr, checker, IntStrategy, paramNames, supply);
}

function expectIR(source: string): IRExpr {
  const result = buildFromSource(source);
  if (isBuildUnsupported(result)) {
    throw new Error(`expected IR, got unsupported: ${result.unsupported}`);
  }
  return result;
}

function containsKind(expr: IRExpr, kind: IRExpr["kind"]): boolean {
  if (expr.kind === kind) {
    return true;
  }
  switch (expr.kind) {
    case "app":
      return (
        (expr.head.kind === "expr" && containsKind(expr.head.expr, kind)) ||
        expr.args.some((arg) => containsKind(arg, kind))
      );
    case "cond":
      return (
        expr.arms.some(
          ([guard, value]) =>
            containsKind(guard, kind) || containsKind(value, kind),
        ) ||
        (expr.otherwise !== undefined && containsKind(expr.otherwise, kind))
      );
    case "let":
      return containsKind(expr.value, kind) || containsKind(expr.body, kind);
    case "each":
      return (
        containsKind(expr.src, kind) ||
        expr.guards.some((guard) => containsKind(guard, kind)) ||
        containsKind(expr.proj, kind)
      );
    case "comb":
      return (
        containsKind(expr.each, kind) ||
        ("init" in expr &&
          expr.init !== undefined &&
          containsKind(expr.init, kind))
      );
    case "comb-typed":
      return (
        expr.guards.some((guard) => containsKind(guard, kind)) ||
        containsKind(expr.proj, kind)
      );
    case "forall":
    case "exists":
      return (
        (expr.guard !== undefined && containsKind(expr.guard, kind)) ||
        containsKind(expr.body, kind)
      );
    case "var":
    case "lit":
      return false;
  }
}

describe("ir-build native construction", () => {
  // Ordinary arithmetic/comparison binary expressions build as native
  // App(binop, ...) nodes.
  it("builds arithmetic and comparison natively", () => {
    const arithmetic = expectIR(`
      function f(a: number, b: number): number {
        return (a + b) * 2;
      }
    `);
    const comparison = expectIR(`
      function f(a: number, b: number): boolean {
        return a <= b;
      }
    `);
    assert.equal(arithmetic.kind, "app");
    assert.equal(comparison.kind, "app");
  });

  // General pure calls build as native App nodes.
  it("builds general calls natively", () => {
    const ir = expectIR(`
      function score(a: number, b: number): number {
        return a + b;
      }
      function f(a: number, b: number): number {
        return score(a, b);
      }
    `);
    assert.equal(ir.kind, "app");
  });

  // Optional/nullish lowering produces native conditional shape.
  it("builds optional chains and nullish coalescing natively", () => {
    expectIR(`
      interface Owner { readonly id: number; }
      interface Account { readonly owner?: Owner; }
      function f(a: Account): number[] {
        return a.owner?.id;
      }
    `);
    expectIR(`
      function f(x: number | null | undefined): number {
        return x ?? 0;
      }
    `);
  });

  // Chain fusion constructs real Each and Comb nodes.
  it("builds filter/map/reduce chains as native Each/Comb", () => {
    const each = expectIR(`
      interface User { readonly active: boolean; readonly name: string; }
      function f(users: User[]): string[] {
        return users.filter((u) => u.active).map((u) => u.name);
      }
    `);
    const comb = expectIR(`
      interface Item { readonly amount: number; }
      function f(items: Item[]): number {
        return items.reduce((sum, item) => sum + item.amount, 0);
      }
    `);
    assert.equal(containsKind(each, "each"), true);
    assert.equal(containsKind(comb, "comb"), true);
  });

  it("builds array-chain receivers natively before member/cardinality fallback", () => {
    const length = expectIR(`
      function f(xs: number[]): number {
        return xs.filter((x) => x > 0).length;
      }
    `);
    const indexedLength = expectIR(`
      function f(xs: number[]): number {
        return xs["filter"]((x) => x > 0)["length"];
      }
    `);
    assert.equal(containsKind(length, "each"), true);
    assert.equal(containsKind(indexedLength, "each"), true);
  });

  it("recognizes string-literal array method calls", () => {
    const dotted = expectIR(`
      interface Item { readonly amount: number; }
      function f(items: Item[]): number[] {
        return items.map((item) => item.amount);
      }
    `);
    const indexed = expectIR(`
      interface Item { readonly amount: number; }
      function f(items: Item[]): number[] {
        return items["map"]((item) => item.amount);
      }
    `);
    assert.deepEqual(indexed, dotted);
  });

  it("recognizes tuple and union array-method receivers", () => {
    const tuple = expectIR(`
      function f(items: readonly [number, number]): number[] {
        return items.map((item) => item + 1);
      }
    `);
    const union = expectIR(`
      function f(items: number[] | readonly number[]): number[] {
        return items.map((item) => item + 1);
      }
    `);
    assert.equal(containsKind(tuple, "each"), true);
    assert.equal(containsKind(union, "each"), true);
  });

  it("does not recurse on syntactic array method names for non-array receivers", () => {
    const method = buildFromSource(`
      interface Service { readonly map: (n: number) => number; }
      function f(service: Service, n: number): number {
        return service.map(n);
      }
    `);
    const indexed = buildFromSource(`
      interface Service { readonly reduce: (n: number, init: number) => number; }
      function f(service: Service, n: number): number {
        return service["reduce"](n, 0);
      }
    `);
    assert.equal(isBuildUnsupported(method), false);
    assert.equal(isBuildUnsupported(indexed), false);
    if (!isBuildUnsupported(method)) {
      assert.equal(containsKind(method, "each"), false);
      assert.equal(containsKind(method, "comb"), false);
    }
    if (!isBuildUnsupported(indexed)) {
      assert.equal(containsKind(indexed, "each"), false);
      assert.equal(containsKind(indexed, "comb"), false);
    }
  });

  it("rejects async, defaulted, optional, and rest array callbacks", () => {
    const cases = [
      {
        source: `
          function f(xs: number[]): Promise<boolean>[] {
            return xs.map(async (x) => x > 0);
          }
        `,
        reason: /must not be async/u,
      },
      {
        source: `
          function f(xs: number[]): number[] {
            return xs.map((x = 0) => x + 1);
          }
        `,
        reason: /plain identifiers/u,
      },
      {
        source: `
          function f(xs: number[]): number[] {
            return xs.map((x?: number) => x ?? 0);
          }
        `,
        reason: /plain identifiers/u,
      },
      {
        source: `
          function f(xs: number[]): number[] {
            return xs.map((...x) => x.length);
          }
        `,
        reason: /plain identifiers/u,
      },
    ];
    for (const { source, reason } of cases) {
      const result = buildFromSource(source);
      assert.ok(isBuildUnsupported(result));
      if (isBuildUnsupported(result)) {
        assert.match(result.unsupported, reason);
      }
    }
  });

  it("rejects array-method calls with unsupported signatures", () => {
    const result = buildFromSource(`
      function f(xs: number[]): number[] {
        return xs["filter"]((x) => x > 0, undefined);
      }
    `);
    assert.ok(isBuildUnsupported(result));
    if (isBuildUnsupported(result)) {
      assert.match(
        result.unsupported,
        /callback must have exactly one argument/u,
      );
    }
  });

  it("rejects non-boolean filter predicates", () => {
    const result = buildFromSource(`
      interface Item { readonly id: number; }
      function f(items: Item[]): Item[] {
        return items.filter((item) => item.id);
      }
    `);
    assert.ok(isBuildUnsupported(result));
    if (isBuildUnsupported(result)) {
      assert.match(result.unsupported, /boolean predicate/u);
    }
  });

  it("rejects reducer operators when operand types do not match the combiner", () => {
    const stringConcat = buildFromSource(`
      interface Item { readonly name: string; }
      function f(items: Item[]): string {
        return items.reduce((s, item) => s + item.name, "");
      }
    `);
    assert.ok(isBuildUnsupported(stringConcat));
    if (isBuildUnsupported(stringConcat)) {
      assert.match(stringConcat.unsupported, /number combiner/u);
    }

    const truthyOr = buildFromSource(`
      interface Item { readonly value: number; }
      function f(items: Item[]): number {
        return items.reduce((a, item) => a || item.value, 0);
      }
    `);
    assert.ok(isBuildUnsupported(truthyOr));
    if (isBuildUnsupported(truthyOr)) {
      assert.match(truthyOr.unsupported, /boolean combiner/u);
    }
  });

  it("rejects reduce callbacks that use a property access in the accumulator slot", () => {
    for (const expr of ["sum.total + item.amount", "item.amount + sum.total"]) {
      const result = buildFromSource(`
        interface Item { readonly amount: number; }
        function f(items: Item[]): number {
          return items.reduce((sum, item) => ${expr}, 0);
        }
      `);
      assert.ok(isBuildUnsupported(result));
      if (isBuildUnsupported(result)) {
        assert.match(
          result.unsupported,
          /callback must reference acc exactly once/u,
        );
      }
    }
  });

  it("preserves conditional otherwise during map-chain substitution", () => {
    const ir = expectIR(`
      interface Item { readonly amount: number | null; }
      function f(items: Item[]): number[] {
        return items
          .map((item) => item.amount)
          .map((amount) => amount ?? 0);
      }
    `);
    assert.equal(ir.kind, "each");
    if (ir.kind === "each") {
      assert.equal(ir.proj.kind, "cond");
      if (ir.proj.kind === "cond") {
        assert.notEqual(ir.proj.otherwise, undefined);
      }
    }
  });

  it("normalizes dotted and string-literal method calls identically", () => {
    const dotted = expectIR(`
      interface Service { readonly run: (n: number) => number; }
      function f(s: Service, n: number): number {
        return s.run(n);
      }
    `);
    const indexed = expectIR(`
      interface Service { readonly run: (n: number) => number; }
      function f(s: Service, n: number): number {
        return s["run"](n);
      }
    `);
    assert.deepEqual(indexed, dotted);
    assert.equal(dotted.kind, "app");
    if (dotted.kind === "app") {
      assert.equal(dotted.head.kind, "expr");
      if (dotted.head.kind === "expr") {
        assert.equal(dotted.head.expr.kind, "app");
        if (dotted.head.expr.kind === "app") {
          assert.equal(dotted.head.expr.head.kind, "name");
          if (dotted.head.expr.head.kind === "name") {
            assert.equal(dotted.head.expr.head.name, "service--run");
          }
        }
      }
    }
  });

  // String-literal optional element access lowers the same way as
  // dotted optional access; computed element access remains unsupported.
  it("records optional element access boundary", () => {
    expectIR(`
      interface User { readonly name: string; }
      function f(u: User | null): string[] {
        return u?.["name"];
      }
    `);

    const computedKey = buildFromSource(`
      interface User { readonly name: string; }
      function f(u: User | null, key: keyof User): unknown {
        return u?.[key];
      }
    `);
    assert.ok(isBuildUnsupported(computedKey));
    if (isBuildUnsupported(computedKey)) {
      assert.match(computedKey.unsupported, /computed property access/u);
    }
  });

  // Computed element access stays unsupported unless the key is a
  // string literal or no-substitution template literal.
  it("keeps computed element access unsupported", () => {
    const result = buildFromSource(`
      interface User { readonly name: string; }
      function f(u: User, key: keyof User): unknown {
        return u[key];
      }
    `);
    assert.ok(isBuildUnsupported(result));
    if (isBuildUnsupported(result)) {
      assert.match(result.unsupported, /computed property access/u);
    }
  });
});
