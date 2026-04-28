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
    throw new Error(`expected function ${functionName} to return an expression`);
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
    case "ir-wrap":
      return false;
  }
}

function expectNoIRWrap(expr: IRExpr): void {
  assert.equal(containsKind(expr, "ir-wrap"), false);
}

describe("ir-build M6 native construction stubs", () => {
  // M6 Patch 2 unskips this after ordinary arithmetic/comparison
  // binary expressions build as native App(binop, ...) nodes.
  it("builds arithmetic and comparison without IRWrap", () => {
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
    expectNoIRWrap(arithmetic);
    expectNoIRWrap(comparison);
  });

  // M6 Patch 2 unskips this after general pure calls build as native
  // App nodes instead of delegating through `translateBodyExpr`.
  it("builds general calls without IRWrap", () => {
    const ir = expectIR(`
      function score(a: number, b: number): number {
        return a + b;
      }
      function f(a: number, b: number): number {
        return score(a, b);
      }
    `);
    assert.equal(ir.kind, "app");
    expectNoIRWrap(ir);
  });

  // M6 Patch 2 unskips this after optional/nullish lowering avoids
  // L1 `from-l2` and native IR carries the complete conditional shape.
  it("builds optional chains and nullish coalescing without IRWrap", () => {
    const optional = expectIR(`
      interface Owner { readonly id: number; }
      interface Account { readonly owner?: Owner; }
      function f(a: Account): number[] {
        return a.owner?.id;
      }
    `);
    const nullish = expectIR(`
      function f(x: number | null | undefined): number {
        return x ?? 0;
      }
    `);
    expectNoIRWrap(optional);
    expectNoIRWrap(nullish);
  });

  // M6 Patch 2 unskips this after chain fusion constructs real Each
  // and Comb nodes rather than preserving legacy opaque comprehensions.
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
    expectNoIRWrap(each);
    expectNoIRWrap(comb);
  });

  // M6 Patch 2 unskips the string-literal optional-element case; the
  // computed-key case should remain unsupported through M6.
  it("records optional element access boundary", () => {
    const literalKey = expectIR(`
      interface User { readonly name: string; }
      function f(u: User | null): string[] {
        return u?.["name"];
      }
    `);
    expectNoIRWrap(literalKey);

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

  // M6 Patch 2 keeps computed element access outside the cleanup
  // boundary unless the key is syntactically a string literal.
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
