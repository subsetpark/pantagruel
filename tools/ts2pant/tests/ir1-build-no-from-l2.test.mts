import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import ts from "typescript";
import {
  buildL1AssignStmt,
  buildL1ForEachCall,
  buildL1ForOfMutation,
  buildL1IfMutation,
  isUnsupported,
  type BuildBodyCtx,
} from "../src/ir1-build-body.js";
import type { IR1Expr, IR1FoldLeaf, IR1Stmt } from "../src/ir1.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { loadAst } from "../src/pant-wasm.js";
import {
  makeSymbolicState,
  type SymbolicState,
  type UniqueSupply,
} from "../src/translate-body.js";
import { IntStrategy, newSynthCell } from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

interface FunctionSetup {
  fn: ts.FunctionDeclaration;
  ctx: BuildBodyCtx;
}

function setupFunction(source: string, functionName = "f"): FunctionSetup {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements.find(
    (stmt): stmt is ts.FunctionDeclaration =>
      ts.isFunctionDeclaration(stmt) && stmt.name?.text === functionName,
  );
  if (!fn?.body) {
    throw new Error(`expected function ${functionName} with a body`);
  }
  const paramNames = new Map<string, string>();
  for (const param of fn.parameters) {
    if (ts.isIdentifier(param.name)) {
      paramNames.set(param.name.text, param.name.text);
    }
  }
  const state: SymbolicState = makeSymbolicState();
  const supply: UniqueSupply = { n: 0, synthCell: newSynthCell() };
  return {
    fn,
    ctx: {
      checker,
      strategy: IntStrategy,
      paramNames,
      state,
      supply,
      applyConst: (expr) => expr,
    },
  };
}

function firstStatement<T extends ts.Statement>(
  source: string,
  guard: (stmt: ts.Statement) => stmt is T,
): { stmt: T; ctx: BuildBodyCtx } {
  const { fn, ctx } = setupFunction(source);
  const stmt = fn.body?.statements.find(guard);
  if (!stmt) {
    throw new Error("expected matching statement in function body");
  }
  return { stmt, ctx };
}

function containsFromL2Expr(expr: IR1Expr): boolean {
  switch (expr.kind) {
    case "from-l2":
      return true;
    case "binop":
      return containsFromL2Expr(expr.lhs) || containsFromL2Expr(expr.rhs);
    case "unop":
      return containsFromL2Expr(expr.arg);
    case "app":
      return (
        containsFromL2Expr(expr.callee) ||
        expr.args.some((arg) => containsFromL2Expr(arg))
      );
    case "member":
      return containsFromL2Expr(expr.receiver);
    case "cond":
      return (
        expr.arms.some(
          ([guard, value]) =>
            containsFromL2Expr(guard) || containsFromL2Expr(value),
        ) || containsFromL2Expr(expr.otherwise)
      );
    case "is-nullish":
      return containsFromL2Expr(expr.operand);
    case "var":
    case "lit":
      return false;
  }
}

function containsFromL2FoldLeaf(leaf: IR1FoldLeaf): boolean {
  return (
    containsFromL2Expr(leaf.target) ||
    containsFromL2Expr(leaf.rhs) ||
    (leaf.guard !== null && containsFromL2Expr(leaf.guard))
  );
}

function containsFromL2Stmt(stmt: IR1Stmt): boolean {
  switch (stmt.kind) {
    case "block":
      return stmt.stmts.some((child) => containsFromL2Stmt(child));
    case "let":
      return containsFromL2Expr(stmt.value);
    case "assign":
      return containsFromL2Expr(stmt.target) || containsFromL2Expr(stmt.value);
    case "cond-stmt":
      return (
        stmt.arms.some(
          ([guard, body]) =>
            containsFromL2Expr(guard) || containsFromL2Stmt(body),
        ) ||
        (stmt.otherwise !== null && containsFromL2Stmt(stmt.otherwise))
      );
    case "foreach":
      return (
        containsFromL2Expr(stmt.source) ||
        (stmt.body !== null && containsFromL2Stmt(stmt.body)) ||
        stmt.foldLeaves.some((leaf) => containsFromL2FoldLeaf(leaf))
      );
    case "for":
      return (
        (stmt.init !== null && containsFromL2Stmt(stmt.init)) ||
        (stmt.cond !== null && containsFromL2Expr(stmt.cond)) ||
        (stmt.step !== null && containsFromL2Stmt(stmt.step)) ||
        containsFromL2Stmt(stmt.body)
      );
    case "while":
      return containsFromL2Expr(stmt.cond) || containsFromL2Stmt(stmt.body);
    case "return":
      return stmt.expr !== null && containsFromL2Expr(stmt.expr);
    case "throw":
      return containsFromL2Expr(stmt.expr);
    case "expr-stmt":
      return containsFromL2Expr(stmt.expr);
    case "map-effect":
      return (
        containsFromL2Expr(stmt.objExpr) ||
        containsFromL2Expr(stmt.keyExpr) ||
        (stmt.valueExpr !== null && containsFromL2Expr(stmt.valueExpr))
      );
    case "set-effect":
      return (
        containsFromL2Expr(stmt.objExpr) ||
        (stmt.elemExpr !== null && containsFromL2Expr(stmt.elemExpr))
      );
  }
}

function expectNoFromL2(stmt: IR1Stmt): void {
  assert.equal(containsFromL2Stmt(stmt), false);
}

describe("ir1-build-body M6 no-from-l2 stubs", () => {
  // M6 Patch 4 unskips this after mutating if guards use native L1
  // builders or return targeted unsupported results.
  it.skip("builds if guards without from-l2", () => {
    const { stmt, ctx } = firstStatement(
      `
      interface Account { balance: number; }
      function f(a: Account, amount: number): void {
        if (amount > 0) {
          a.balance = amount;
        }
      }
      `,
      ts.isIfStatement,
    );
    const built = buildL1IfMutation(stmt, ctx);
    assert.ok(!isUnsupported(built));
    if (!isUnsupported(built)) {
      expectNoFromL2(built);
    }
  });

  // M6 Patch 4 unskips this after foreach sources build as native L1
  // expressions instead of `from-l2(ir-wrap(...))`.
  it.skip("builds foreach sources without from-l2", () => {
    const { stmt, ctx } = firstStatement(
      `
      interface Item { value: number; }
      interface Account { total: number; }
      function f(items: Item[], account: Account): void {
        for (const item of items) {
          account.total += item.value;
        }
      }
      `,
      ts.isForOfStatement,
    );
    const built = buildL1ForOfMutation(stmt, ctx);
    assert.ok(!isUnsupported(built));
    if (!isUnsupported(built)) {
      expectNoFromL2(built);
    }
  });

  // M6 Patch 4 unskips this after Shape B rhs/guard sub-expressions
  // build natively under the per-iteration L1 context.
  it.skip("builds Shape B rhs and guard without from-l2", () => {
    const { stmt, ctx } = firstStatement(
      `
      interface Item { value: number; active: boolean; }
      interface Account { total: number; }
      function f(items: Item[], account: Account): void {
        items.forEach((item) => {
          if (item.active) {
            account.total += item.value + 1;
          }
        });
      }
      `,
      (stmt): stmt is ts.ExpressionStatement =>
        ts.isExpressionStatement(stmt) && ts.isCallExpression(stmt.expression),
    );
    const built = buildL1ForEachCall(stmt.expression, ctx);
    assert.ok(!isUnsupported(built));
    if (!isUnsupported(built)) {
      expectNoFromL2(built);
    }
  });

  // M6 Patch 4 unskips this after assignment RHS construction stops
  // delegating through `translateBodyExpr` for supported expressions.
  it.skip("builds Shape A rhs without from-l2", () => {
    const { stmt, ctx } = firstStatement(
      `
      interface Account { balance: number; }
      function f(a: Account, amount: number): void {
        a.balance = amount + 1;
      }
      `,
      ts.isExpressionStatement,
    );
    const built = buildL1AssignStmt(stmt, ctx);
    assert.ok(!isUnsupported(built));
    if (!isUnsupported(built)) {
      expectNoFromL2(built);
    }
  });

  // M6 Patch 4 unskips this after Map mutation object/key/value
  // payloads build as native L1 forms or reject explicitly.
  it.skip("builds map object/key/value payloads without from-l2", () => {
    const { stmt, ctx } = firstStatement(
      `
      function f(m: Map<string, number>, suffix: string, value: number): void {
        if (value > 0) {
          m.set("k" + suffix, value + 1);
        }
      }
      `,
      ts.isIfStatement,
    );
    const built = buildL1IfMutation(stmt, ctx);
    assert.ok(!isUnsupported(built));
    if (!isUnsupported(built)) {
      expectNoFromL2(built);
    }
  });

  // M6 Patch 4 unskips this after Set mutation object/element payloads
  // build as native L1 forms or reject explicitly.
  it.skip("builds set object/element payloads without from-l2", () => {
    const { stmt, ctx } = firstStatement(
      `
      function f(s: Set<string>, suffix: string): void {
        if (suffix !== "") {
          s.add("k" + suffix);
        }
      }
      `,
      ts.isIfStatement,
    );
    const built = buildL1IfMutation(stmt, ctx);
    assert.ok(!isUnsupported(built));
    if (!isUnsupported(built)) {
      expectNoFromL2(built);
    }
  });
});
