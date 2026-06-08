// @archlint.module test
// @archlint.domain ts2pant.ir1-build

import assert from "node:assert/strict";
import * as fc from "fast-check";
import { before, it } from "node:test";
import ts from "typescript";
import * as B from "../src/ir1-build.js";
import { createSourceFileFromSource, getChecker } from "../src/extract.js";
import { ir1LitNat, ir1Var } from "../src/ir1.js";
import { loadAst } from "../src/pant-wasm.js";
import { IntStrategy, newSynthCell } from "../src/translate-types.js";
import type { MuSearch } from "../src/translate-body.js";

before(async () => {
  await loadAst();
});

const tsKeywords = new Set([
  "abstract",
  "any",
  "as",
  "asserts",
  "async",
  "await",
  "bigint",
  "boolean",
  "break",
  "case",
  "catch",
  "class",
  "const",
  "constructor",
  "continue",
  "declare",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "enum",
  "export",
  "extends",
  "false",
  "finally",
  "for",
  "from",
  "function",
  "get",
  "global",
  "if",
  "implements",
  "import",
  "in",
  "infer",
  "interface",
  "intrinsic",
  "instanceof",
  "is",
  "keyof",
  "let",
  "module",
  "namespace",
  "never",
  "new",
  "null",
  "object",
  "of",
  "out",
  "override",
  "package",
  "private",
  "protected",
  "public",
  "readonly",
  "require",
  "return",
  "satisfies",
  "set",
  "static",
  "string",
  "super",
  "switch",
  "symbol",
  "this",
  "throw",
  "true",
  "try",
  "type",
  "typeof",
  "undefined",
  "unique",
  "unknown",
  "using",
  "var",
  "void",
  "while",
  "with",
  "yield",
]);
const identifierArb = fc
  .stringMatching(/^[a-z][a-z0-9]{0,8}$/)
  .filter((name) => !tsKeywords.has(name));

function firstReturnExpression(source: string): {
  expr: ts.Expression;
  checker: ts.TypeChecker;
} {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.getFunctions()[0]?.compilerNode;
  const stmt = fn?.body?.statements.find(ts.isReturnStatement);
  if (!stmt?.expression) {
    throw new Error("expected return expression");
  }
  return { expr: stmt.expression, checker };
}

function firstWhileSearch(source: string): {
  checker: ts.TypeChecker;
  mu: MuSearch;
} {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.getFunctions()[0]?.compilerNode;
  const statements = fn?.body?.statements;
  const letStmt = statements?.find(ts.isVariableStatement);
  const whileStmt = statements?.find(ts.isWhileStatement);
  const decl = letStmt?.declarationList.declarations[0];
  if (
    !decl ||
    !ts.isIdentifier(decl.name) ||
    !decl.initializer ||
    !whileStmt ||
    !ts.isBlock(whileStmt.statement)
  ) {
    throw new Error("expected let/while search fixture");
  }
  const stepStmt = whileStmt.statement.statements[0];
  if (!stepStmt || !ts.isExpressionStatement(stepStmt)) {
    throw new Error("expected while step expression");
  }
  return {
    checker,
    mu: {
      counterName: decl.name.text,
      initTsExpr: decl.initializer,
      predicateTsExpr: whileStmt.expression,
      stepExpr: stepStmt.expression,
    },
  };
}

it("generated inputs exercise the ir1-build exported surface", () => {
  fc.assert(
    fc.property(identifierArb, (name) => {
      const { expr, checker } = firstReturnExpression(`
        function f(${name}: number): number {
          return (${name} + 1);
        }
      `);
      const keyed = firstReturnExpression(`
        function key(obj: Record<string, number>): number {
          return obj["${name}"];
        }
      `);
      if (!ts.isElementAccessExpression(keyed.expr)) {
        throw new Error("expected element access expression");
      }
      const { checker: muChecker, mu } = firstWhileSearch(`
        function search(${name}: number): number {
          let j = 0;
          while (j < ${name}) {
            j = j + 1;
          }
          return j;
        }
      `);
      const ctx = {
        checker,
        strategy: IntStrategy,
        paramNames: new Map([[name, name]]),
        state: undefined,
        supply: { n: 0, synthCell: newSynthCell() },
        env: B.createL1AssumptionEnv(),
      };
      const unsupported = { unsupported: name };
      assert.equal(B.isL1Unsupported(unsupported), true);
      assert.equal(B.isL1StmtUnsupported(unsupported), true);
      assert.equal(B.isL1ConditionalForm(ir1Var(name)), false);
      assert.equal(B.contagiousOpaqueForOperands(ctx, [ir1Var(name), ir1LitNat(1)], expr), null);
      assert.notEqual(B.snapshotAssumptionEnv(ctx), undefined);
      assert.equal(B.unwrapParens(expr).kind !== undefined, true);
      assert.equal(B.isCollectionMutationCall(expr, ctx), false);
      assert.equal(B.isArrayChainCall(expr, ctx), false);
      assert.equal(B.tryBuildBuiltinCall(expr, ctx) === null, true);
      assert.equal(B.tryBuildL1PureSubExpression(expr, ctx).kind !== undefined, true);
      assert.equal(B.tryBuildL1Cardinality(expr, ctx) === null, true);
      assert.equal(B.elementAccessLiteralKey(keyed.expr), name);
      assert.equal(B.buildL1MemberAccess(expr, ctx).unsupported !== undefined, true);
      assert.equal(
        B.tryRecognizeFunctorLift(
          { guard: expr, thenExpr: expr, elseExpr: expr, contextNode: expr },
          ctx,
        ),
        null,
      );
      assert.equal(B.buildL1Conditional(expr, ctx).unsupported !== undefined, true);
      assert.equal(B.buildL1ConditionalFromArms([], expr, ctx).unsupported !== undefined, true);
      assert.equal(B.lowerL1ToOpaque(ir1Var(name)) !== undefined, true);
      assert.notEqual(B.buildL1IncrementStep(ir1Var(name), "add", ir1LitNat(1)), undefined);
      const muCtx = {
        ...ctx,
        checker: muChecker,
        paramNames: new Map([
          [name, name],
          ["j", "j"],
        ]),
      };
      assert.equal(B.buildL1LetWhile(mu, muCtx).kind, "block");
      assert.equal(B.buildL1MuSearchCombTyped(mu, muCtx).kind, "comb-typed");
    }),
  );
});
