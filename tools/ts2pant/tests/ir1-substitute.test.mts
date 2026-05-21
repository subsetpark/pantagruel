import assert from "node:assert/strict";
import { describe, it } from "node:test";

import * as fc from "fast-check";

import {
  type IR1Expr,
  type IR1Stmt,
  ir1App,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1CombTyped,
  ir1Cond,
  ir1Each,
  ir1Exists,
  ir1ExprStmt,
  ir1For,
  ir1Foreach,
  ir1Forall,
  ir1IsNullish,
  ir1Let,
  ir1LitBool,
  ir1LitNat,
  ir1Member,
  ir1Return,
  ir1Throw,
  ir1Unop,
  ir1Var,
  ir1While,
} from "../src/ir1.js";
import {
  CaptureRiskError,
  freeVarsIR1Expr,
  freeVarsIR1Stmt,
  substituteIR1ExprSubtree,
  substituteIR1StmtSubtree,
} from "../src/ir1-substitute.js";

type _IR1Expr = IR1Expr;
type _IR1Stmt = IR1Stmt;

describe("ir1-substitute", () => {
  describe("unit", () => {
    it("freeVarsIR1Expr returns vars from a leaf var", () => {
      assertSetEqual(freeVarsIR1Expr(ir1Var("x")), ["x"]);
    });

    it("freeVarsIR1Expr ignores literals", () => {
      assertSetEqual(freeVarsIR1Expr(ir1LitNat(1)), []);
    });

    it("freeVarsIR1Expr respects each binder", () => {
      const expr = ir1Each(
        "x",
        ir1Var("src"),
        [ir1Var("x"), ir1Var("g")],
        ir1Var("x"),
      );
      assertSetEqual(freeVarsIR1Expr(expr), ["g", "src"]);
    });

    it("freeVarsIR1Expr scopes comb-typed binder", () => {
      const expr = ir1CombTyped(
        "min",
        "x",
        "Int",
        [ir1Var("x"), ir1Var("g")],
        ir1Binop("add", ir1Var("x"), ir1Var("y")),
      );
      assertSetEqual(freeVarsIR1Expr(expr), ["g", "y"]);
    });

    it("freeVarsIR1Expr scopes forall binder", () => {
      const expr = ir1Forall(
        "x",
        "Int",
        ir1Binop("gt", ir1Var("x"), ir1Var("z")),
        ir1Binop("ge", ir1Var("x"), ir1Var("lower")),
      );
      assertSetEqual(freeVarsIR1Expr(expr), ["lower", "z"]);
    });

    it("freeVarsIR1Expr scopes exists binder", () => {
      const expr = ir1Exists("x", "Int", ir1Binop("eq", ir1Var("x"), ir1Var("z")));
      assertSetEqual(freeVarsIR1Expr(expr), ["z"]);
    });

    it("freeVarsIR1Stmt respects block / let / foreach / for binders", () => {
      const stmt = ir1Block([
        ir1Let("x", ir1Var("seed")),
        ir1Foreach(
          "y",
          ir1Var("xs"),
          ir1Assign(ir1Member(ir1Var("y"), "p"), ir1Var("x")),
          [
            {
              target: ir1Var("acc"),
              prop: "total",
              combiner: "add",
              outerOp: "add",
              rhs: ir1Var("y"),
              guard: ir1Var("x"),
            },
          ],
        ),
        ir1For(
          ir1Let("i", ir1Var("start")),
          ir1Var("i"),
          ir1Assign(ir1Var("i"), ir1Var("x")),
          ir1Return(ir1Var("i")),
        ),
      ]);
      assertSetEqual(freeVarsIR1Stmt(stmt), ["acc", "seed", "start", "xs"]);
    });

    it("substituteIR1ExprSubtree replaces a leaf Var", () => {
      assert.deepEqual(
        substituteIR1ExprSubtree(ir1Var("x"), ir1Var("x"), ir1LitNat(1)),
        ir1LitNat(1),
      );
    });

    it("substituteIR1ExprSubtree replaces a Member subtree", () => {
      const haystack = ir1Member(ir1Member(ir1Var("u"), "profile"), "name");
      const needle = ir1Member(ir1Var("u"), "profile");
      assert.deepEqual(
        substituteIR1ExprSubtree(haystack, needle, ir1Var("p")),
        ir1Member(ir1Var("p"), "name"),
      );
    });

    it("substituteIR1ExprSubtree halts at a shadowing each binder", () => {
      const expr = ir1Each(
        "x",
        ir1Var("xs"),
        [ir1Var("x")],
        ir1Member(ir1Var("x"), "p"),
      );
      assert.deepEqual(
        substituteIR1ExprSubtree(expr, ir1Var("x"), ir1LitNat(0)),
        ir1Each("x", ir1Var("xs"), [ir1Var("x")], ir1Member(ir1Var("x"), "p")),
      );
    });

    it("substituteIR1ExprSubtree throws CaptureRiskError on capture risk", () => {
      assert.throws(
        () =>
          substituteIR1ExprSubtree(
            ir1Each("x", ir1Var("xs"), [], ir1Var("y")),
            ir1Var("y"),
            ir1Var("x"),
          ),
        CaptureRiskError,
      );
    });

    it("substituteIR1ExprSubtree throws under comb-typed capture", () => {
      assert.throws(
        () =>
          substituteIR1ExprSubtree(
            ir1CombTyped("min", "x", "Int", [], ir1Var("target")),
            ir1Var("target"),
            ir1Var("x"),
          ),
        CaptureRiskError,
      );
    });

    it("substituteIR1ExprSubtree throws under forall capture", () => {
      assert.throws(
        () =>
          substituteIR1ExprSubtree(
            ir1Forall("x", "Int", ir1Var("target")),
            ir1Var("target"),
            ir1Var("x"),
          ),
        CaptureRiskError,
      );
    });

    it("substituteIR1ExprSubtree throws under exists capture", () => {
      assert.throws(
        () =>
          substituteIR1ExprSubtree(
            ir1Exists("x", "Int", ir1Var("target")),
            ir1Var("target"),
            ir1Var("x"),
          ),
        CaptureRiskError,
      );
    });

    it("substituteIR1ExprSubtree halts at shadowing comb-typed", () => {
      const expr = ir1CombTyped(
        "min",
        "x",
        "Int",
        [ir1Var("x")],
        ir1Member(ir1Var("x"), "p"),
      );
      assert.deepEqual(
        substituteIR1ExprSubtree(expr, ir1Var("x"), ir1LitNat(0)),
        expr,
      );
    });

    it("substituteIR1ExprSubtree halts at shadowing forall", () => {
      const expr = ir1Forall(
        "x",
        "Int",
        ir1Member(ir1Var("x"), "p"),
        ir1Var("x"),
      );
      assert.deepEqual(
        substituteIR1ExprSubtree(expr, ir1Var("x"), ir1LitNat(0)),
        expr,
      );
    });

    it("substituteIR1ExprSubtree halts at shadowing exists", () => {
      const expr = ir1Exists(
        "x",
        "Int",
        ir1Member(ir1Var("x"), "p"),
        ir1Var("x"),
      );
      assert.deepEqual(
        substituteIR1ExprSubtree(expr, ir1Var("x"), ir1LitNat(0)),
        expr,
      );
    });

    it("arbIR1Expr includes new L1 binder forms", () => {
      const seen = new Set<string>();
      for (let seed = 1; seed <= 20; seed += 1) {
        for (const expr of fc.sample(arbIR1Expr(3), { seed, numRuns: 200 })) {
          seen.add(expr.kind);
        }
      }
      assert.equal(seen.has("comb-typed"), true);
      assert.equal(seen.has("forall"), true);
      assert.equal(seen.has("exists"), true);
    });

    it("substituteIR1ExprSubtree preserves shape outside replacement sites", () => {
      const expr = ir1Cond(
        [[ir1Var("g"), ir1Binop("add", ir1Var("x"), ir1LitNat(1))]],
        ir1Unop("not", ir1Var("done")),
      );
      assert.deepEqual(
        substituteIR1ExprSubtree(expr, ir1Var("x"), ir1Var("z")),
        ir1Cond(
          [[ir1Var("g"), ir1Binop("add", ir1Var("z"), ir1LitNat(1))]],
          ir1Unop("not", ir1Var("done")),
        ),
      );
    });

    it("substituteIR1ExprSubtree preserves root identity when unchanged", () => {
      const expr = ir1Member(ir1Member(ir1Var("u"), "profile"), "name");
      assert.equal(
        substituteIR1ExprSubtree(
          expr,
          ir1Member(ir1Var("v"), "profile"),
          ir1Var("p"),
        ),
        expr,
      );
    });

    it("substituteIR1StmtSubtree threads block-scope let binders", () => {
      const stmt = ir1Block([ir1Let("x", ir1Var("x")), ir1Return(ir1Var("x"))]);
      assert.deepEqual(
        substituteIR1StmtSubtree(stmt, ir1Var("x"), ir1LitNat(1)),
        ir1Block([ir1Let("x", ir1LitNat(1)), ir1Return(ir1Var("x"))]),
      );
    });

    it("substituteIR1StmtSubtree handles foreach binder scope", () => {
      const stmt = ir1Foreach(
        "x",
        ir1Var("x"),
        ir1Assign(ir1Member(ir1Var("x"), "p"), ir1Var("x")),
        [
          {
            target: ir1Var("x"),
            prop: "q",
            combiner: "add",
            outerOp: "add",
            rhs: ir1Var("x"),
            guard: null,
          },
        ],
      );
      assert.deepEqual(
        substituteIR1StmtSubtree(stmt, ir1Var("x"), ir1LitNat(7)),
        ir1Foreach(
          "x",
          ir1LitNat(7),
          ir1Assign(ir1Member(ir1Var("x"), "p"), ir1Var("x")),
          [
            {
              target: ir1Var("x"),
              prop: "q",
              combiner: "add",
              outerOp: "add",
              rhs: ir1Var("x"),
              guard: null,
            },
          ],
        ),
      );
    });

    it("substituteIR1StmtSubtree handles for-init let scope", () => {
      const stmt = ir1For(
        ir1Let("i", ir1Var("i")),
        ir1Var("i"),
        ir1Assign(ir1Var("i"), ir1Var("i")),
        ir1Return(ir1Var("i")),
      );
      assert.deepEqual(
        substituteIR1StmtSubtree(stmt, ir1Var("i"), ir1LitNat(0)),
        ir1For(
          ir1Let("i", ir1LitNat(0)),
          ir1Var("i"),
          ir1Assign(ir1Var("i"), ir1Var("i")),
          ir1Return(ir1Var("i")),
        ),
      );
    });
  });

  describe("properties", () => {
    it("free vars compose under substitution", () => {
      fc.assert(
        fc.property(arbIR1Expr(), (expr) => {
          const rewritten = substituteIR1ExprSubtree(
            expr,
            ir1Var("a"),
            ir1LitNat(0),
          );
          const expected = freeVarsIR1Expr(expr);
          expected.delete("a");
          assertSetEqual(freeVarsIR1Expr(rewritten), [...expected]);
        }),
      );
    });

    it("substitution is idempotent when needle does not occur", () => {
      fc.assert(
        fc.property(arbIR1Expr(), arbIR1Stmt(), (expr, stmt) => {
          const needle = ir1Var("z");
          assert(!exprContainsNeedle(expr, needle));
          assert.deepEqual(
            substituteIR1ExprSubtree(expr, needle, ir1LitNat(0)),
            expr,
          );
          assert.deepEqual(
            substituteIR1StmtSubtree(stmt, needle, ir1LitNat(0)),
            stmt,
          );
        }),
      );
    });

    it("substitution under shadowing binder is identity", () => {
      fc.assert(
        fc.property(arbIR1Expr(), (body) => {
          const expr = ir1Each("a", ir1Var("src"), [body], body);
          assert.deepEqual(
            substituteIR1ExprSubtree(expr, ir1Var("a"), ir1LitNat(0)),
            expr,
          );
        }),
      );
    });

    it("capture-risk inputs throw", () => {
      fc.assert(
        fc.property(nameArb, (name) => {
          assert.throws(
            () =>
              substituteIR1ExprSubtree(
                ir1Each(name, ir1Var("src"), [], ir1Var("target")),
                ir1Var("target"),
                ir1Var(name),
              ),
            CaptureRiskError,
          );
          assert.throws(
            () =>
              substituteIR1StmtSubtree(
                ir1Foreach(name, ir1Var("src"), ir1Return(ir1Var("target"))),
                ir1Var("target"),
                ir1Var(name),
              ),
            CaptureRiskError,
          );
        }),
      );
    });
  });
});

const names = ["a", "b", "c", "d", "e", "f"] as const;
const nameArb = fc.constantFrom(...names);

function arbIR1Expr(depth = 4): fc.Arbitrary<IR1Expr> {
  return fc.letrec<{ expr: IR1Expr }>(() => ({
    expr:
      depth <= 0
        ? arbLeafExpr()
        : fc.oneof(
            arbLeafExpr(),
            fc
              .tuple(arbIR1Expr(depth - 1), fc.constantFrom("p", "q"))
              .map(([receiver, name]) => ir1Member(receiver, name)),
            fc
              .tuple(arbIR1Expr(depth - 1), arbIR1Expr(depth - 1))
              .map(([lhs, rhs]) => ir1Binop("add", lhs, rhs)),
            arbIR1Expr(depth - 1).map((arg) => ir1Unop("not", arg)),
            fc
              .tuple(
                arbIR1Expr(depth - 1),
                fc.array(arbIR1Expr(depth - 1), { maxLength: 2 }),
              )
              .map(([callee, args]) => ir1App(callee, args)),
            arbIR1Expr(depth - 1).map((operand) => ir1IsNullish(operand)),
            fc
              .tuple(
                arbIR1Expr(depth - 1),
                arbIR1Expr(depth - 1),
                arbIR1Expr(depth - 1),
              )
              .map(([guard, value, otherwise]) =>
                ir1Cond([[guard, value]], otherwise),
              ),
            fc
              .tuple(
                nameArb,
                arbIR1Expr(depth - 1),
                fc.array(arbIR1Expr(depth - 1), { maxLength: 2 }),
                arbIR1Expr(depth - 1),
              )
              .map(([binder, src, guards, proj]) =>
                ir1Each(binder, src, guards, proj),
              ),
            fc
              .tuple(
                fc.constantFrom("min", "max" as const),
                nameArb,
                fc.constantFrom("Nat0", "Int", "Real"),
                fc.array(arbIR1Expr(depth - 1), { maxLength: 2 }),
                arbIR1Expr(depth - 1),
              )
              .map(([combiner, binder, binderType, guards, proj]) =>
                ir1CombTyped(combiner, binder, binderType, guards, proj),
              ),
            fc
              .tuple(
                nameArb,
                fc.constantFrom("Nat0", "Int", "Real"),
                arbIR1Expr(depth - 1),
                fc.option(arbIR1Expr(depth - 1), { nil: undefined }),
              )
              .map(([binder, binderType, body, guard]) =>
                ir1Forall(binder, binderType, body, guard),
              ),
            fc
              .tuple(
                nameArb,
                fc.constantFrom("Nat0", "Int", "Real"),
                arbIR1Expr(depth - 1),
                fc.option(arbIR1Expr(depth - 1), { nil: undefined }),
              )
              .map(([binder, binderType, body, guard]) =>
                ir1Exists(binder, binderType, body, guard),
              ),
          ),
  })).expr;
}

function arbIR1Stmt(depth = 4): fc.Arbitrary<IR1Stmt> {
  return fc.letrec<{ stmt: IR1Stmt }>(() => ({
    stmt:
      depth <= 0
        ? arbLeafStmt()
        : fc.oneof(
            arbLeafStmt(),
            fc
              .tuple(nameArb, arbIR1Expr(depth - 1))
              .map(([name, value]) => ir1Let(name, value)),
            fc
              .tuple(arbIR1Expr(depth - 1), arbIR1Expr(depth - 1))
              .map(([target, value]) => ir1Assign(target, value)),
            fc
              .array(arbIR1Stmt(depth - 1), { minLength: 2, maxLength: 3 })
              .map((stmts) => ir1Block(stmts as [IR1Stmt, ...IR1Stmt[]])),
            fc
              .tuple(nameArb, arbIR1Expr(depth - 1), arbIR1Stmt(depth - 1))
              .map(([binder, source, body]) =>
                ir1Foreach(
                  binder,
                  source,
                  body as Extract<IR1Stmt, { kind: "assign" }>,
                  [],
                ),
              ),
            fc
              .tuple(
                nameArb,
                arbIR1Expr(depth - 1),
                arbIR1Expr(depth - 1),
                arbIR1Stmt(depth - 1),
              )
              .map(([binder, initValue, cond, body]) =>
                ir1For(ir1Let(binder, initValue), cond, null, body),
              ),
            fc
              .tuple(arbIR1Expr(depth - 1), arbIR1Stmt(depth - 1))
              .map(([cond, body]) => ir1While(cond, body)),
          ),
  })).stmt;
}

function arbLeafExpr(): fc.Arbitrary<IR1Expr> {
  return fc.oneof(
    nameArb.map((name) => ir1Var(name)),
    fc.boolean().map(ir1LitBool),
    fc.nat(10).map(ir1LitNat),
  );
}

function arbLeafStmt(): fc.Arbitrary<IR1Stmt> {
  return fc.oneof(
    arbIR1Expr(0).map(ir1Return),
    arbIR1Expr(0).map(ir1Throw),
    arbIR1Expr(0).map(ir1ExprStmt),
  );
}

function assertSetEqual(actual: Set<string>, expected: Iterable<string>): void {
  assert.deepEqual([...actual].sort(), [...expected].sort());
}

function exprContainsNeedle(
  expr: IR1Expr,
  needle: Extract<IR1Expr, { kind: "var" | "member" }>,
): boolean {
  if (exprNeedleEqual(expr, needle)) {
    return true;
  }
  switch (expr.kind) {
    case "var":
    case "lit":
      return false;
    case "binop":
      return (
        exprContainsNeedle(expr.lhs, needle) ||
        exprContainsNeedle(expr.rhs, needle)
      );
    case "unop":
      return exprContainsNeedle(expr.arg, needle);
    case "app":
      return (
        exprContainsNeedle(expr.callee, needle) ||
        expr.args.some((arg) => exprContainsNeedle(arg, needle))
      );
    case "member":
      return exprContainsNeedle(expr.receiver, needle);
    case "cond":
      return (
        expr.arms.some(
          ([guard, value]) =>
            exprContainsNeedle(guard, needle) ||
            exprContainsNeedle(value, needle),
        ) || exprContainsNeedle(expr.otherwise, needle)
      );
    case "is-nullish":
      return exprContainsNeedle(expr.operand, needle);
    case "each":
      return (
        exprContainsNeedle(expr.src, needle) ||
        expr.guards.some((guard) => exprContainsNeedle(guard, needle)) ||
        exprContainsNeedle(expr.proj, needle)
      );
    case "comb-typed":
      return (
        expr.guards.some((guard) => exprContainsNeedle(guard, needle)) ||
        exprContainsNeedle(expr.proj, needle)
      );
    case "forall":
    case "exists":
      return (
        (expr.guard !== undefined && exprContainsNeedle(expr.guard, needle)) ||
        exprContainsNeedle(expr.body, needle)
      );
    case "map-read":
      return (
        exprContainsNeedle(expr.receiver, needle) ||
        exprContainsNeedle(expr.key, needle)
      );
    case "set-read":
      return (
        exprContainsNeedle(expr.receiver, needle) ||
        exprContainsNeedle(expr.elem, needle)
      );
    default: {
      const _exhaustive: never = expr;
      return _exhaustive;
    }
  }
}

function exprNeedleEqual(
  expr: IR1Expr,
  needle: Extract<IR1Expr, { kind: "var" | "member" }>,
): boolean {
  if (expr.kind !== needle.kind) {
    return false;
  }
  if (expr.kind === "var" && needle.kind === "var") {
    return (
      expr.name === needle.name &&
      (expr.primed ?? false) === (needle.primed ?? false)
    );
  }
  if (expr.kind === "member" && needle.kind === "member") {
    return (
      expr.name === needle.name &&
      exprNeedleEqual(
        expr.receiver,
        needle.receiver as Extract<IR1Expr, { kind: "var" | "member" }>,
      )
    );
  }
  return false;
}
