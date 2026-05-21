import { describe, it } from "node:test";

import * as fc from "fast-check";

import {
  ir1App,
  ir1Assign,
  ir1Binop,
  ir1Block,
  ir1Cond,
  ir1CondStmt,
  ir1Each,
  ir1ExprStmt,
  ir1For,
  ir1Foreach,
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
  type IR1Expr,
  type IR1Stmt,
} from "../src/ir1.js";

void fc;
void ir1App;
void ir1Assign;
void ir1Binop;
void ir1Block;
void ir1Cond;
void ir1CondStmt;
void ir1Each;
void ir1ExprStmt;
void ir1For;
void ir1Foreach;
void ir1IsNullish;
void ir1Let;
void ir1LitBool;
void ir1LitNat;
void ir1Member;
void ir1Return;
void ir1Throw;
void ir1Unop;
void ir1Var;
void ir1While;

type _IR1Expr = IR1Expr;
type _IR1Stmt = IR1Stmt;

describe("ir1-substitute", () => {
  describe("unit", () => {
    it.skip("freeVarsIR1Expr returns vars from a leaf var", () => {
      // PENDING Patch 3: assert leaf var free-var behavior.
    });

    it.skip("freeVarsIR1Expr ignores literals", () => {
      // PENDING Patch 3: assert literal expressions have no free vars.
    });

    it.skip("freeVarsIR1Expr respects each binder", () => {
      // PENDING Patch 3: assert each-binder free-var behavior.
    });

    it.skip("freeVarsIR1Stmt respects block / let / foreach / for binders", () => {
      // PENDING Patch 3: assert statement-level free vars across block, let, foreach, and for-init binders.
    });

    it.skip("substituteIR1ExprSubtree replaces a leaf Var", () => {
      // PENDING Patch 3: assert leaf Var replacement through the expression primitive.
    });

    it.skip("substituteIR1ExprSubtree replaces a Member subtree", () => {
      // PENDING Patch 3: assert Member subtree replacement through the expression primitive.
    });

    it.skip("substituteIR1ExprSubtree halts at a shadowing each binder", () => {
      // PENDING Patch 3: assert Var-needle substitution does not descend under a shadowing each binder.
    });

    it.skip("substituteIR1ExprSubtree throws CaptureRiskError on capture risk", () => {
      // PENDING Patch 3: assert replacement free vars captured by haystack binders throw CaptureRiskError.
    });

    it.skip("substituteIR1ExprSubtree preserves shape outside replacement sites", () => {
      // PENDING Patch 3: assert unrelated expression structure is preserved while matching subtrees are replaced.
    });

    it.skip("substituteIR1StmtSubtree threads block-scope let binders", () => {
      // PENDING Patch 3: assert let binders affect only subsequent block statements during statement substitution.
    });

    it.skip("substituteIR1StmtSubtree handles foreach binder scope", () => {
      // PENDING Patch 3: assert foreach binder scope covers body and fold leaves but not source.
    });

    it.skip("substituteIR1StmtSubtree handles for-init let scope", () => {
      // PENDING Patch 3: assert for-init let binders scope over cond, step, and body.
    });
  });

  describe("properties", () => {
    it.skip("free vars compose under substitution", () => {
      // PENDING Patch 3: generator skeleton uses fc.letrec for IR1Expr / IR1Stmt trees, name arbitraries from a small identifier set, and filters to no-capture cases before asserting FV(substitute(h, n, r)) = (FV(h) minus the Var needle name when applicable) union FV(r).
    });

    it.skip("substitution is idempotent when needle does not occur", () => {
      // PENDING Patch 3: generator skeleton uses fc.letrec recursive IR1Expr / IR1Stmt trees plus a needle arbitrary filtered so the needle is absent from the haystack, then asserts substitution returns the original tree.
    });

    it.skip("substitution under shadowing binder is identity", () => {
      // PENDING Patch 3: generator skeleton builds each / let / foreach / for-init binder wrappers around recursive IR1Expr / IR1Stmt haystacks with a Var needle matching the binder, then asserts descent halts at the binder boundary.
    });

    it.skip("capture-risk inputs throw", () => {
      // PENDING Patch 3: generator skeleton builds haystacks with binder names drawn from replacement free vars via fc.letrec expression / statement trees, then asserts CaptureRiskError is thrown.
    });
  });
});
