import assert from "node:assert/strict";
import { before, describe, it } from "node:test";

import {
  type IR1Expr,
  type IR1Stmt,
  ir1Assign,
  ir1Binop,
  ir1For,
  ir1Let,
  ir1LitNat,
  ir1Member,
  ir1Var,
} from "../src/ir1.js";
import { lowerCounterLoopL1Body } from "../src/ir1-ssa-counter-loop.js";
import { getAst, loadAst } from "../src/pant-wasm.js";

const account = ir1Var("account");
const bound = ir1Var("n");
const total = ir1Member(account, "Account_total");

before(async () => {
  await loadAst();
});

function counterFor(body: IR1Stmt, init: IR1Expr = ir1LitNat(0)): IR1Stmt {
  return ir1For(
    ir1Let("i", init),
    ir1Binop("lt", ir1Var("i"), bound),
    ir1Assign(ir1Var("i"), ir1Binop("add", ir1Var("i"), ir1LitNat(1))),
    body,
  );
}

function descCounterFor(
  body: IR1Stmt,
  init: IR1Expr = ir1Var("start"),
  boundExpr: IR1Expr = ir1LitNat(0),
  cmp: "gt" | "ge" = "gt",
): IR1Stmt {
  return ir1For(
    ir1Let("i", init),
    ir1Binop(cmp, ir1Var("i"), boundExpr),
    ir1Assign(ir1Var("i"), ir1Binop("sub", ir1Var("i"), ir1LitNat(1))),
    body,
  );
}

function lower(stmt: IR1Stmt) {
  assert.equal(stmt.kind, "for");
  return lowerCounterLoopL1Body(stmt);
}

function diagnosticReasons(stmt: IR1Stmt): string[] {
  return lower(stmt).diagnostics.map((d) => d.reason);
}

describe("ir1-ssa-counter-loop", () => {
  it("builds a loop-header join per mutated location", () => {
    const result = lower(
      counterFor(ir1Assign(total, ir1Binop("add", total, ir1Var("i")))),
    );

    assert.deepEqual(result.diagnostics, []);
    const program = result.programs[0];
    assert.equal(program?.loopHeaderJoins.length, 1);
    assert.equal(program?.loopBodies.length, 1);
    assert.equal(program?.loopBodies[0]?.headerJoins[0]?.closed, true);
    assert.equal(
      program?.loopBodies[0]?.headerJoins[0],
      program.loopHeaderJoins[0],
    );
  });

  it("attaches a termination metric to the loop body", () => {
    const result = lower(
      counterFor(ir1Assign(total, ir1Binop("add", total, ir1Var("i")))),
    );

    const metric = result.programs[0]?.loopBodies[0]?.terminationMetric;
    assert.equal(metric?.kind, "ssa-termination-metric");
    assert.deepEqual(metric?.lowerBound, ir1LitNat(0));
    assert.equal(metric?.expr.kind, "binop");
    if (metric?.expr.kind === "binop") {
      assert.equal(metric.expr.op, "sub");
      assert.deepEqual(metric.expr.lhs, bound);
      assert.deepEqual(metric.expr.rhs, ir1Var("i"));
    }
  });

  it("emits an over-each equation for an accumulator-fold body", () => {
    const ast = getAst();
    const result = lower(
      counterFor(ir1Assign(total, ir1Binop("add", total, ir1Var("i")))),
    );

    assert.deepEqual(result.diagnostics, []);
    const prop = result.propositions[0];
    assert.equal(prop?.kind, "equation");
    if (prop?.kind === "equation") {
      assert.equal(ast.strExpr(prop.lhs), "Account_total' account");
      assert.equal(
        ast.strExpr(prop.rhs),
        "Account_total account + (+ over each i: Nat0, i >= 0, i < n | i)",
      );
    }
  });

  it("emits an init lower-bound guard for non-zero accumulator folds", () => {
    const ast = getAst();
    const result = lower(
      counterFor(
        ir1Assign(total, ir1Binop("add", total, ir1Var("i"))),
        ir1LitNat(3),
      ),
    );

    assert.deepEqual(result.diagnostics, []);
    const prop = result.propositions[0];
    assert.equal(prop?.kind, "equation");
    if (prop?.kind === "equation") {
      assert.equal(
        ast.strExpr(prop.rhs),
        "Account_total account + (+ over each i: Nat0, i >= 3, i < n | i)",
      );
    }
  });

  it("emits a cond equation for a simple counter-only assign body", () => {
    const ast = getAst();
    const result = lower(counterFor(ir1Assign(total, ir1Var("i"))));

    assert.deepEqual(result.diagnostics, []);
    const prop = result.propositions[0];
    assert.equal(prop?.kind, "equation");
    if (prop?.kind === "equation") {
      assert.equal(ast.strExpr(prop.lhs), "Account_total' account");
      assert.equal(
        ast.strExpr(prop.rhs),
        "cond 0 < n => n - 1, true => Account_total account",
      );
    }
  });

  it("uses init-vs-bound to detect simple-assign zero-iteration loops", () => {
    const ast = getAst();
    const result = lower(
      counterFor(ir1Assign(total, ir1Var("i")), ir1LitNat(5)),
    );

    assert.deepEqual(result.diagnostics, []);
    const prop = result.propositions[0];
    assert.equal(prop?.kind, "equation");
    if (prop?.kind === "equation") {
      assert.equal(
        ast.strExpr(prop.rhs),
        "cond 5 < n => n - 1, true => Account_total account",
      );
    }
  });

  it("rejects non-literal init", () => {
    assert.match(
      diagnosticReasons(
        counterFor(
          ir1Assign(total, ir1Binop("add", total, ir1Var("i"))),
          ir1Var("start"),
        ),
      ).join("\n"),
      /initializer must be a numeric literal/u,
    );
  });

  it("rejects non-canonical step", () => {
    const stmt = ir1For(
      ir1Let("i", ir1LitNat(0)),
      ir1Binop("lt", ir1Var("i"), bound),
      ir1Assign(ir1Var("i"), ir1Binop("add", ir1Var("i"), ir1LitNat(2))),
      ir1Assign(total, ir1Binop("add", total, ir1Var("i"))),
    );

    assert.match(
      diagnosticReasons(stmt).join("\n"),
      /step must be a canonical \+1 increment or -1 decrement/u,
    );
  });

  it("rejects body with accumulator self-recurrence", () => {
    assert.match(
      diagnosticReasons(
        counterFor(ir1Assign(total, ir1Binop("mul", total, total))),
      ).join("\n"),
      /accumulator self-recurrence; lift to L4 fixed-point lowering/u,
    );
  });

  it("rejects body with effectful step expression", () => {
    const stmt = ir1For(
      ir1Let("i", ir1LitNat(0)),
      ir1Binop("lt", ir1Var("i"), bound),
      ir1Assign(
        ir1Var("other"),
        ir1Binop("add", ir1Var("other"), ir1LitNat(1)),
      ),
      ir1Assign(total, ir1Binop("add", total, ir1Var("i"))),
    );

    assert.match(
      diagnosticReasons(stmt).join("\n"),
      /step must be a canonical \+1 increment or -1 decrement/u,
    );
  });

  it("recognizes descending counter loop shape (`>` + `-1` step)", () => {
    const result = lower(
      descCounterFor(ir1Assign(total, ir1Binop("add", total, ir1Var("i")))),
    );

    assert.deepEqual(result.diagnostics, []);
    assert.equal(result.programs[0]?.loopBodies.length, 1);
  });

  it("attaches counter - bound termination metric for desc loops", () => {
    const result = lower(
      descCounterFor(
        ir1Assign(total, ir1Binop("add", total, ir1Var("i"))),
        ir1Var("start"),
        ir1LitNat(2),
      ),
    );

    const metric = result.programs[0]?.loopBodies[0]?.terminationMetric;
    assert.equal(metric?.kind, "ssa-termination-metric");
    assert.deepEqual(metric?.lowerBound, ir1LitNat(0));
    assert.equal(metric?.expr.kind, "binop");
    if (metric?.expr.kind === "binop") {
      assert.equal(metric.expr.op, "sub");
      assert.deepEqual(metric.expr.lhs, ir1Var("i"));
      assert.deepEqual(metric.expr.rhs, ir1LitNat(2));
    }
  });

  it("emits desc over-each accumulator fold equation", () => {
    const ast = getAst();
    const result = lower(
      descCounterFor(
        ir1Assign(total, ir1Binop("add", total, ir1Var("i"))),
        ir1Var("start"),
        ir1LitNat(0),
      ),
    );

    assert.deepEqual(result.diagnostics, []);
    const prop = result.propositions[0];
    assert.equal(prop?.kind, "equation");
    if (prop?.kind === "equation") {
      assert.equal(
        ast.strExpr(prop.rhs),
        "Account_total account + (+ over each i: Nat0, i <= start, i > 0 | i)",
      );
    }
  });

  it("emits desc cond simple-assign equation", () => {
    const ast = getAst();
    const result = lower(
      descCounterFor(ir1Assign(total, ir1Var("i")), ir1Var("start")),
    );

    assert.deepEqual(result.diagnostics, []);
    const prop = result.propositions[0];
    assert.equal(prop?.kind, "equation");
    if (prop?.kind === "equation") {
      assert.equal(
        ast.strExpr(prop.rhs),
        "cond start > 0 => 0 + 1, true => Account_total account",
      );
    }
  });

  it("rejects asc cmp with -1 step as mixed-direction", () => {
    const stmt = ir1For(
      ir1Let("i", ir1LitNat(0)),
      ir1Binop("lt", ir1Var("i"), bound),
      ir1Assign(ir1Var("i"), ir1Binop("sub", ir1Var("i"), ir1LitNat(1))),
      ir1Assign(total, ir1Binop("add", total, ir1Var("i"))),
    );

    assert.match(
      diagnosticReasons(stmt).join("\n"),
      /counter loop direction is inconsistent/u,
    );
  });

  it("rejects desc shape with non-literal bound", () => {
    assert.match(
      diagnosticReasons(
        descCounterFor(
          ir1Assign(total, ir1Binop("add", total, ir1Var("i"))),
          ir1Var("start"),
          bound,
        ),
      ).join("\n"),
      /descending counter loop bound must be a numeric literal/u,
    );
  });

  it("accepts non-literal init for desc shape", () => {
    const ast = getAst();
    const result = lower(
      descCounterFor(
        ir1Assign(total, ir1Binop("add", total, ir1Var("i"))),
        ir1Var("start"),
        ir1LitNat(1),
        "ge",
      ),
    );

    assert.deepEqual(result.diagnostics, []);
    const prop = result.propositions[0];
    assert.equal(prop?.kind, "equation");
    if (prop?.kind === "equation") {
      assert.equal(
        ast.strExpr(prop.rhs),
        "Account_total account + (+ over each i: Nat0, i <= start, i >= 1 | i)",
      );
    }
  });
});
