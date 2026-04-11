import { describe, it } from "node:test";
import assert from "node:assert/strict";
import {
  Var, Lit, Apply, PrimedApply, Binop, Unop, Cardinality,
  Membership, Cond, Comprehension, Unsupported,
  Equation, UnsupportedProp, RawProp,
  renderExpr, renderProp,
} from "../src/pant-expr.js";

describe("renderExpr", () => {
  it("renders var", () => {
    assert.equal(renderExpr(Var("x")), "x");
  });

  it("renders literal", () => {
    assert.equal(renderExpr(Lit("42")), "42");
  });

  it("renders zero-arg apply", () => {
    assert.equal(renderExpr(Apply("getVersion")), "getVersion");
  });

  it("renders single-arg apply (property access)", () => {
    assert.equal(renderExpr(Apply("balance", Var("a"))), "balance a");
  });

  it("renders multi-arg apply", () => {
    assert.equal(renderExpr(Apply("add", Var("a"), Var("b"))), "add a b");
  });

  it("renders nested apply (nested property access)", () => {
    assert.equal(renderExpr(Apply("name", Apply("owner", Var("a")))), "name owner a");
  });

  it("wraps compound args in parens", () => {
    const expr = Apply("f", Binop("+", Var("a"), Var("b")));
    assert.equal(renderExpr(expr), "f (a + b)");
  });

  it("renders primed-apply zero args", () => {
    assert.equal(renderExpr(PrimedApply("balance")), "balance'");
  });

  it("renders primed-apply with args", () => {
    assert.equal(renderExpr(PrimedApply("balance", Var("a"))), "balance' a");
  });

  it("renders binop", () => {
    assert.equal(renderExpr(Binop("+", Var("a"), Lit("2"))), "a + 2");
  });

  it("renders nested binop with parens to preserve grouping", () => {
    assert.equal(renderExpr(Binop("and", Var("a"), Binop("or", Var("b"), Var("c")))),
      "a and (b or c)",
    );
  });

  it("renders unop ~", () => {
    assert.equal(renderExpr(Unop("~", Var("b"))), "~(b)");
  });

  it("renders unop -", () => {
    assert.equal(renderExpr(Unop("-", Var("x"))), "-(x)");
  });

  it("renders cardinality", () => {
    assert.equal(renderExpr(Cardinality(Var("items"))), "#items");
  });

  it("renders cardinality with compound expr in parens", () => {
    assert.equal(renderExpr(Cardinality(Binop("+", Var("a"), Var("b")))), "#(a + b)");
  });

  it("renders membership", () => {
    assert.equal(renderExpr(Membership(Var("x"), Var("items"))), "x in items");
  });

  it("renders cond with one arm", () => {
    assert.equal(renderExpr(Cond(
      [{ guard: Binop(">=", Var("a"), Var("b")), value: Var("a") }],
      Var("b"),
    )), "cond a >= b => a, true => b");
  });

  it("renders cond with multiple arms", () => {
    assert.equal(renderExpr(Cond(
      [
        { guard: Binop(">", Var("x"), Lit("0")), value: Lit("1") },
        { guard: Binop("<", Var("x"), Lit("0")), value: Lit("-1") },
      ],
      Lit("0"),
    )), "cond x > 0 => 1, x < 0 => -1, true => 0");
  });

  it("renders comprehension without predicate", () => {
    assert.equal(renderExpr(Comprehension("x", "User", Apply("name", Var("x")))),
      "(each x: User | name x)",
    );
  });

  it("renders comprehension with predicate", () => {
    assert.equal(renderExpr(Comprehension(
      "x", "User", Apply("name", Var("x")), Apply("active", Var("x")),
    )), "(each x: User, active x | name x)");
  });

  it("renders unsupported", () => {
    assert.equal(renderExpr(Unsupported("foo()")), "> UNSUPPORTED: foo()");
  });
});

describe("renderProp", () => {
  it("renders equation with quantifiers", () => {
    assert.equal(renderProp(Equation(
      [{ name: "a", type: "Int" }, { name: "b", type: "Int" }],
      Apply("add", Var("a"), Var("b")),
      Binop("+", Var("a"), Var("b")),
    )), "all a: Int, b: Int | add a b = a + b");
  });

  it("renders equation without quantifiers", () => {
    assert.equal(renderProp(Equation(
      [],
      Apply("getVersion"),
      Lit("42"),
    )), "getVersion = 42");
  });

  it("renders primed equation (mutation)", () => {
    assert.equal(renderProp(Equation(
      [],
      PrimedApply("balance", Var("a")),
      Binop("+", Apply("balance", Var("a")), Var("amount")),
    )), "balance' a = balance a + amount");
  });

  it("renders frame condition", () => {
    assert.equal(renderProp(Equation(
      [{ name: "a", type: "Account" }],
      PrimedApply("owner", Var("a")),
      Apply("owner", Var("a")),
    )), "all a: Account | owner' a = owner a");
  });

  it("renders unsupported prop", () => {
    assert.equal(renderProp(UnsupportedProp("conditional assignment (if/else)")),
      "> UNSUPPORTED: conditional assignment (if/else)",
    );
  });

  it("renders raw prop", () => {
    assert.equal(renderProp(RawProp("all a: Account | balance' a >= 0")),
      "all a: Account | balance' a >= 0",
    );
  });
});
