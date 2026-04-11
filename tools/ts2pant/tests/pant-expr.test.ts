import { describe, it, expect } from "vitest";
import {
  Var, Lit, Apply, PrimedApply, Binop, Unop, Cardinality,
  Membership, Cond, Comprehension, Unsupported,
  Equation, UnsupportedProp, RawProp,
  renderExpr, renderProp,
} from "../src/pant-expr.js";

describe("renderExpr", () => {
  it("renders var", () => {
    expect(renderExpr(Var("x"))).toBe("x");
  });

  it("renders literal", () => {
    expect(renderExpr(Lit("42"))).toBe("42");
  });

  it("renders zero-arg apply", () => {
    expect(renderExpr(Apply("getVersion"))).toBe("getVersion");
  });

  it("renders single-arg apply (property access)", () => {
    expect(renderExpr(Apply("balance", Var("a")))).toBe("balance a");
  });

  it("renders multi-arg apply", () => {
    expect(renderExpr(Apply("add", Var("a"), Var("b")))).toBe("add a b");
  });

  it("renders nested apply (nested property access)", () => {
    expect(renderExpr(Apply("name", Apply("owner", Var("a"))))).toBe("name owner a");
  });

  it("wraps compound args in parens", () => {
    const expr = Apply("f", Binop("+", Var("a"), Var("b")));
    expect(renderExpr(expr)).toBe("f (a + b)");
  });

  it("renders primed-apply zero args", () => {
    expect(renderExpr(PrimedApply("balance"))).toBe("balance'");
  });

  it("renders primed-apply with args", () => {
    expect(renderExpr(PrimedApply("balance", Var("a")))).toBe("balance' a");
  });

  it("renders binop", () => {
    expect(renderExpr(Binop("+", Var("a"), Lit("2")))).toBe("a + 2");
  });

  it("renders nested binop with parens to preserve grouping", () => {
    expect(renderExpr(Binop("and", Var("a"), Binop("or", Var("b"), Var("c"))))).toBe(
      "a and (b or c)",
    );
  });

  it("renders unop ~", () => {
    expect(renderExpr(Unop("~", Var("b")))).toBe("~(b)");
  });

  it("renders unop -", () => {
    expect(renderExpr(Unop("-", Var("x")))).toBe("-(x)");
  });

  it("renders cardinality", () => {
    expect(renderExpr(Cardinality(Var("items")))).toBe("#items");
  });

  it("renders cardinality with compound expr in parens", () => {
    expect(renderExpr(Cardinality(Binop("+", Var("a"), Var("b"))))).toBe("#(a + b)");
  });

  it("renders membership", () => {
    expect(renderExpr(Membership(Var("x"), Var("items")))).toBe("x in items");
  });

  it("renders cond with one arm", () => {
    expect(renderExpr(Cond(
      [{ guard: Binop(">=", Var("a"), Var("b")), value: Var("a") }],
      Var("b"),
    ))).toBe("cond a >= b => a, true => b");
  });

  it("renders cond with multiple arms", () => {
    expect(renderExpr(Cond(
      [
        { guard: Binop(">", Var("x"), Lit("0")), value: Lit("1") },
        { guard: Binop("<", Var("x"), Lit("0")), value: Lit("-1") },
      ],
      Lit("0"),
    ))).toBe("cond x > 0 => 1, x < 0 => -1, true => 0");
  });

  it("renders comprehension without predicate", () => {
    expect(renderExpr(Comprehension("x", "User", Apply("name", Var("x"))))).toBe(
      "(each x: User | name x)",
    );
  });

  it("renders comprehension with predicate", () => {
    expect(renderExpr(Comprehension(
      "x", "User", Apply("name", Var("x")), Apply("active", Var("x")),
    ))).toBe("(each x: User, active x | name x)");
  });

  it("renders unsupported", () => {
    expect(renderExpr(Unsupported("foo()"))).toBe("> UNSUPPORTED: foo()");
  });
});

describe("renderProp", () => {
  it("renders equation with quantifiers", () => {
    expect(renderProp(Equation(
      [{ name: "a", type: "Int" }, { name: "b", type: "Int" }],
      Apply("add", Var("a"), Var("b")),
      Binop("+", Var("a"), Var("b")),
    ))).toBe("all a: Int, b: Int | add a b = a + b");
  });

  it("renders equation without quantifiers", () => {
    expect(renderProp(Equation(
      [],
      Apply("getVersion"),
      Lit("42"),
    ))).toBe("getVersion = 42");
  });

  it("renders primed equation (mutation)", () => {
    expect(renderProp(Equation(
      [],
      PrimedApply("balance", Var("a")),
      Binop("+", Apply("balance", Var("a")), Var("amount")),
    ))).toBe("balance' a = balance a + amount");
  });

  it("renders frame condition", () => {
    expect(renderProp(Equation(
      [{ name: "a", type: "Account" }],
      PrimedApply("owner", Var("a")),
      Apply("owner", Var("a")),
    ))).toBe("all a: Account | owner' a = owner a");
  });

  it("renders unsupported prop", () => {
    expect(renderProp(UnsupportedProp("conditional assignment (if/else)"))).toBe(
      "> UNSUPPORTED: conditional assignment (if/else)",
    );
  });

  it("renders raw prop", () => {
    expect(renderProp(RawProp("all a: Account | balance' a >= 0"))).toBe(
      "all a: Account | balance' a >= 0",
    );
  });
});
