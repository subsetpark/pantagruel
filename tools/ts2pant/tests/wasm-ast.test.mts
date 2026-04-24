import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { loadAst } from "../src/pant-wasm.js";

describe("wasm AST constructors", async () => {
  const ast = await loadAst();

  it("var", () => {
    assert.equal(ast.strExpr(ast.var("x")), "x");
  });

  it("app", () => {
    assert.equal(
      ast.strExpr(ast.app(ast.var("f"), [ast.var("a"), ast.var("b")])),
      "f a b",
    );
  });

  it("primed app", () => {
    assert.equal(
      ast.strExpr(ast.app(ast.primed("balance"), [ast.var("a")])),
      "balance' a",
    );
  });

  it("binops", () => {
    const a = ast.var("a");
    const b = ast.var("b");
    assert.equal(ast.strExpr(ast.binop(ast.opAnd(), a, b)), "a and b");
    assert.equal(ast.strExpr(ast.binop(ast.opGe(), a, ast.litNat(0))), "a >= 0");
    assert.equal(ast.strExpr(ast.binop(ast.opAdd(), a, b)), "a + b");
    assert.equal(ast.strExpr(ast.binop(ast.opIn(), a, ast.domain("User"))), "a in User");
  });

  it("unops", () => {
    assert.equal(ast.strExpr(ast.unop(ast.opCard(), ast.var("xs"))), "#xs");
  });

  it("cond", () => {
    const a = ast.var("a");
    const b = ast.var("b");
    const c = ast.cond([
      [ast.binop(ast.opGe(), a, b), a],
      [ast.litBool(true), b],
    ]);
    assert.equal(ast.strExpr(c), "cond a >= b => a, true => b");
  });

  it("forall", () => {
    const p = ast.param("x", ast.tName("Int"));
    const e = ast.forall([p], [], ast.binop(ast.opGt(), ast.var("x"), ast.litNat(0)));
    assert.equal(ast.strExpr(e), "all x: Int | x > 0");
  });

  it("each with guard", () => {
    const e = ast.each(
      [ast.param("u", ast.tName("User"))],
      [ast.gExpr(ast.app(ast.var("active"), [ast.var("u")]))],
      ast.app(ast.var("name"), [ast.var("u")]),
    );
    assert.equal(ast.strExpr(e), "each u: User, active u | name u");
  });

  it("type expressions", () => {
    assert.equal(ast.strTypeExpr(ast.tName("Int")), "Int");
    assert.equal(ast.strTypeExpr(ast.tList(ast.tName("User"))), "[User]");
    assert.equal(
      ast.strTypeExpr(ast.tSum([ast.tName("Value"), ast.tName("Int")])),
      "Value + Int",
    );
  });

  it("declarations", () => {
    assert.equal(ast.strDecl(ast.declDomain("User")), "User.");
    assert.equal(
      ast.strDecl(
        ast.declRule("balance", [ast.param("a", ast.tName("Account"))], [], ast.tName("Int")),
      ),
      "balance a: Account => Int.",
    );
  });

  it("substituteBinder", () => {
    const expr = ast.app(ast.var("f"), [ast.var("x")]);
    const subst = ast.substituteBinder(expr, "x", ast.app(ast.var("g"), [ast.var("y")]));
    assert.equal(ast.strExpr(subst), "f (g y)");
  });

  it("composed expression round-trip", () => {
    const app = ast.app(ast.var("f"), [ast.var("a"), ast.var("b")]);
    const lhs = ast.binop(ast.opGe(), app, ast.var("a"));
    const rhs = ast.binop(ast.opGe(), app, ast.var("b"));
    const conj = ast.binop(ast.opAnd(), lhs, rhs);
    assert.equal(ast.strExpr(conj), "f a b >= a and f a b >= b");
  });
});
