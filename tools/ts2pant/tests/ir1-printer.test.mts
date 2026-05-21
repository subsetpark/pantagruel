import assert from "node:assert/strict";
import { describe, it } from "node:test";

import {
  ir1App,
  ir1Binop,
  ir1Cond,
  ir1Each,
  ir1IsNullish,
  ir1LitBool,
  ir1LitNat,
  ir1LitString,
  ir1MapRead,
  ir1Member,
  ir1SetRead,
  ir1Unop,
  ir1Var,
} from "../src/ir1.js";
import { formatIR1Expr } from "../src/ir1-printer.js";

describe("formatIR1Expr", () => {
  it("formats var expressions", () => {
    assert.equal(formatIR1Expr(ir1Var("balance")), "balance");
    assert.equal(formatIR1Expr(ir1Var("balance", true)), "balance'");
  });

  it("formats lit expressions", () => {
    assert.equal(formatIR1Expr(ir1LitNat(42)), "42");
    assert.equal(formatIR1Expr(ir1LitBool(false)), "false");
    assert.equal(
      formatIR1Expr(ir1LitString('a "quoted" value')),
      '"a \\"quoted\\" value"',
    );
  });

  it("formats binop expressions", () => {
    assert.equal(
      formatIR1Expr(ir1Binop("neq", ir1Var("left"), ir1Var("right"))),
      "(left ~= right)",
    );
    assert.equal(
      formatIR1Expr(
        ir1Binop(
          "and",
          ir1Binop("le", ir1Var("x"), ir1LitNat(3)),
          ir1Binop("ge", ir1Var("y"), ir1LitNat(2)),
        ),
      ),
      "((x <= 3) and (y >= 2))",
    );
  });

  it("formats unop expressions", () => {
    assert.equal(formatIR1Expr(ir1Unop("not", ir1Var("ready"))), "~ready");
    assert.equal(formatIR1Expr(ir1Unop("neg", ir1Var("delta"))), "(-delta)");
    assert.equal(formatIR1Expr(ir1Unop("card", ir1Var("items"))), "#items");
    assert.equal(
      formatIR1Expr(ir1Unop("not", ir1Binop("eq", ir1Var("x"), ir1LitNat(0)))),
      "~(x = 0)",
    );
  });

  it("formats app expressions", () => {
    assert.equal(
      formatIR1Expr(
        ir1App(ir1Var("f"), [
          ir1Var("x"),
          ir1Binop("add", ir1Var("y"), ir1LitNat(1)),
        ]),
      ),
      "f x (y + 1)",
    );
    assert.equal(
      formatIR1Expr(ir1App(ir1App(ir1Var("f"), [ir1Var("x")]), [ir1Var("y")])),
      "f x y",
    );
  });

  it("formats member expressions", () => {
    assert.equal(
      formatIR1Expr(ir1Member(ir1Var("account"), "Account_balance")),
      "Account_balance account",
    );
    assert.equal(
      formatIR1Expr(
        ir1Member(ir1Member(ir1Var("account"), "profile"), "Profile_name"),
      ),
      "Profile_name (profile account)",
    );
  });

  it("formats cond expressions", () => {
    assert.equal(
      formatIR1Expr(
        ir1Cond(
          [
            [ir1Var("g1"), ir1LitNat(1)],
            [ir1Var("g2"), ir1LitNat(2)],
          ],
          ir1LitNat(0),
        ),
      ),
      "cond g1 => 1, g2 => 2, true => 0",
    );
  });

  it("formats is-nullish expressions", () => {
    assert.equal(
      formatIR1Expr(ir1IsNullish(ir1Member(ir1Var("user"), "User_name"))),
      "nullish? User_name user",
    );
  });

  it("formats each expressions", () => {
    assert.equal(
      formatIR1Expr(
        ir1Each(
          "n",
          ir1Var("nums"),
          [ir1Binop("gt", ir1Var("n"), ir1LitNat(0))],
          ir1Binop("mul", ir1Var("n"), ir1LitNat(2)),
        ),
      ),
      "each n in nums | (n * 2), when (n > 0)",
    );
  });

  it("formats map-read expressions", () => {
    assert.equal(
      formatIR1Expr(
        ir1MapRead(
          "get",
          "Score_value",
          "Score_hasKey",
          "Scoreboard",
          "Player",
          ir1Var("scores"),
          ir1Var("player"),
        ),
      ),
      "Score_value scores player",
    );
    assert.equal(
      formatIR1Expr(
        ir1MapRead(
          "has",
          "Score_value",
          "Score_hasKey",
          "Scoreboard",
          "Player",
          ir1Var("scores"),
          ir1Var("player"),
        ),
      ),
      "Score_hasKey scores player",
    );
  });

  it("formats set-read expressions", () => {
    assert.equal(
      formatIR1Expr(
        ir1SetRead(
          "Group_members",
          "Group",
          "User",
          ir1Var("group"),
          ir1Var("user"),
        ),
      ),
      "user in group",
    );
  });
});
