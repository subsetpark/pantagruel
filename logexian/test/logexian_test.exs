defmodule LogexianTest do
  use ExUnit.Case

  defp tryexp(text, r) do
    {:ok, subexp, "", %{}, _, _} = Logexian.Parse.subexpression(text)
    assert r == subexp
  end

  describe "subexpression parsing" do
    test "parse symbol sequence" do
      text = "x y z"
      tryexp(text, ["x", "y", "z"])
    end

    test "parse bunch symbol sequence" do
      text = "(x y z)"
      tryexp(text, bunch: ["x", "y", "z"])
    end

    test "parse set symbol sequence" do
      text = "{x y z}"
      tryexp(text, set: ["x", "y", "z"])
    end

    test "parse bunched set symbol sequence" do
      text = "(x {y z})"
      tryexp(text, bunch: ["x", set: ["y", "z"]])
    end

    test "parse string followed by integer" do
      text = ~s("D" 10)
      tryexp(text, [{:string, ["D"]}, 10])
    end
  end

  defp tryparse(text, r) do
    {:ok, program, "", %{}, _, _} = Logexian.Parse.program(text)
    assert r == program
  end

  describe "expression parsing" do
    test "parse two expressions" do
      text = "f||\nx != y\ny > 1"

      tryparse(text,
        sect: [
          decl: [
            decl_ident: "f"
          ],
          expr: [right: ["x", :notequals, "y"]],
          expr: [right: ["y", :gt, 1]]
        ]
      )
    end

    test "parse two expressions with connecting operator" do
      text = "f||\nx != y\n∨ y > 1"

      tryparse(text,
        sect: [
          decl: [
            decl_ident: "f"
          ],
          expr: [right: ["x", :notequals, "y"]],
          expr: [intro_op: :or, right: ["y", :gt, 1]]
        ]
      )
    end

    test "parse expression with domain" do
      text = "f|x:Y ∧ a:Y|"

      tryparse(text,
        sect: [
          decl: [
            decl_ident: "f",
            decl_args: ["x"],
            decl_doms: ["Y"],
            expr: [right: ["a", :in, "Y"]]
          ]
        ]
      )
    end

    test "parse expression with relation in it" do
      text = "f||\nx=y ∧ y > 1"

      tryparse(text,
        sect: [
          decl: [
            decl_ident: "f"
          ],
          expr: [
            left: ["x"],
            op: :iff,
            right: ["y", :and, "y", :gt, 1]
          ]
        ]
      )
    end

    test "parse expression with multiple elements on the left" do
      text = "f||\nf x→y"

      tryparse(text,
        sect: [
          decl: [
            decl_ident: "f"
          ],
          expr: [
            left: ["f", "x"],
            op: :then,
            right: ["y"]
          ]
        ]
      )
    end
  end

  describe "declaration parsing" do
    test "empty heading parsing" do
      text = "f||=>D"

      tryparse(text,
        sect: [
          decl: [
            decl_ident: "f",
            yield_type: :produces,
            yield_domain: "D"
          ]
        ]
      )
    end

    test "dual heading parsing" do
      text = "f||=>D\ng||=>E"

      tryparse(text,
        sect: [
          decl: [
            decl_ident: "f",
            yield_type: :produces,
            yield_domain: "D"
          ],
          decl: [
            decl_ident: "g",
            yield_type: :produces,
            yield_domain: "E"
          ]
        ]
      )
    end

    test "basic heading parsing" do
      text = "f|a,b:B,C|::D"

      {:ok, [sect: [decl: [ident, vars, doms, yield_type, yield_domain]]], "", _, _, _} =
        Logexian.Parse.program(text)

      assert ident == {:decl_ident, "f"}
      assert vars == {:decl_args, ["a", "b"]}
      assert doms == {:decl_doms, ["B", "C"]}
      assert yield_type == {:yield_type, :yields}
      assert yield_domain == {:yield_domain, "D"}
    end

    test "heading with multiple clauses" do
      text = "f|x:Y ∧ x != 1|"

      tryparse(text,
        sect: [
          decl: [
            decl_ident: "f",
            decl_args: ["x"],
            decl_doms: ["Y"],
            expr: [right: ["x", :notequals, 1]]
          ]
        ]
      )
    end
  end

  describe "program structure" do
    test "two sections" do
      text = "f|x:Y|\n;;\ny||=>Y"

      {:ok, [sect: [decl: decl], sect: [decl: decl2]], "", %{}, _, _} =
        Logexian.Parse.program(text)

      assert [
               decl_ident: "f",
               decl_args: ["x"],
               decl_doms: ["Y"]
             ] == decl

      assert [
               decl_ident: "y",
               yield_type: :produces,
               yield_domain: "Y"
             ] == decl2
    end
  end
end
