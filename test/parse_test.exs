defmodule PantagruelTest do
  use ExUnit.Case

  defp tryparse(text, r) do
    {:ok, program, "", %{}, _, _} = Pantagruel.Parse.program(text)
    assert r == program
  end

  describe "expression parsing" do
    test "parse two expressions" do
      text = "f||\nx != y\ny > 1"

      tryparse(text,
        decl: [
          decl_ident: "f"
        ],
        expr: [right: ["x", :notequals, "y"]],
        expr: [right: ["y", :gt, 1]]
      )
    end

    test "parse two expressions with connecting operator" do
      text = "f||\nx != y\n∨y > 1"

      tryparse(text,
        decl: [
          decl_ident: "f"
        ],
        expr: [right: ["x", :notequals, "y"]],
        expr: [intro_op: :or, right: ["y", :gt, 1]]
      )
    end

    test "parse expression with domain" do
      text = "f|x:Y∧a:Y|"

      tryparse(text,
        decl: [
          decl_ident: "f",
          decl_args: ["x"],
          decl_doms: ["Y"],
          expr: [right: ["a", :in, "Y"]]
        ]
      )
    end

    test "parse expression with relation in it" do
      text = "f||\nx=y ∧ y > 1"

      tryparse(text,
        decl: [
          decl_ident: "f"
        ],
        expr: [
          left: ["x"],
          op: :iff,
          right: ["y", :and, "y", :gt, 1]
        ]
      )
    end

    test "parse expression with multiple elements on the left" do
      text = "f||\nf x→y"

      tryparse(text,
        decl: [
          decl_ident: "f"
        ],
        expr: [
          left: ["f", "x"],
          op: :then,
          right: ["y"]
        ]
      )
    end
  end

  describe "declaration parsing" do
    test "empty heading parsing" do
      text = "f||=>D"

      tryparse(text,
        decl: [
          decl_ident: "f",
          yield_type: :produces,
          yield_domain: "D"
        ]
      )
    end

    test "dual heading parsing" do
      text = "f||=>D\ng||=>E"

      tryparse(text,
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
      )
    end

    test "basic heading parsing" do
      text = "f|a,b:B,C|::D"

      {:ok, [decl: [ident, vars, doms, yield_type, yield_domain]], "", _, _, _} =
        Pantagruel.Parse.program(text)

      assert ident == {:decl_ident, "f"}
      assert vars == {:decl_args, ["a", "b"]}
      assert doms == {:decl_doms, ["B", "C"]}
      assert yield_type == {:yield_type, :yields}
      assert yield_domain == {:yield_domain, "D"}
    end

    test "heading with multiple clauses" do
      text = "f|x:Y∧x != 1|"

      tryparse(text,
        decl: [
          decl_ident: "f",
          decl_args: ["x"],
          decl_doms: ["Y"],
          expr: [right: ["x", :notequals, 1]]
        ]
      )
    end

    test "heading with sequenced domain" do
      text = "f|x:X|::[X]"

      tryparse(text,
        decl: [
          decl_ident: "f",
          decl_args: ["x"],
          decl_doms: ["X"],
          yield_type: :yields,
          yield_domain: {:list, ["X"]}
        ]
      )
    end
  end

  describe "program structure" do
    test "two sections" do
      text = "f|x:Y|\n;;\ny||=>Y"

      {:ok, [decl: decl, decl: decl2], "", %{}, _, _} = Pantagruel.Parse.program(text)

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
