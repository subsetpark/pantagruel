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
        section: [
          head: [
            decl: [
              decl_ident: "f"
            ]
          ],
          body: [
            expr: [right: ["x", :notequals, "y"]],
            expr: [right: ["y", :gt, 1]]
          ]
        ]
      )
    end

    test "parse two expressions with connecting operator" do
      text = "f||\nx != y\n∨y > 1"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f"
            ]
          ],
          body: [
            expr: [right: ["x", :notequals, "y"]],
            expr: [intro_op: :or, right: ["y", :gt, 1]]
          ]
        ]
      )
    end

    test "parse expression with domain" do
      text = "f|x:Y⸳a:Y|"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              decl_args: ["x"],
              decl_doms: ["Y"],
              expr: [right: ["a", :in, "Y"]]
            ]
          ]
        ]
      )
    end

    test "parse expression with relation in it" do
      text = "f||\nx←y ∧ y > 1"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f"
            ]
          ],
          body: [
            expr: [
              left: ["x"],
              right: ["y", :and, "y", :gt, 1]
            ]
          ]
        ]
      )
    end

    test "parse expression with multiple elements on the left" do
      text = "f||\nf x←y"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f"
            ]
          ],
          body: [
            expr: [
              left: ["f", "x"],
              right: ["y"]
            ]
          ]
        ]
      )
    end
  end

  describe "declaration parsing" do
    test "empty heading parsing" do
      text = "f||=>D"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              yield_type: :constructor,
              yield_domain: "D"
            ]
          ]
        ]
      )
    end

    test "dual heading parsing" do
      text = "f||=>D\ng||=>E"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              yield_type: :constructor,
              yield_domain: "D"
            ],
            decl: [
              decl_ident: "g",
              yield_type: :constructor,
              yield_domain: "E"
            ]
          ]
        ]
      )
    end

    test "basic heading parsing" do
      text = "f|a,b:B,C|::D"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              decl_args: ["a", "b"],
              decl_doms: ["B", "C"],
              yield_type: :function,
              yield_domain: "D"
            ]
          ]
        ]
      )
    end

    test "heading with multiple clauses" do
      text = "f|x:Y⸳x != 1|"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              decl_args: ["x"],
              decl_doms: ["Y"],
              expr: [right: ["x", :notequals, 1]]
            ]
          ]
        ]
      )
    end

    test "heading with sequenced domain" do
      text = "f|x:X|::[X]"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              decl_args: ["x"],
              decl_doms: ["X"],
              yield_type: :function,
              yield_domain: {:list, ["X"]}
            ]
          ]
        ]
      )
    end
  end

  describe "program structure" do
    test "two sections" do
      text = "f|x:Y|\n;;\ny||=>Y"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              decl_args: ["x"],
              decl_doms: ["Y"]
            ]
          ]
        ],
        section: [
          head: [
            decl: [
              decl_ident: "y",
              yield_type: :constructor,
              yield_domain: "Y"
            ]
          ]
        ]
      )
    end
  end
end
