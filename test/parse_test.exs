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
            expr: ["x", :notequals, "y"],
            expr: ["y", :gt, 1]
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
            expr: ["x", :notequals, "y"],
            expr: [{:intro_op, :or}, "y", :gt, 1]
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
              lambda_args: ["x"],
              lambda_doms: ["Y"],
              predicate: [["a", :in, "Y"]]
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
              refinement: [
                pattern: ["x"],
                subexpr: ["y", :and, "y", :gt, 1]
              ]
            ]
          ]
        ]
      )
    end

    test "parse expression with multiple elements in the pattern" do
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
              refinement: [
                pattern: ["f", "x"],
                subexpr: ["y"]
              ]
            ]
          ]
        ]
      )
    end

    test "parse guarded refinment" do
      text = "f||\nf x⸳x<0←y\nf x←1"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f"
            ]
          ],
          body: [
            expr: [
              refinement: [
                pattern: ["f", "x"],
                guard: ["x", :lt, 0],
                subexpr: ["y"]
              ]
            ],
            expr: [
              refinement: [
                pattern: ["f", "x"],
                subexpr: [1]
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "declaration parsing" do
    test "empty heading parsing" do
      text = "f||⇒D"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              yield_type: :constructor,
              lambda_codomain: "D"
            ]
          ]
        ]
      )
    end

    test "dual heading parsing" do
      text = "f||⇒D\ng||⇒E"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              yield_type: :constructor,
              lambda_codomain: "D"
            ],
            decl: [
              decl_ident: "g",
              yield_type: :constructor,
              lambda_codomain: "E"
            ]
          ]
        ]
      )
    end

    test "basic heading parsing" do
      text = "f|a,b:B,C|∷D"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              lambda_args: ["a", "b"],
              lambda_doms: ["B", "C"],
              yield_type: :function,
              lambda_codomain: "D"
            ]
          ]
        ]
      )
    end

    test "heading with multiple clauses" do
      text = "f|x:Y⸳x != 1,x > 0|"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              lambda_args: ["x"],
              lambda_doms: ["Y"],
              predicate: [["x", :notequals, 1], ["x", :gt, 0]]
            ]
          ]
        ]
      )
    end

    test "heading with sequenced domain" do
      text = "f|x:X|∷[X]"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              lambda_args: ["x"],
              lambda_doms: ["X"],
              yield_type: :function,
              lambda_codomain: {:list, ["X"]}
            ]
          ]
        ]
      )
    end

    test "heading with generic domain" do
      text = "f|x:_A⸳x*y>10|∷[_A]"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              lambda_args: ["x"],
              lambda_doms: ["_A"],
              predicate: [["x", "*", "y", :gt, 10]],
              yield_type: :function,
              lambda_codomain: {:list, ["_A"]}
            ]
          ]
        ]
      )
    end
  end

  describe "program structure" do
    test "two sections" do
      text = "f|x:Y|\n;;\ny||⇒Y"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              lambda_args: ["x"],
              lambda_doms: ["Y"]
            ]
          ]
        ],
        section: [
          head: [
            decl: [
              decl_ident: "y",
              yield_type: :constructor,
              lambda_codomain: "Y"
            ]
          ]
        ]
      )
    end
  end

  describe "comments handling" do
    test "can parse a comment" do
      text = "f||\nf>1\n; Here is a comment.\nf<2"

      tryparse(text,
        section: [
          head: [decl: [decl_ident: "f"]],
          body: [
            expr: ["f", :gt, 1],
            comment: ["Here is a comment."],
            expr: ["f", :lt, 2]
          ]
        ]
      )
    end
  end
end
