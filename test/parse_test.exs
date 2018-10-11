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
            expr: [appl: [operator: :notequals, x: "x", y: "y"]],
            expr: [appl: [operator: :gt, x: "y", y: 1]]
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
            expr: [appl: [operator: :notequals, x: "x", y: "y"]],
            expr: [intro_op: :or, appl: [operator: :gt, x: "y", y: 1]]
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
              predicate: [{:appl, [operator: :in, x: "a", y: "Y"]}]
            ]
          ]
        ]
      )
    end

    test "parse expression with relation in it" do
      text = "f||\nx←y ∧ y > 1"

      tryparse(text,
        section: [
          head: [decl: [decl_ident: "f"]],
          body: [
            expr: [
              refinement: [
                pattern: "x",
                expr: {:appl, [operator: :gt, x: {:appl, [operator: :and, x: "y", y: "y"]}, y: 1]}
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
                pattern: {:appl, [f: "f", x: "x"]},
                expr: "y"
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
          head: [decl: [decl_ident: "f"]],
          body: [
            expr: [
              refinement: [
                pattern: {:appl, [f: "f", x: "x"]},
                guard: {:appl, [operator: :lt, x: "x", y: 0]},
                expr: "y"
              ]
            ],
            expr: [refinement: [pattern: {:appl, [f: "f", x: "x"]}, expr: 1]]
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
              predicate: [
                {:appl, [operator: :notequals, x: "x", y: 1]},
                {:appl, [operator: :gt, x: "x", y: 0]}
              ]
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
              predicate: [
                appl: [operator: :gt, x: {:appl, [operator: :times, x: "x", y: "y"]}, y: 10]
              ],
              yield_type: :function,
              lambda_codomain: {:list, ["_A"]}
            ]
          ]
        ]
      )
    end

    test "heading with lambda" do
      text = "f|x:|z:Nat|∷z|∷Bool"

      tryparse(text,
        section: [
          head: [
            decl: [
              decl_ident: "f",
              lambda_args: ["x"],
              lambda_doms: [
                {:lambda,
                 [
                   lambda_args: ["z"],
                   lambda_doms: ["Nat"],
                   yield_type: :function,
                   lambda_codomain: "z"
                 ]}
              ],
              yield_type: :function,
              lambda_codomain: "Bool"
            ]
          ]
        ]
      )
    end

    test "domain aliasing" do
      text = "{`ok}⇒Status"

      tryparse(text,
        section: [head: [alias: [alias_expr: {:set, [literal: "ok"]}, alias_name: ["Status"]]]]
      )
    end

    test "multiple domain aliasing" do
      text = "{`ok}⇒Status,State"

      tryparse(text,
        section: [
          head: [
            alias: [
              alias_expr: {:set, [literal: "ok"]},
              alias_name: ["Status", "State"]
            ]
          ]
        ]
      )
    end

    test "comprehension aliasing" do
      text = "{n:Nat,n<=30⸳n}⇒Day"

      tryparse(text,
        section: [
          head: [
            alias: [
              alias_expr:
                {:comprehension,
                 [
                   set: [
                     {:comp_bindings,
                      [
                        binding: [bind_symbol: "n", bind_op: :in, bind_domain: "Nat"],
                        guard: {:appl, [operator: :lte, x: "n", y: 30]}
                      ]},
                     {:comp_expression, "n"}
                   ]
                 ]},
              alias_name: ["Day"]
            ]
          ]
        ]
      )
    end
  end

  describe "program structure" do
    test "two sections" do
      text = "f|x:Y|\n;\ny||⇒Y"

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
      text = "f||\nf>1\n\" Here is a comment.\nf<2"

      tryparse(text,
        section: [
          head: [decl: [decl_ident: "f"]],
          body: [
            expr: [appl: [operator: :gt, x: "f", y: 1]],
            comment: ["Here is a comment."],
            expr: [appl: [operator: :lt, x: "f", y: 2]]
          ]
        ]
      )
    end
  end
end
