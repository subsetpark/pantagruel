defmodule ExpressionParserTest do
  use ExUnit.Case

  defp tryexp(text, r) do
    {:ok, exp, "", %{}, _, _} = Pantagruel.Parse.expression(text)
    assert r == exp
  end

  describe "applications" do
    test "parse symbol" do
      text = "x"
      tryexp(text, ["x"])
    end

    test "parse symbol sequence" do
      text = "x y z"
      tryexp(text, appl: [f: {:appl, [f: "x", x: "y"]}, x: "z"])
    end

    test "parse par symbol sequence" do
      text = "(x y z)"
      tryexp(text, par: [appl: [f: {:appl, [f: "x", x: "y"]}, x: "z"]])
    end

    test "parse set symbol sequence" do
      text = "{x y z}"
      tryexp(text, set: [appl: [f: {:appl, [f: "x", x: "y"]}, x: "z"]])
    end

    test "parse bracketed set symbol sequence" do
      text = "(x,{y z})"
      tryexp(text, par: ["x", set: [{:appl, [f: "y", x: "z"]}]])
    end

    test "parse string followed by integer" do
      text = ~s({D} 10)
      tryexp(text, [{:appl, [f: {:set, ["D"]}, x: 10]}])
    end

    test "function application with float" do
      text = "f 1.0"

      expected = [
        appl: [
          f: "f",
          x: 1.0
        ]
      ]

      tryexp(text, expected)
    end

    test "insert operator" do
      text = "+\\[1,2,3]=6"

      expected = [
        appl: [
          operator: :equals,
          x:
            {:appl,
             [
               operator: :insert,
               x: :plus,
               y: {:list, [1, 2, 3]}
             ]},
          y: 6
        ]
      ]

      tryexp(text, expected)
    end
  end

  describe "comprehensions" do
    test "comprehension parsing" do
      text = "{x∈X⸳x * 2}"

      comprehension_elements = [
        comp_bindings: [
          binding: [
            bind_symbol: "x",
            bind_op: :from,
            bind_domain: "X"
          ]
        ],
        comp_expression: {:appl, [operator: :times, x: "x", y: 2]}
      ]

      tryexp(text,
        comprehension: [set: comprehension_elements]
      )
    end

    test "comprehension parsing 2 elements" do
      text = "{x∈X,y∈Y⸳x * y}"

      comprehension_elements = [
        comp_bindings: [
          binding: [
            bind_symbol: "x",
            bind_op: :from,
            bind_domain: "X"
          ],
          binding: [
            bind_symbol: "y",
            bind_op: :from,
            bind_domain: "Y"
          ]
        ],
        comp_expression: {
          :appl,
          [operator: :times, x: "x", y: "y"]
        }
      ]

      tryexp(text,
        comprehension: [set: comprehension_elements]
      )
    end

    test "comprehension parsing 3 elements" do
      text = "{x∈X,y∈Y,z∈Z⸳x * y ^ z}"

      comprehension_elements = [
        comp_bindings: [
          binding: [
            bind_symbol: "x",
            bind_op: :from,
            bind_domain: "X"
          ],
          binding: [
            bind_symbol: "y",
            bind_op: :from,
            bind_domain: "Y"
          ],
          binding: [
            bind_symbol: "z",
            bind_op: :from,
            bind_domain: "Z"
          ]
        ],
        comp_expression: {
          :appl,
          [operator: :exp, x: {:appl, [operator: :times, x: "x", y: "y"]}, y: "z"]
        }
      ]

      tryexp(text,
        comprehension: [set: comprehension_elements]
      )
    end

    test "comprehension with in and constraint testing" do
      text = "{x∈X,x>1⸳x * y}"

      comprehension_elements = [
        comp_bindings: [
          binding: [
            bind_symbol: "x",
            bind_op: :from,
            bind_domain: "X"
          ],
          guard:
            {:appl,
             [
               operator: :gt,
               x: "x",
               y: 1
             ]}
        ],
        comp_expression: {
          :appl,
          [operator: :times, x: "x", y: "y"]
        }
      ]

      tryexp(text,
        comprehension: [set: comprehension_elements]
      )
    end

    test "exists quantification parsing" do
      text = "∃x:X⸳x>1"

      tryexp(text,
        quantification: [
          quantifier: :exists,
          quant_bindings: [binding: [bind_symbol: "x", bind_op: :in, bind_domain: "X"]],
          quant_expression: {:appl, [operator: :gt, x: "x", y: 1]}
        ]
      )
    end

    test "exists quantification parsing with bunch" do
      text = "∃(x,y):X⸳x"

      tryexp(text,
        quantification: [
          quantifier: :exists,
          quant_bindings: [
            binding: [bind_symbol: {:par, ["x", "y"]}, bind_op: :in, bind_domain: "X"]
          ],
          quant_expression: "x"
        ]
      )
    end
  end

  describe "values" do
    test "empty set" do
      text = "{}"
      tryexp(text, set: [])
    end

    test "literal" do
      text = "`foo"
      tryexp(text, literal: "foo")
    end

    test "spaced literal" do
      text = "`foo 0 Bar`"
      tryexp(text, literal: "foo 0 Bar")
    end

    test "float" do
      text = "1.5"
      tryexp(text, [1.5])
    end

    test "negative float" do
      text = "-1.5"
      tryexp(text, [-1.5])
    end

    test "application parsing" do
      text = "x X"
      tryexp(text, [{:appl, [f: "x", x: "X"]}])
    end

    test "operator parsing" do
      text = "x∈X"
      tryexp(text, [{:appl, [operator: :from, x: "x", y: "X"]}])
    end

    test "unary operator" do
      text = "#x"
      expected = [unary_exp: [op: :card, operand: "x"]]
      tryexp(text, expected)
    end

    test "unary operator with dot expression" do
      text = "#x.f"

      expected = [
        unary_exp: [op: :card, operand: {:dot, [f: "f", x: "x"]}]
      ]

      tryexp(text, expected)
    end

    test "cardinality testing" do
      text = "#x > 3"
      text2 = "# x > 3"
      expected = [appl: [operator: :gt, x: {:unary_exp, [op: :card, operand: "x"]}, y: 3]]

      tryexp(text, expected)
      tryexp(text2, expected)
    end

    test "lambda with comma-joined domain" do
      text = "λ(x:(Y,Z))"

      expected = [
        lambda: [
          lambda_args: ["x"],
          lambda_doms: [par: ["Y", "Z"]]
        ]
      ]

      tryexp(text, expected)
    end

    test "insert" do
      text = "\\"
      expected = [:insert]

      tryexp(text, expected)
    end
  end

  describe "objects" do
    test "object access with string splitting" do
      text = "obj.foo"

      expected = [
        dot: [
          f: "foo",
          x: "obj"
        ]
      ]

      tryexp(text, expected)
    end

    test "object access chaining" do
      text = "foo.bar.baz"

      expected = [
        dot: [
          f: "baz",
          x: {:dot, [f: "bar", x: "foo"]}
        ]
      ]

      tryexp(text, expected)
    end

    test "object access chaining with an operator" do
      text = "f foo.bar.baz > 1"

      chain = {:dot, [f: "baz", x: {:dot, [f: "bar", x: "foo"]}]}
      left_side = {:appl, [f: "f", x: chain]}
      right_side = 1

      expected = [
        appl: [
          operator: :gt,
          x: left_side,
          y: right_side
        ]
      ]

      tryexp(text, expected)
    end

    test "object access is treated as special case of function application" do
      text = "(x - 1).foo"

      expected = [
        dot: [
          f: "foo",
          x: {:par, [appl: [operator: :minus, x: "x", y: 1]]}
        ]
      ]

      tryexp(text, expected)
    end

    test "dot access on a value" do
      text = "x∈1.f"

      expected = [
        appl: [
          operator: :from,
          x: "x",
          y: {:dot, [f: "f", x: 1]}
        ]
      ]

      tryexp(text, expected)
    end

    test "dot access on a complex value" do
      text = "x∈(foo bar).f"

      expected = [
        appl: [
          operator: :from,
          x: "x",
          y: {:dot, [f: "f", x: {:par, [{:appl, f: "foo", x: "bar"}]}]}
        ]
      ]

      tryexp(text, expected)
    end

    test "dot access 2" do
      text = "∀b∈(p).body⸳f"

      expected = [
        quantification: [
          quantifier: :forall,
          quant_bindings: [
            binding: [
              bind_symbol: "b",
              bind_op: :from,
              bind_domain: {:dot, [f: "body", x: {:par, ["p"]}]}
            ]
          ],
          quant_expression: "f"
        ]
      ]

      tryexp(text, expected)
    end

    test "quantification regression" do
      text = "∀u:User⸳(∃ent∈(entities c)⸳c)→c"

      expected = [
        quantification: [
          quantifier: :forall,
          quant_bindings: [
            binding: [bind_symbol: "u", bind_op: :in, bind_domain: "User"]
          ],
          quant_expression:
            {:appl,
             [
               operator: :then,
               x:
                 {:par,
                  [
                    quantification: [
                      quantifier: :exists,
                      quant_bindings: [
                        binding: [
                          bind_symbol: "ent",
                          bind_op: :from,
                          bind_domain: {:par, [appl: [f: "entities", x: "c"]]}
                        ]
                      ],
                      quant_expression: "c"
                    ]
                  ]},
               y: "c"
             ]}
        ]
      ]

      tryexp(text, expected)
    end

    test "quantification regression without parens" do
      text = "∀u:User⸳(∃ent∈entities c⸳c)→c"

      expected = [
        quantification: [
          quantifier: :forall,
          quant_bindings: [binding: [bind_symbol: "u", bind_op: :in, bind_domain: "User"]],
          quant_expression:
            {:appl,
             [
               operator: :then,
               x:
                 {:par,
                  [
                    quantification: [
                      quantifier: :exists,
                      quant_bindings: [
                        binding: [
                          bind_symbol: "ent",
                          bind_op: :from,
                          bind_domain: {:appl, [f: "entities", x: "c"]}
                        ]
                      ],
                      quant_expression: "c"
                    ]
                  ]},
               y: "c"
             ]}
        ]
      ]

      tryexp(text, expected)
    end
  end
end
