defmodule ExpressionParserTest do
  use ExUnit.Case

  defp tryexp(text, r) do
    {:ok, exp, "", %{}, _, _} = Pantagruel.Parse.expression(text)
    assert r == exp
  end

  describe "expression parsing" do
    test "parse symbol sequence" do
      text = "x y z"
      tryexp(text, appl: [f: {:appl, [f: "x", x: "y"]}, x: "z"])
    end

    test "parse bunch symbol sequence" do
      text = "(x y z)"
      tryexp(text, bunch: [appl: [f: {:appl, [f: "x", x: "y"]}, x: "z"]])
    end

    test "parse set symbol sequence" do
      text = "{x y z}"
      tryexp(text, set: [appl: [f: {:appl, [f: "x", x: "y"]}, x: "z"]])
    end

    test "parse bunched set symbol sequence" do
      text = "(x,{y z})"
      tryexp(text, bunch: ["x", set: [{:appl, [f: "y", x: "z"]}]])
    end

    test "parse string followed by integer" do
      text = ~s("D" 10)
      tryexp(text, [{:appl, [f: {:string, ["D"]}, x: 10]}])
    end

    test "comprehension parsing" do
      text = "{x∈X⸳x * 2}"

      comprehension_elements = [
        [appl: [operator: :from, x: "x", y: "X"]],
        appl: [operator: :times, x: "x", y: 2]
      ]

      tryexp(text,
        comprehension: [set: comprehension_elements]
      )
    end

    test "comprehension parsing 2 elements" do
      text = "{x∈X,y∈Y⸳x * y}"

      comprehension_elements = [
        [
          appl: [operator: :from, x: "x", y: "X"],
          appl: [operator: :from, x: "y", y: "Y"]
        ],
        appl: [operator: :times, x: "x", y: "y"]
      ]

      [
        [appl: [operator: :from, x: "x", y: "X"]],
        [appl: [operator: :from, x: "y", y: "Y"]],
        [appl: [operator: :times, x: "x", y: "y"]]
      ]

      tryexp(text,
        comprehension: [set: comprehension_elements]
      )
    end

    test "comprehension parsing 3 elements" do
      text = "{x∈X,y∈Y,z∈Z⸳x * y ^ z}"

      comprehension_elements = [
        set: [
          [
            appl: [operator: :from, x: "x", y: "X"],
            appl: [operator: :from, x: "y", y: "Y"],
            appl: [operator: :from, x: "z", y: "Z"]
          ],
          appl: [operator: :exp, x: {:appl, [operator: :times, x: "x", y: "y"]}, y: "z"]
        ]
      ]

      tryexp(text,
        comprehension: comprehension_elements
      )
    end

    test "exists quantifier parsing" do
      text = "∃x:X⸳x>1"

      tryexp(text,
        quantifier: [
          quant_operator: :exists,
          quant_bindings: [{:appl, [operator: :in, x: "x", y: "X"]}],
          quant_expression: {:appl, [operator: :gt, x: "x", y: 1]}
        ]
      )
    end

    test "empty set" do
      text = "{}"
      tryexp(text, set: [])
    end

    test "empty string" do
      text = "\"\""
      tryexp(text, string: [])
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

    test "cardinality testing" do
      text = "#x > 3"
      text2 = "# x > 3"
      expected = [appl: [operator: :gt, x: {:appl, [f: :card, x: "x"]}, y: 3]]

      tryexp(text, expected)
      tryexp(text2, expected)
    end

    test "lambda with comma-joined domain" do
      text = "|x:(Y,Z)|"

      expected = [
        lambda: [
          lambda_args: ["x"],
          lambda_doms: [bunch: ["Y", "Z"]]
        ]
      ]

      tryexp(text, expected)
    end

    test "object access is treated as special case of function application" do
      text = "(x - 1).foo"

      expected = [
        appl: [
          f: ".foo",
          x: {:bunch, [appl: [operator: :minus, x: "x", y: 1]]},
        ]
      ]

      tryexp(text, expected)
    end
  end
end
