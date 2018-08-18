defmodule ExpressionParserTest do
  use ExUnit.Case

  defp tryexp(text, r) do
    {:ok, subexp, "", %{}, _, _} = Pantagruel.Parse.subexpression(text)
    assert r == subexp
  end

  describe "subexpression parsing" do
    test "parse symbol sequence" do
      text = "x y z"
      tryexp(text, ["x", "y", "z"])
    end

    test "parse bunch symbol sequence" do
      text = "(x y z)"
      tryexp(text, bunch: [["x", "y", "z"]])
    end

    test "parse set symbol sequence" do
      text = "{x y z}"
      tryexp(text, set: [["x", "y", "z"]])
    end

    test "parse bunched set symbol sequence" do
      text = "(x,{y z})"
      tryexp(text, bunch: [["x"], [set: [["y", "z"]]]])
    end

    test "parse string followed by integer" do
      text = ~s("D" 10)
      tryexp(text, [{:string, [["D"]]}, 10])
    end

    test "comprehension parsing" do
      text = "{x * 2,x ∈ X}"
      tryexp(text, [:comprehension, :set, [{"x", "X"}], ["x", "*", 2]])
    end

    test "comprehension parsing 2 elements" do
      text = "{x * y,x ∈ X,y ∈ Y}"
      tryexp(text, [:comprehension, :set, [{"x", "X"}, {"y", "Y"}], ["x", "*", "y"]])
    end

    test "comprehension parsing 3 elements" do
      text = "{x * y ^ z,x ∈ X,y ∈ Y,z ∈ Z}"

      tryexp(text, [
        :comprehension,
        :set,
        [{"x", "X"}, {"y", "Y"}, {"z", "Z"}],
        ["x", "*", "y", "^", "z"]
      ])
    end

    test "exists quantifier parsing" do
      text = "∃x:X x>1"
      tryexp(text, [:exists, "x", :in, "X", ["x", :gt, 1]])
    end

    test "empty set" do
      text = "{}"
      tryexp(text, set: [])
    end

    test "empty string" do
      text = "\"\""
      tryexp(text, string: [])
    end
  end
end
