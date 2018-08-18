defmodule EvalTest do
  use ExUnit.Case

  defp scan_and_parse(text) do
    {:ok, parsed, "", %{}, _, _} =
      text
      |> Pantagruel.Scan.scan()
      |> Pantagruel.Parse.program()

    parsed
  end

  describe "program evaluation" do
    test "eval happy path" do
      parsed = "f|x:Nat and x > 1| :: Real" |> scan_and_parse

      assert %{
               "x" => %Variable{name: "x", domain: "Nat"},
               "f" => %Function{name: "f", codomain: "Real"}
             } == Pantagruel.Eval.eval(parsed)
    end

    test "eval unbound" do
      parsed = "f|x:X| :: Real" |> scan_and_parse

      exc =
        assert_raise UnboundVariablesError, fn ->
          Pantagruel.Eval.eval(parsed)
        end

      assert exc.unbound == MapSet.new(["X"])
    end

    test "eval late binding" do
      parsed =
        """
        f|x, y:X, Y| :: Real
        y|| => Y
        """
        |> scan_and_parse

      assert %{
               "f" => %Function{name: "f", codomain: "Real"},
               "x" => %Variable{name: "x", domain: "X"},
               "Y" => %Variable{name: "Y", domain: "Y"},
               "y" => %Function{name: "y", codomain: "Y"}
             } == Pantagruel.Eval.eval(parsed)
    end
  end
end
