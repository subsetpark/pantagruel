defmodule Pantagruel.PrintTest do
  use ExUnit.Case
  alias Pantagruel.Print

  defp eval(text) do
    with scanned <- Pantagruel.Scan.scan(text),
         {:ok, parsed, "", %{}, _, _} <- Pantagruel.Parse.program(scanned) do
      Pantagruel.Eval.eval(parsed)
    end
  end

  describe "scope display" do
    test "null case" do
      assert "" == "" |> eval |> Print.to_string()
    end

    test "minimal function" do
      string =
        """
        f||
        """
        |> eval
        |> Print.to_string()

      assert "f : ||" == string
    end

    test "function" do
      string =
        """
        f|x: Nat| :: Real
        """
        |> eval
        |> Print.to_string()

      assert "f : |ℕ| :: ℝ\nx : ℕ\nx' : ℕ" == string
    end
  end
end
