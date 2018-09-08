defmodule Pantagruel.PrintTest do
  use ExUnit.Case
  alias Pantagruel.Print

  defp eval(text) do
    with scanned <- Pantagruel.Scan.scan(text),
         {:ok, parsed, "", %{}, _, _} <- Pantagruel.Parse.program(scanned) do
      {parsed, Pantagruel.Eval.eval(parsed)}
    end
  end

  describe "scope display" do
    test "null case" do
      {parsed, scopes} = eval("")
      assert "" == Print.to_string(parsed, scopes)
    end

    test "minimal function" do
      {parsed, scopes} =
        """
        f||
        """
        |> eval

      assert "f : ||\n――――――――――\nf : ||\n" == Print.to_string(parsed, scopes)
    end

    test "function" do
      {parsed, scopes} =
        """
        f|x: Nat| :: Real
        """
        |> eval

      assert "f : |ℕ| :: ℝ\nx : ℕ\n――――――――――\nf : |x:ℕ| :: ℝ\n" ==
               Print.to_string(parsed, scopes)
    end

    test "constructor" do
      {parsed, scopes} =
        """
        f|| => F
        """
        |> eval

      assert "[F]\nf : || ⇒ F\n――――――――――\nf : || ⇒ F\n" == Print.to_string(parsed, scopes)
    end

    test "section" do
      {parsed, scopes} =
        """
        f|| => F
        f 1 = 0
        """
        |> eval

      assert "[F]\nf : || ⇒ F\n――――――――――\nf : || ⇒ F\nf 1 ⇔ 0" == Print.to_string(parsed, scopes)
    end
  end
end
