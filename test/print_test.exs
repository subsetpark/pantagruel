defmodule Pantagruel.FormatTest do
  use ExUnit.Case
  alias Pantagruel.Format

  defp eval(text) do
    with scanned <- Pantagruel.Scan.scan(text),
         {:ok, parsed, "", %{}, _, _} <- Pantagruel.Parse.program(scanned) do
      {parsed, Pantagruel.Eval.eval(parsed)}
    end
  end

  describe "scope display" do
    test "null case" do
      {parsed, scopes} = eval("")
      assert "" == Format.format_program(parsed, scopes)
    end

    test "minimal function" do
      {parsed, scopes} =
        """
        f||
        """
        |> eval

      assert "f : ||\n――――――――――\nf : ||" == Format.format_program(parsed, scopes)
    end

    test "function" do
      {parsed, scopes} =
        """
        f|x: Nat| :: Real
        """
        |> eval

      assert "f : |ℕ| ∷ ℝ\nx : ℕ\n――――――――――\nf : |x:ℕ| ∷ ℝ" ==
               Format.format_program(parsed, scopes)
    end

    test "constructor" do
      {parsed, scopes} =
        """
        f|| => F
        """
        |> eval

      assert "F ⇒ F\nf : || ⇒ F\n――――――――――\nf : || ⇒ F" == Format.format_program(parsed, scopes)
    end

    test "section" do
      {parsed, scopes} =
        """
        f|| => F
        f 1 = 0
        """
        |> eval

      assert "F ⇒ F\nf : || ⇒ F\n――――――――――\nf : || ⇒ F\nf 1 ⇔ 0" ==
               Format.format_program(parsed, scopes)
    end
  end
end
