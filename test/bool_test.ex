defmodule BoolEvalTest do
  alias Pantagruel.Bool.{Convert, Slurp, Eval}
  use ExUnit.Case

  describe "test bool eval" do
    test "arith test" do
      tree =
        """
        f.
        ---
        (x = 3) or (x = 5).
        """
        |> tryp()
        |> Convert.convert()
        |> Slurp.slurp()

      prop = Pantagruel.Shell.get_ast("x = 3.")

      replaced =
        tree
        |> BoolAlg.replace_with(prop, &Eval.test/2, true)

      assert {:program,
              [
                nil,
                [],
                [
                  chapter: [
                    [decl: [{:symbol, 'f'}, [], nil, nil]],
                    [
                      %BoolAlg{
                        op: :disj,
                        x: true,
                        y: {:cont, [:par, [bin_appl: [:=, {:symbol, 'x'}, 5]]]}
                      }
                    ]
                  ]
                ]
              ]} == replaced
    end
  end

  defp tryp(string) do
    with scanned <- Pantagruel.Scan.scan(string),
         {:ok, tokens, _} <- :pant_lexer.string(scanned),
         {:ok, parsed} <- :pant_parser.parse(tokens) do
      parsed
    else
      e -> {:error, e}
    end
  end
end
