defmodule LifterTest do
  alias Pantagruel.Bool.{Convert, Slurp}
  use ExUnit.Case

  describe "lift" do
    test "lift base case" do
      tree =
        """
        f
        """
        |> tryp()

      assert tree == Convert.convert(tree)
    end

    test "Convert expr" do
      tree =
        """
        f x and y
        """
        |> tryp()
        |> Convert.convert()

      assert {:program,
              [
                nil,
                [],
                [
                  chapter: [
                    [
                      decl: [
                        {:symbol, 'f'},
                        [guard: %BoolAlg{op: :conj, x: {:symbol, 'x'}, y: {:symbol, 'y'}}],
                        nil,
                        nil
                      ]
                    ],
                    []
                  ]
                ]
              ]} == tree
    end

    test "lift line" do
      tree =
        """
        f
        ---
        1
        and `ok
        """
        |> tryp()
        |> Slurp.slurp()

      assert {:program,
              [
                nil,
                [],
                [
                  chapter: [
                    [%BoolAlg{op: :conj, x: true, y: {:decl, [{:symbol, 'f'}, [], nil, nil]}}],
                    [
                      %BoolAlg{
                        op: :conj,
                        x: %BoolAlg{op: :conj, x: true, y: 1},
                        y: {:literal, 'ok'}
                      }
                    ]
                  ]
                ]
              ]} == tree
    end

    test "assert line" do
      tree =
        """
        f
        ---
        1
        and `ok
        """
        |> tryp()
        |> Convert.convert()
        |> Slurp.slurp()
        |> BoolAlg.assert(1)

      assert {:program,
              [
                nil,
                [],
                [
                  chapter: [
                    [{:decl, [{:symbol, 'f'}, [], nil, nil]}],
                    [%BoolAlg{op: :conj, x: true, y: {:literal, 'ok'}}]
                  ]
                ]
              ]} == tree
    end

    test "print line" do
      out =
        """
        f
        ---
        1
        and `ok
        """
        |> tryp()
        |> Convert.convert()
        |> Slurp.slurp()
        |> BoolAlg.assert(1)
        |> Pantagruel.Format.format_program()

      assert "f\s\s\n....\s\s\ntrue conj *ok*\s\s" == out
    end

    test "print other half of line" do
      out =
        """
        f
        ---
        1
        and `ok
        """
        |> tryp()
        |> Convert.convert()
        |> Slurp.slurp()
        |> BoolAlg.assert({:literal, 'ok'})
        |> Pantagruel.Format.format_program()

      assert "f\s\s\n....\s\s\ntrue conj 1\s\s" == out
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
