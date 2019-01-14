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
                    [{:decl, [{:symbol, 'f'}, [], nil, nil]}],
                    [%BoolAlg{op: :conj, y: {:literal, 'ok'}, x: 1}]
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
                    [decl: [{:symbol, 'f'}, [], nil, nil]],
                    [{:literal, 'ok'}]
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

      assert "f\s\s\n....\s\s\n*ok*\s\s" == out
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

      assert "f\s\s\n....\s\s\n1\s\s" == out
    end
  end

  describe "refinement handling" do
    test "slurp refinement" do
      out =
        """
        f
        ---
        f <- y
        """
        |> tryp()
        |> Convert.convert()
        |> Slurp.slurp()

      assert {:program,
              [
                nil,
                [],
                [
                  chapter: [
                    [decl: [{:symbol, 'f'}, [], nil, nil]],
                    [
                      {:refinement,
                       [
                         {:symbol, 'f'},
                         [case_exp: [true, {:symbol, 'y'}]]
                       ]}
                    ]
                  ]
                ]
              ]} == out
    end

    test "slurp guarded refinement" do
      out =
        """
        f
        ---
        f <- x \\ y
        """
        |> tryp()
        |> Convert.convert()
        |> Slurp.slurp()

      assert {:program,
              [
                nil,
                [],
                [
                  chapter: [
                    [decl: [{:symbol, 'f'}, [], nil, nil]],
                    [
                      %BoolAlg{
                        op: :conj,
                        x: {:symbol, 'x'},
                        y: %BoolAlg{
                          op: :conj,
                          x: {:symbol, 'x'},
                          y:
                            {:refinement,
                             [
                               {:symbol, 'f'},
                               [
                                 case_exp: [true, {:symbol, 'y'}]
                               ]
                             ]}
                        }
                      }
                    ]
                  ]
                ]
              ]} == out
    end

    test "assert guarded refinement" do
      out =
        """
        f
        ---
        f <- x \\ y
        """
        |> tryp()
        |> Convert.convert()
        |> Slurp.slurp()
        |> BoolAlg.assert({:symbol, 'x'})

      assert {:program,
              [
                nil,
                [],
                [
                  chapter: [
                    [decl: [{:symbol, 'f'}, [], nil, nil]],
                    [
                      {:refinement,
                       [
                         {:symbol, 'f'},
                         [case_exp: [true, {:symbol, 'y'}]]
                       ]}
                    ]
                  ]
                ]
              ]} == out
    end

    test "print guarded refinement" do
      out =
        """
        f
        ---
        f <- x \\ y
        """
        |> tryp()
        |> Convert.convert()
        |> Slurp.slurp()
        |> BoolAlg.assert({:symbol, 'x'})
        |> Pantagruel.Format.format_program()

      assert "f  \n....  \nf ← y  " == out
    end

    test "slurp multi-clause refinement" do
      out =
        """
        f
        ---
        f <- (x \\ y, z \\ g)
        """
        |> tryp()
        |> Convert.convert()
        |> Slurp.slurp()

      assert {:program,
              [
                nil,
                [],
                [
                  chapter: [
                    [decl: [{:symbol, 'f'}, [], nil, nil]],
                    [
                      %BoolAlg{
                        op: :conj,
                        x: %BoolAlg{op: :disj, x: {:symbol, 'x'}, y: {:symbol, 'z'}},
                        y: %BoolAlg{
                          op: :disj,
                          x: %BoolAlg{
                            op: :conj,
                            x: {:symbol, 'x'},
                            y:
                              {:refinement,
                               [
                                 {:symbol, 'f'},
                                 [
                                   case_exp: [true, {:symbol, 'y'}]
                                 ]
                               ]}
                          },
                          y: %BoolAlg{
                            op: :conj,
                            x: {:symbol, 'z'},
                            y: {:refinement, [{:symbol, 'f'}, [case_exp: [true, {:symbol, 'g'}]]]}
                          }
                        }
                      }
                    ]
                  ]
                ]
              ]} == out
    end

    test "print multi-clause refinement" do
      out =
        """
        f
        ---
        f <- (x \\ y, z \\ g)
        """
        |> tryp()
        |> Convert.convert()
        |> Slurp.slurp()
        |> Pantagruel.Format.format_program()

      assert "f  \n....  \n((x ∨ z) ∧ ((x ∧ f ← y) ∨ (z ∧ f ← g)))  " == out
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
