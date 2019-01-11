defmodule Pantagruel.Test.LegacyParser do
  use ExUnit.Case

  defp tryparse(text, r) do
    {:ok, program} =
      Pantagruel.Scan.scan(text)
      |> :pant_lexer.string()
      |> Pantagruel.Parse.handle_lex()

    assert r == program
  end

  describe "expression parsing" do
    test "parse two expressions" do
      text = "f\n---\nx != y\ny > 1"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [nil, {:bin_appl, [:!=, {:symbol, 'x'}, {:symbol, 'y'}]}],
                 expr: [nil, {:bin_appl, [:>, {:symbol, 'y'}, 1]}]
               ]
             ]
           ]
         ]}
      )
    end

    test "parse two expressions with connecting op" do
      text = """
      f
      ---
      x != y
      or y > 1
      """

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [nil, {:bin_appl, [:!=, {:symbol, 'x'}, {:symbol, 'y'}]}],
                 expr: [:or, {:bin_appl, [:>, {:symbol, 'y'}, 1]}]
               ]
             ]
           ]
         ]}
      )
    end

    test "parse expression with domain" do
      text = "f x:Y, a in Y"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [
                   {:symbol, 'f'},
                   [
                     binding: [symbol: 'x', symbol: 'Y'],
                     guard: {:bin_appl, [:in, {:symbol, 'a'}, {:symbol, 'Y'}]}
                   ],
                   nil,
                   nil
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "parse expression with relation in it" do
      text = "f\n---\nx <- y and y > 1"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 {:refinement,
                  [
                    {:symbol, 'x'},
                    nil,
                    {:bin_appl, [:>, {:bin_appl, [:and, {:symbol, 'y'}, {:symbol, 'y'}]}, 1]}
                  ]}
               ]
             ]
           ]
         ]}
      )
    end

    test "parse expression with multiple elements in the pattern" do
      text = "f\n---\nf x <- y"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 {:refinement,
                  [
                    {:f_appl, [symbol: 'f', symbol: 'x']},
                    nil,
                    {:symbol, 'y'}
                  ]}
               ]
             ]
           ]
         ]}
      )
    end

    test "parse guarded refinement" do
      text = "f\n---\nf x \\ x< 0 <- y\nf x<-1"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 {:refinement,
                  [
                    f_appl: [symbol: 'f', symbol: 'x'],
                    bin_appl: [:<, {:symbol, 'x'}, 0],
                    symbol: 'y'
                  ]},
                 {:refinement,
                  [
                    {:f_appl, [symbol: 'f', symbol: 'x']},
                    nil,
                    1
                  ]}
               ]
             ]
           ]
         ]}
      )
    end
  end

  describe "declaration parsing" do
    test "empty heading parsing" do
      text = "f => D"

      tryparse(
        text,
        {:program, [nil, [], [chapter: [[decl: [{:symbol, 'f'}, [], '=>', {:symbol, 'D'}]], []]]]}
      )
    end

    test "dual heading parsing" do
      text = "f=>D\ng=>E"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [{:symbol, 'f'}, [], '=>', {:symbol, 'D'}],
                 decl: [{:symbol, 'g'}, [], '=>', {:symbol, 'E'}]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "basic heading parsing" do
      text = "f a:B, b:C :: D"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [
                   {:symbol, 'f'},
                   [
                     binding: [symbol: 'a', symbol: 'B'],
                     binding: [symbol: 'b', symbol: 'C']
                   ],
                   '::',
                   {:symbol, 'D'}
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "heading with multiple clauses" do
      text = "f x:Y, x != 1,x > 0"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [
                   {:symbol, 'f'},
                   [
                     binding: [symbol: 'x', symbol: 'Y'],
                     guard: {:bin_appl, [:!=, {:symbol, 'x'}, 1]},
                     guard: {:bin_appl, [:>, {:symbol, 'x'}, 0]}
                   ],
                   nil,
                   nil
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "heading with sequenced domain" do
      text = "f x:X :: [X]"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [
                   {:symbol, 'f'},
                   [binding: [symbol: 'x', symbol: 'X']],
                   '::',
                   {:cont, [:list, [symbol: 'X']]}
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "heading with generic domain" do
      text = "f x:_A, x*y>10 :: [_A]"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [
                   {:symbol, 'f'},
                   [
                     binding: [symbol: 'x', symbol: '_A'],
                     guard:
                       {:bin_appl,
                        [
                          :>,
                          {:bin_appl, [:*, {:symbol, 'x'}, {:symbol, 'y'}]},
                          10
                        ]}
                   ],
                   '::',
                   {:cont, [:list, [symbol: '_A']]}
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "heading with lambda" do
      text = "f x:fn z:Nat :: z  :: Bool"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [
                   {:symbol, 'f'},
                   [
                     binding: [
                       symbol: 'x',
                       lambda: [
                         [binding: [symbol: 'z', symbol: 'Nat']],
                         '::',
                         {:symbol, 'z'}
                       ]
                     ]
                   ],
                   '::',
                   {:symbol, 'Bool'}
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "heading with par" do
      text = "f x:X, x in (Y,Z)"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [
                   {:symbol, 'f'},
                   [
                     binding: [symbol: 'x', symbol: 'X'],
                     guard:
                       {:bin_appl,
                        [
                          :in,
                          {:symbol, 'x'},
                          {:cont, [:par, [symbol: 'Y', symbol: 'Z']]}
                        ]}
                   ],
                   nil,
                   nil
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "domain aliasing" do
      text = "Status<={`ok}"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [chapter: [[alias: [[symbol: 'Status'], {:cont, [:set, [literal: 'ok']]}]], []]]
         ]}
      )
    end

    test "multiple domain aliasing" do
      text = "Status,State<={`ok}"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 alias: [
                   [symbol: 'Status', symbol: 'State'],
                   {:cont, [:set, [literal: 'ok']]}
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "comprehension aliasing" do
      text = "Day<={n:Nat,n=<30 \\ n}"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 alias: [
                   [symbol: 'Day'],
                   {:cont,
                    [
                      :set,
                      {:comprehension,
                       [
                         [
                           binding: [symbol: 'n', symbol: 'Nat'],
                           guard: {:bin_appl, [:"=<", {:symbol, 'n'}, 30]}
                         ],
                         {:symbol, 'n'}
                       ]}
                    ]}
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end
  end

  describe "program structure" do
    test "two chapters" do
      text = "f x:Y \n;\ny=>Y"

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [
                   {:symbol, 'f'},
                   [binding: [symbol: 'x', symbol: 'Y']],
                   nil,
                   nil
                 ]
               ],
               []
             ],
             chapter: [
               [decl: [{:symbol, 'y'}, [], '=>', {:symbol, 'Y'}]],
               []
             ]
           ]
         ]}
      )
    end
  end

  describe "comments handling" do
    test "can parse a comment" do
      text = """
      f
      ---
      f>1
      " Here is a comment.
      f<2
      """

      tryparse(
        text,
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [nil, {:bin_appl, [:>, {:symbol, 'f'}, 1]}],
                 comment: 'Here is a comment.',
                 expr: [nil, {:bin_appl, [:<, {:symbol, 'f'}, 2]}]
               ]
             ]
           ]
         ]}
      )
    end
  end

  describe "module handling" do
    test "module declaration" do
      text = "module MOD\nf"

      tryparse(
        text,
        {:program,
         [
           'MOD',
           [],
           [chapter: [[decl: [{:symbol, 'f'}, [], nil, nil]], []]]
         ]}
      )
    end

    test "module import" do
      text = "import MOD\nf"

      tryparse(
        text,
        {:program,
         [
           nil,
           ['MOD'],
           [chapter: [[decl: [{:symbol, 'f'}, [], nil, nil]], []]]
         ]}
      )
    end

    test "module declaration and import" do
      text = "module MOD\nimport MOD2\nf"

      tryparse(
        text,
        {:program,
         [
           'MOD',
           ['MOD2'],
           [chapter: [[decl: [{:symbol, 'f'}, [], nil, nil]], []]]
         ]}
      )
    end

    test "multiple module import" do
      text = "import MOD,MOD2\nf"

      tryparse(
        text,
        {:program,
         [
           nil,
           ['MOD', 'MOD2'],
           [chapter: [[decl: [{:symbol, 'f'}, [], nil, nil]], []]]
         ]}
      )
    end

    test "two import lines" do
      text = "import MOD\nimport MOD2\nf"

      tryparse(
        text,
        {:program,
         [
           nil,
           ['MOD', 'MOD2'],
           [chapter: [[decl: [{:symbol, 'f'}, [], nil, nil]], []]]
         ]}
      )
    end
  end
end
