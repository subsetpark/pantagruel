defmodule ParserTest do
  use ExUnit.Case

  describe "decl" do
    test "empty" do
      ""
      |> tryp({:program, [nil, [], []]})
    end

    test "basic decl" do
      "f."
      |> tryp({:program, [nil, [], [chapter: [[decl: [{:symbol, 'f'}, [], nil, nil]], []]]]})
    end

    test "binding regression" do
      """
      f.
      ---

      x.

      """
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [expr: [nil, {:symbol, 'x'}]]
             ]
           ]
         ]}
      )
    end

    test "binding regression 2" do
      """
      f.
      ---
      (x in f).
      """
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [
                   nil,
                   {:cont,
                    [
                      :par,
                      [bin_appl: [:in, {:symbol, 'x'}, {:symbol, 'f'}]]
                    ]}
                 ]
               ]
             ]
           ]
         ]}
      )
    end

    test "decl with argument" do
      "f x : X."
      |> tryp(
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

    test "decl with two argument" do
      "f x : X, y : Y."
      |> tryp(
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
                     binding: [symbol: 'y', symbol: 'Y']
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

    test "decl with yield" do
      "f -> F."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], '->', {:symbol, 'F'}]],
               []
             ]
           ]
         ]}
      )
    end

    test "decl with value" do
      "f x : X, y : Y, x => F."
      |> tryp(
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
                     binding: [symbol: 'y', symbol: 'Y'],
                     guard: {:symbol, 'x'}
                   ],
                   '=>',
                   {:symbol, 'F'}
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "full decl" do
      "f x : X, y : Y, x > y => F."
      |> tryp(
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
                     binding: [symbol: 'y', symbol: 'Y'],
                     guard: {:bin_appl, [:>, {:symbol, 'x'}, {:symbol, 'y'}]}
                   ],
                   '=>',
                   {:symbol, 'F'}
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "decl with unary guard" do
      "f x:X, ~x."
      |> tryp(
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
                     guard: {:un_appl, [:"~", {:symbol, 'x'}]}
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

    test "decl with function application" do
      "f x:X, f x."
      |> tryp(
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
                     guard: {:f_appl, [symbol: 'f', symbol: 'x']}
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

    test "decl with expression application" do
      "f x:X, (x + 1) x."
      |> tryp(
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
                       {:f_appl,
                        [
                          cont: [:par, [bin_appl: [:+, {:symbol, 'x'}, 1]]],
                          symbol: 'x'
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

    test "decl with two guards" do
      "f x:X, x > 1, x < 3."
      |> tryp(
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
                     guard: {:bin_appl, [:>, {:symbol, 'x'}, 1]},
                     guard: {:bin_appl, [:<, {:symbol, 'x'}, 3]}
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
  end

  describe "literals" do
    test "delimited literal" do
      """
      f.
      ---
      [`ok].
      """
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [expr: [nil, {:cont, [:sequence, [literal: 'ok']]}]]
             ]
           ]
         ]}
      )
    end
  end

  describe "alias" do
    test "domain alias" do
      "S <= String."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [chapter: [[alias: [[symbol: 'S'], {:symbol, 'String'}]], []]]
         ]}
      )
    end

    test "multiple domain alias" do
      "S, T <= String."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [alias: [[symbol: 'S', symbol: 'T'], {:symbol, 'String'}]],
               []
             ]
           ]
         ]}
      )
    end

    test "alias of exp" do
      "Status <= {`ok}."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 alias: [
                   [symbol: 'Status'],
                   {:cont, [:set, [literal: 'ok']]}
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test 'expr aliasing' do
      "Day <= x =< 30."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 alias: [
                   [symbol: 'Day'],
                   {:bin_appl, [:"=<", {:symbol, 'x'}, 30]}
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end
  end

  describe "containers" do
    test "empty sequence" do
      "f x:X, []."
      |> tryp(
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
                     guard: {:cont, [:sequence, []]}
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

    test "sequence" do
      "f x:X, [x]."
      |> tryp(
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
                     guard: {:cont, [:sequence, [symbol: 'x']]}
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

    test "sequence with multiple items" do
      "f x:X, [x, x 1]."
      |> tryp(
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
                     guard: {:cont, [:sequence, [symbol: 'x', f_appl: [{:symbol, 'x'}, 1]]]}
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
  end

  describe "head" do
    test "two declarations" do
      "f.\ng."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [{:symbol, 'f'}, [], nil, nil],
                 decl: [{:symbol, 'g'}, [], nil, nil]
               ],
               []
             ]
           ]
         ]}
      )
    end

    test "comment and decl" do
      "\"  ok\nf."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [comment: 'ok', decl: [{:symbol, 'f'}, [], nil, nil]],
               []
             ]
           ]
         ]}
      )
    end

    test "blank line" do
      "f.\n\ng."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [{:symbol, 'f'}, [], nil, nil],
                 decl: [{:symbol, 'g'}, [], nil, nil]
               ],
               []
             ]
           ]
         ]}
      )
    end
  end

  describe "comments" do
    test "comment with no newline" do
      "\"  ok"
      |> tryp({:program, [nil, [], [chapter: [[comment: 'ok'], []]]]})
    end

    test "two comments" do
      """
      " ok
      " go
      """
      |> tryp({:program, [nil, [], [chapter: [[comment: [111, 107, 63743, 103, 111]], []]]]})
    end

    test "comment in body" do
      """
      f.
      ---
      " A
      " B
      """
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [comment: [?A, 63743, ?B]]
             ]
           ]
         ]}
      )
    end

    test "scratch regression" do
      """
      andr.
      ---
      z. " Here
      """
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'andr'}, [], nil, nil]],
               [{:expr, [nil, {:symbol, 'z'}]}, {:comment, 'Here'}]
             ]
           ]
         ]}
      )
    end
  end

  describe "precendence" do
    test "decl with precedence" do
      "f x: Nat, f x > g y."
      |> tryp(
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
                     binding: [symbol: 'x', symbol: 'Nat'],
                     guard:
                       {:bin_appl,
                        [
                          :>,
                          {:f_appl, [symbol: 'f', symbol: 'x']},
                          {:f_appl, [symbol: 'g', symbol: 'y']}
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

    test "decl with function precedence" do
      "f x: Nat, f x y."
      |> tryp(
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
                     binding: [symbol: 'x', symbol: 'Nat'],
                     guard: {:f_appl, [f_appl: [symbol: 'f', symbol: 'x'], symbol: 'y']}
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

    test "decl with function application on unary" do
      "f x: Nat, f ~ y."
      |> tryp(
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
                     binding: [symbol: 'x', symbol: 'Nat'],
                     guard:
                       {:f_appl,
                        [
                          symbol: 'f',
                          un_appl: [:"~", {:symbol, 'y'}]
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

    test "decl with unary associativity" do
      "f x:X, ~ # z x."
      |> tryp(
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
                       {:f_appl,
                        [
                          un_appl: [:"~", {:un_appl, [:"#", {:symbol, 'z'}]}],
                          symbol: 'x'
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
  end

  describe "chapter" do
    test "simple chapter" do
      "f.\n---\n1."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [expr: [nil, 1]]
             ]
           ]
         ]}
      )
    end

    test "two chapters" do
      "f.\n;\ng."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [[decl: [{:symbol, 'f'}, [], nil, nil]], []],
             chapter: [[decl: [{:symbol, 'g'}, [], nil, nil]], []]
           ]
         ]}
      )
    end

    test "chapter refinement" do
      "f.\n---\n1 <- 2."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [refinement: [1, [case_exp: [nil, 2]]]]
             ]
           ]
         ]}
      )
    end

    test "chapter binary op" do
      "f.\n---\n1 + 2."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [{:expr, [nil, {:bin_appl, [:+, 1, 2]}]}]
             ]
           ]
         ]}
      )
    end

    test "chapter function application" do
      "f.\n---\nf x."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [{:expr, [nil, {:f_appl, [symbol: 'f', symbol: 'x']}]}]
             ]
           ]
         ]}
      )
    end

    test "chapter sequence" do
      "f.\n---\n[f x]."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [
                   nil,
                   {:cont, [:sequence, [f_appl: [symbol: 'f', symbol: 'x']]]}
                 ]
               ]
             ]
           ]
         ]}
      )
    end

    test "chapter with two lines" do
      "f.\n---\n1.\n2."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [{:expr, [nil, 1]}, {:expr, [nil, 2]}]
             ]
           ]
         ]}
      )
    end
  end

  describe "import" do
    test "import line" do
      "import F.\nf."
      |> tryp(
        {:program,
         [
           nil,
           ['F'],
           [chapter: [[decl: [{:symbol, 'f'}, [], nil, nil]], []]]
         ]}
      )
    end

    test "multiple imports" do
      "import F, X.\nf."
      |> tryp(
        {:program,
         [
           nil,
           ['F', 'X'],
           [chapter: [[decl: [{:symbol, 'f'}, [], nil, nil]], []]]
         ]}
      )
    end
  end

  describe "comprehensions" do
    test "set comprehension" do
      "f.\n---\n{x : X .. x}."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [
                   nil,
                   {:cont,
                    [
                      :set,
                      {:comprehension,
                       [
                         [binding: [symbol: 'x', symbol: 'X']],
                         {:symbol, 'x'}
                       ]}
                    ]}
                 ]
               ]
             ]
           ]
         ]}
      )
    end

    test "comprehension with guard" do
      "f.\n---\n{x : X, x > 1 .. x}."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 {:expr,
                  [
                    nil,
                    {:cont,
                     [
                       :set,
                       {:comprehension,
                        [
                          [
                            binding: [symbol: 'x', symbol: 'X'],
                            guard: {:bin_appl, [:>, {:symbol, 'x'}, 1]}
                          ],
                          {:symbol, 'x'}
                        ]}
                     ]}
                  ]}
               ]
             ]
           ]
         ]}
      )
    end

    test "comprehension with in" do
      "f.\n---\n[x : X .. x]."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [
                   nil,
                   {:cont,
                    [
                      :sequence,
                      {:comprehension,
                       [
                         [binding: [symbol: 'x', symbol: 'X']],
                         {:symbol, 'x'}
                       ]}
                    ]}
                 ]
               ]
             ]
           ]
         ]}
      )
    end
  end

  describe "quantification" do
    test "existential quantification" do
      "f.\n---\nexists x : X .. x > 1."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [
                   nil,
                   {:quantification,
                    [
                      :exists,
                      [binding: [symbol: 'x', symbol: 'X']],
                      {:bin_appl, [:>, {:symbol, 'x'}, 1]}
                    ]}
                 ]
               ]
             ]
           ]
         ]}
      )
    end

    test "let quantification" do
      "f.\n---\nlet x:f .. x."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [
                   nil,
                   {:quantification,
                    [
                      :let,
                      [binding: [symbol: 'x', symbol: 'f']],
                      {:symbol, 'x'}
                    ]}
                 ]
               ]
             ]
           ]
         ]}
      )
    end

    test "nested quantification" do
      "f.\n---\nexists x : X .. all y : X .. x > y."
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [
                   nil,
                   {:quantification,
                    [
                      :exists,
                      [binding: [symbol: 'x', symbol: 'X']],
                      {:quantification,
                       [
                         :all,
                         [binding: [symbol: 'y', symbol: 'X']],
                         {:bin_appl, [:>, {:symbol, 'x'}, {:symbol, 'y'}]}
                       ]}
                    ]}
                 ]
               ]
             ]
           ]
         ]}
      )
    end
  end

  describe "objects" do
    test "object syntax" do
      """
      f.
      ---
      f.y.
      """
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [expr: [nil, {:dot, [symbol: 'y', symbol: 'f']}]]
             ]
           ]
         ]}
      )
    end
  end

  describe "lambdas" do
    test "lambda parsing" do
      """
      f.
      ---
      fn->D.
      """
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [{:expr, [nil, {:lambda, [[], '->', {:symbol, 'D'}]}]}]
             ]
           ]
         ]}
      )
    end
  end

  describe "sort" do
    test "sort" do
      """
      sort xs : [X] -> [X].
      """
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [
                 decl: [
                   {:symbol, 'sort'},
                   [binding: [symbol: 'xs', cont: [:sequence, [symbol: 'X']]]],
                   '->',
                   {:cont, [:sequence, [symbol: 'X']]}
                 ]
               ],
               []
             ]
           ]
         ]}
      )
    end
  end

  describe "logical operators" do
    test "and" do
      """
      f.
      ---
      x and y.
      """
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [
                   nil,
                   {:bin_appl, [:and, {:symbol, 'x'}, {:symbol, 'y'}]}
                 ]
               ]
             ]
           ]
         ]}
      )
    end
  end

  describe "module line" do
    test "module" do
      """

      module TEST.
      f.
      """
      |> tryp(
        {:program,
         [
           'TEST',
           [],
           [chapter: [[decl: [{:symbol, 'f'}, [], nil, nil]], []]]
         ]}
      )
    end
  end

  describe "new refinements" do
    test "base case" do
      """
      f.
      ---
      x <- y.
      """
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 refinement: [
                   {:symbol, 'x'},
                   [{:case_exp, [nil, {:symbol, 'y'}]}]
                 ]
               ]
             ]
           ]
         ]}
      )
    end

    test "guard case" do
      """
      f.
      ---
      x <- 0 .. y.
      """
      |> tryp(
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 refinement: [
                   {:symbol, 'x'},
                   [case_exp: [0, {:symbol, 'y'}]]
                 ]
               ]
             ]
           ]
         ]}
      )
    end

    test "sequence case" do
      text = """
      f.
      ---
      x <- (0..y, 1 .. z).
      """

      text_newlines = """
      f.
      ---
      x <- (
        0 .. y,
        1 .. z
      ).
      """

      expected =
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 refinement: [
                   {:symbol, 'x'},
                   [
                     case_exp: [0, {:symbol, 'y'}],
                     case_exp: [1, {:symbol, 'z'}]
                   ]
                 ]
               ]
             ]
           ]
         ]}

      tryp(text, expected)
      tryp(text_newlines, expected)
    end
  end

  describe "multi lines" do
    test "single expression over multiple lines" do
      text = """
      f.
      ---
      f x >
        2.
      """

      expected =
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [
                   nil,
                   {:bin_appl, [:>, {:f_appl, [symbol: 'f', symbol: 'x']}, 2]}
                 ]
               ]
             ]
           ]
         ]}

      tryp(text, expected)
    end

    test "quantification over multiple lines" do
      text = """
      f.
      ---
      all x:X ..
        x > 1.
      """

      expected =
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 expr: [
                   nil,
                   {:quantification,
                    [
                      :all,
                      [binding: [symbol: 'x', symbol: 'X']],
                      {:bin_appl, [:>, {:symbol, 'x'}, 1]}
                    ]}
                 ]
               ]
             ]
           ]
         ]}

      tryp(text, expected)
    end
  end

  describe "fullstop fib" do
    test "fib regression" do
      text = """
      f.
      ---
      fib x <- (
      f .. 1,
      f .. 1
      ).
      """

      expected =
        {:program,
         [
           nil,
           [],
           [
             chapter: [
               [decl: [{:symbol, 'f'}, [], nil, nil]],
               [
                 refinement: [
                   {:f_appl, [symbol: 'fib', symbol: 'x']},
                   [
                     case_exp: [{:symbol, 'f'}, 1],
                     case_exp: [{:symbol, 'f'}, 1]
                   ]
                 ]
               ]
             ]
           ]
         ]}

      tryp(text, expected)
    end
  end

  defp tryp(string, expected) do
    string = string

    {:ok, tokens, _} =
      Pantagruel.Scan.scan(string)
      |> :pant_lexer.string()

    {:ok, parsed} = :pant_parser.parse(tokens)
    assert expected == parsed
  end
end
