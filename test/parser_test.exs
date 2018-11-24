defmodule ParserTest do
  use ExUnit.Case

  describe "decl" do
    test "empty" do
      ""
      |> tryp([])
    end

    test "basic decl" do
      "f()"
      |> tryp(chapters: [chapter: [head: [{:decl, decl_ident: {:symbol, 'f'}}]]])
    end

    test "binding regression" do
      """
      f()
      ---

      x

      """
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [[expr: {:symbol, 'x'}]]
          ]
        ]
      )
    end

    test "binding regression 2" do
      """
      f()
      ---
      (x from f)
      """
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [
                expr:
                  {:par,
                   [
                     appl: [
                       op: :from,
                       x: {:symbol, 'x'},
                       y: {:symbol, 'f'}
                     ]
                   ]}
              ]
            ]
          ]
        ]
      )
    end

    test "decl with argument" do
      "f(x : X)"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              {:decl,
               [
                 decl_ident: {:symbol, 'f'},
                 lambda_args: [
                   args: [{:symbol, 'x'}],
                   doms: [{:symbol, 'X'}]
                 ]
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with two argument" do
      "f(x, y : X, Y)"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              {:decl,
               [
                 decl_ident: {:symbol, 'f'},
                 lambda_args: [
                   args: [{:symbol, 'x'}, {:symbol, 'y'}],
                   doms: [{:symbol, 'X'}, {:symbol, 'Y'}]
                 ]
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with yield" do
      "f() :: F"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              {:decl,
               [
                 decl_ident: {:symbol, 'f'},
                 yield_type: '::',
                 lambda_codomain: {:symbol, 'F'}
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with value" do
      "f(x, y : X, Y \\ x) => F"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              {:decl,
               [
                 decl_ident: {:symbol, 'f'},
                 lambda_args: [
                   args: [{:symbol, 'x'}, {:symbol, 'y'}],
                   doms: [{:symbol, 'X'}, {:symbol, 'Y'}]
                 ],
                 lambda_guards: [{:symbol, 'x'}],
                 yield_type: '=>',
                 lambda_codomain: {:symbol, 'F'}
               ]}
            ]
          ]
        ]
      )
    end

    test "full decl" do
      "f(x, y : X, Y \\ x > y) => F"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              {:decl,
               [
                 decl_ident: {:symbol, 'f'},
                 lambda_args: [
                   args: [{:symbol, 'x'}, {:symbol, 'y'}],
                   doms: [{:symbol, 'X'}, {:symbol, 'Y'}]
                 ],
                 lambda_guards: [appl: [op: :>, x: {:symbol, 'x'}, y: {:symbol, 'y'}]],
                 yield_type: '=>',
                 lambda_codomain: {:symbol, 'F'}
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with unary guard" do
      "f(x:X \\ ~x)"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              {:decl,
               [
                 decl_ident: {:symbol, 'f'},
                 lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                 lambda_guards: [
                   appl: [op: :"~", x: {:symbol, 'x'}]
                 ]
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with function application" do
      "f(x:X \\ f x)"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              {:decl,
               [
                 decl_ident: {:symbol, 'f'},
                 lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                 lambda_guards: [
                   appl: [f: {:symbol, 'f'}, x: {:symbol, 'x'}]
                 ]
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with expression application" do
      "f(x:X \\ (x + 1) x)"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                lambda_guards: [
                  appl: [
                    f: {:par, [appl: [op: :+, x: {:symbol, 'x'}, y: 1]]},
                    x: {:symbol, 'x'}
                  ]
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "decl with two guards" do
      "f(x:X \\ x > 1, x < 3)"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              {:decl,
               [
                 decl_ident: {:symbol, 'f'},
                 lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                 lambda_guards: [
                   appl: [op: :>, x: {:symbol, 'x'}, y: 1],
                   appl: [op: :<, x: {:symbol, 'x'}, y: 3]
                 ]
               ]}
            ]
          ]
        ]
      )
    end
  end

  describe "literals" do
    test "delimited literal" do
      """
      f()
      ---
      [`ok]
      """
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [[expr: {:list, [literal: 'ok']}]]
          ]
        ]
      )
    end
  end

  describe "alias" do
    test "domain alias" do
      "S <= String"
      |> tryp(
        chapters: [
          chapter: [
            head: [alias: [alias_name: [{:symbol, 'S'}], alias_expr: {:symbol, 'String'}]]
          ]
        ]
      )
    end

    test "multiple domain alias" do
      "S, T <= String"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              alias: [
                alias_name: [symbol: 'S', symbol: 'T'],
                alias_expr: {:symbol, 'String'}
              ]
            ]
          ]
        ]
      )
    end

    test "alias of exp" do
      "Status <= {`ok}"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              alias: [alias_name: [symbol: 'Status'], alias_expr: {:set, [literal: 'ok']}]
            ]
          ]
        ]
      )
    end

    test 'expr aliasing' do
      "Day <= x =< 30"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              alias: [
                alias_name: [symbol: 'Day'],
                alias_expr: {:appl, [op: :"=<", x: {:symbol, 'x'}, y: 30]}
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "containers" do
    test "empty list" do
      "f(x:X \\ [])"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                lambda_guards: [list: []]
              ]
            ]
          ]
        ]
      )
    end

    test "list" do
      "f(x:X \\ [x])"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                lambda_guards: [list: [{:symbol, 'x'}]]
              ]
            ]
          ]
        ]
      )
    end

    test "list with multiple items" do
      "f(x:X \\ [x, x 1])"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                lambda_guards: [list: [{:symbol, 'x'}, appl: [f: {:symbol, 'x'}, x: 1]]]
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "head" do
    test "two declarations" do
      "f()\ng()"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              decl: [decl_ident: {:symbol, 'f'}],
              decl: [decl_ident: {:symbol, 'g'}]
            ]
          ]
        ]
      )
    end

    test "comment and decl" do
      "\"  ok\nf()"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              comment: 'ok',
              decl: [decl_ident: {:symbol, 'f'}]
            ]
          ]
        ]
      )
    end

    test "blank line" do
      "f()\n\ng()"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              decl: [decl_ident: {:symbol, 'f'}],
              decl: [decl_ident: {:symbol, 'g'}]
            ]
          ]
        ]
      )
    end
  end

  describe "comments" do
    test "comment with no newline" do
      "\"  ok"
      |> tryp(chapters: [chapter: [head: [comment: 'ok']]])
    end

    test "two comments" do
      """
      " ok
      " go
      """
      |> tryp(chapters: [chapter: [head: [comment: 'ok' ++ [63743] ++ 'go']]])
    end

    test "comment in body" do
      """
      f()
      ---
      " A
      " B
      """
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [comment: 'A' ++ [63743] ++ 'B']
          ]
        ]
      )
    end

    test "scratch regression" do
      """
      andr()
      ---
      z " Here
      """
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'andr'}]],
            body: [[expr: {:symbol, 'z'}], {:comment, 'Here'}]
          ]
        ]
      )
    end
  end

  describe "precendence" do
    test "decl with precedence" do
      "f(x: Nat \\ f x > g y)"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'Nat'}]],
                lambda_guards: [
                  appl: [
                    op: :>,
                    x: {:appl, [f: {:symbol, 'f'}, x: {:symbol, 'x'}]},
                    y: {:appl, [f: {:symbol, 'g'}, x: {:symbol, 'y'}]}
                  ]
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "decl with function precedence" do
      "f(x: Nat \\ f x y)"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'Nat'}]],
                lambda_guards: [
                  appl: [f: {:appl, [f: {:symbol, 'f'}, x: {:symbol, 'x'}]}, x: {:symbol, 'y'}]
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "decl with function application on unary" do
      "f(x: Nat \\ f ~ y)"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'Nat'}]],
                lambda_guards: [
                  appl: [f: {:symbol, 'f'}, x: {:appl, [op: :"~", x: {:symbol, 'y'}]}]
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "decl with unary associativity" do
      "f(x:X \\ ~ # z x)"
      |> tryp(
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                lambda_guards: [
                  appl: [
                    f:
                      {:appl,
                       [
                         op: :"~",
                         x:
                           {:appl,
                            [
                              op: :"#",
                              x: {:symbol, 'z'}
                            ]}
                       ]},
                    x: {:symbol, 'x'}
                  ]
                ]
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "chapter" do
    test "simple chapter" do
      "f()\n---\n1"
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [[expr: 1]]
          ]
        ]
      )
    end

    test "two chapters" do
      "f()\n;\ng()"
      |> tryp(
        chapters: [
          chapter: [head: [decl: [decl_ident: {:symbol, 'f'}]]],
          chapter: [head: [decl: [decl_ident: {:symbol, 'g'}]]]
        ]
      )
    end

    test "chapter refinement" do
      "f()\n---\n1 <- 2"
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [[refinement: [pattern: 1, expr: 2]]]
          ]
        ]
      )
    end

    test "chapter binary op" do
      "f()\n---\n1 + 2"
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [[expr: {:appl, [op: :+, x: 1, y: 2]}]]
          ]
        ]
      )
    end

    test "chapter function application" do
      "f()\n---\nf x"
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [
                expr:
                  {:appl,
                   [
                     f: {:symbol, 'f'},
                     x: {:symbol, 'x'}
                   ]}
              ]
            ]
          ]
        ]
      )
    end

    test "chapter list" do
      "f()\n---\n[f x]"
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [[expr: {:list, [appl: [f: {:symbol, 'f'}, x: {:symbol, 'x'}]]}]]
          ]
        ]
      )
    end

    test "chapter with two lines" do
      "f()\n---\n1\n2"
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [[expr: 1], [expr: 2]]
          ]
        ]
      )
    end
  end

  describe "import" do
    test "import line" do
      "import F\nf()"
      |> tryp(
        imports: [import: 'F'],
        chapters: [chapter: [head: [decl: [decl_ident: {:symbol, 'f'}]]]]
      )
    end

    test "multiple imports" do
      "import F, X\nf()"
      |> tryp(
        imports: [import: 'F', import: 'X'],
        chapters: [chapter: [head: [decl: [decl_ident: {:symbol, 'f'}]]]]
      )
    end
  end

  describe "comprehensions" do
    test "set comprehension" do
      "f()\n---\n{x : X \\ x}"
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [
                expr:
                  {:set,
                   [
                     comprehension: [
                       bindings: [
                         binding: [
                           bind_symbol: {:symbol, 'x'},
                           bind_domain: {:symbol, 'X'}
                         ]
                       ],
                       expr: {:symbol, 'x'}
                     ]
                   ]}
              ]
            ]
          ]
        ]
      )
    end

    test "comprehension with guard" do
      "f()\n---\n{x : X, x > 1 \\ x}"
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [
                expr:
                  {:set,
                   [
                     comprehension: [
                       bindings: [
                         binding: [
                           bind_symbol: {:symbol, 'x'},
                           bind_domain: {:symbol, 'X'}
                         ],
                         guard: {:appl, [op: :>, x: {:symbol, 'x'}, y: 1]}
                       ],
                       expr: {:symbol, 'x'}
                     ]
                   ]}
              ]
            ]
          ]
        ]
      )
    end

    test "comprehension with from" do
      "f()\n---\n[x : X \\ x]"
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [
                expr:
                  {:list,
                   [
                     comprehension: [
                       bindings: [
                         binding: [
                           bind_symbol: {:symbol, 'x'},
                           bind_domain: {:symbol, 'X'}
                         ]
                       ],
                       expr: {:symbol, 'x'}
                     ]
                   ]}
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "quantification" do
    test "existential quantification" do
      "f()\n---\nexists x : X \\ x > 1"
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [
                expr:
                  {:quantification,
                   [
                     quantifier: :exists,
                     bindings: [
                       binding: [
                         bind_symbol: {:symbol, 'x'},
                         bind_domain: {:symbol, 'X'}
                       ]
                     ],
                     expr: {:appl, [op: :>, x: {:symbol, 'x'}, y: 1]}
                   ]}
              ]
            ]
          ]
        ]
      )
    end

    test "nested quantification" do
      "f()\n---\nexists x : X \\ all y : X \\ x > y"
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [
                expr:
                  {:quantification,
                   [
                     quantifier: :exists,
                     bindings: [
                       binding: [
                         bind_symbol: {:symbol, 'x'},
                         bind_domain: {:symbol, 'X'}
                       ]
                     ],
                     expr:
                       {:quantification,
                        [
                          quantifier: :all,
                          bindings: [
                            binding: [
                              bind_symbol: {:symbol, 'y'},
                              bind_domain: {:symbol, 'X'}
                            ]
                          ],
                          expr: {:appl, [op: :>, x: {:symbol, 'x'}, y: {:symbol, 'y'}]}
                        ]}
                   ]}
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "objects" do
    test "object syntax" do
      """
      f()
      ---
      f.y
      """
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [[expr: {:dot, [f: {:symbol, 'y'}, x: {:symbol, 'f'}]}]]
          ]
        ]
      )
    end
  end

  describe "lambdas" do
    test "lambda parsing" do
      """
      f()
      ---
      fn ()::D
      """
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [
                expr:
                  {:lambda,
                   [
                     yield_type: '::',
                     lambda_codomain: {:symbol, 'D'}
                   ]}
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "sort" do
    test "sort" do
      """
      sort(xs : [X]) :: [X]
      """
      |> tryp(
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'sort'},
                lambda_args: [
                  args: [symbol: 'xs'],
                  doms: [list: [symbol: 'X']]
                ],
                yield_type: '::',
                lambda_codomain: {:list, [symbol: 'X']}
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "logical operators" do
    test "and" do
      """
      f()
      ---
      x and y
      """
      |> tryp(
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [[expr: {:appl, [op: :and, x: {:symbol, 'x'}, y: {:symbol, 'y'}]}]]
          ]
        ]
      )
    end
  end

  describe "module line" do
    test "module" do
      """

      module TEST
      f()
      """
      |> tryp(
        module: 'TEST',
        chapters: [chapter: [head: [decl: [decl_ident: {:symbol, 'f'}]]]]
      )
    end
  end

  defp tryp(string, expected) do
    string = string

    {:ok, tokens, _} =
      Pantagruel.Scan.scan(string)
      |> :lexer.string()

    {:ok, parsed} = :parser.parse(tokens)
    assert expected == parsed
  end
end
