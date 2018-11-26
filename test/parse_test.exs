defmodule Pantagruel.Test.LegacyParser do
  use ExUnit.Case

  defp tryparse(text, r) do
    {:ok, program} =
      Pantagruel.Scan.scan(text)
      |> :lexer.string()
      |> Pantagruel.Parse.handle_lex()

    assert r == program
  end

  describe "expression parsing" do
    test "parse two expressions" do
      text = "f()\n---\nx != y\ny > 1"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'}
              ]
            ],
            body: [
              [expr: {:appl, [op: :!=, x: {:symbol, 'x'}, y: {:symbol, 'y'}]}],
              [expr: {:appl, [op: :>, x: {:symbol, 'y'}, y: 1]}]
            ]
          ]
        ]
      )
    end

    test "parse two expressions with connecting op" do
      text = """
      f()
      ---
      x != y
      or y > 1
      """

      tryparse(text,
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [expr: {:appl, [op: :!=, x: {:symbol, 'x'}, y: {:symbol, 'y'}]}],
              [
                intro_op: :or,
                expr: {:appl, [op: :>, x: {:symbol, 'y'}, y: 1]}
              ]
            ]
          ]
        ]
      )
    end

    test "parse expression with domain" do
      text = "f(x:Y \\ a in Y)"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [
                  args: [{:symbol, 'x'}],
                  doms: [{:symbol, 'Y'}]
                ],
                lambda_guards: [{:appl, [op: :in, x: {:symbol, 'a'}, y: {:symbol, 'Y'}]}]
              ]
            ]
          ]
        ]
      )
    end

    test "parse expression with relation in it" do
      text = "f()\n---\nx <- y and y > 1"

      tryparse(text,
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [
                refinement: [
                  pattern: {:symbol, 'x'},
                  expr:
                    {:appl,
                     [op: :>, x: {:appl, [op: :and, x: {:symbol, 'y'}, y: {:symbol, 'y'}]}, y: 1]}
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "parse expression with multiple elements in the pattern" do
      text = "f()\n---\nf x <- y"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'}
              ]
            ],
            body: [
              [
                refinement: [
                  pattern: {:appl, [f: {:symbol, 'f'}, x: {:symbol, 'x'}]},
                  expr: {:symbol, 'y'}
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "parse guarded refinement" do
      text = "f()\n---\nf x \\ x< 0 <- y\nf x<-1"

      tryparse(text,
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [
                refinement: [
                  pattern: {:appl, [f: {:symbol, 'f'}, x: {:symbol, 'x'}]},
                  guard: [{:appl, [op: :<, x: {:symbol, 'x'}, y: 0]}],
                  expr: {:symbol, 'y'}
                ]
              ],
              [refinement: [pattern: {:appl, [f: {:symbol, 'f'}, x: {:symbol, 'x'}]}, expr: 1]]
            ]
          ]
        ]
      )
    end
  end

  describe "declaration parsing" do
    test "empty heading parsing" do
      text = "f() => D"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                yield_type: '=>',
                lambda_codomain: {:symbol, 'D'}
              ]
            ]
          ]
        ]
      )
    end

    test "dual heading parsing" do
      text = "f()=>D\ng()=>E"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                yield_type: '=>',
                lambda_codomain: {:symbol, 'D'}
              ],
              decl: [
                decl_ident: {:symbol, 'g'},
                yield_type: '=>',
                lambda_codomain: {:symbol, 'E'}
              ]
            ]
          ]
        ]
      )
    end

    test "basic heading parsing" do
      text = "f(a,b:B,C) :: D"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [
                  args: [{:symbol, 'a'}, {:symbol, 'b'}],
                  doms: [{:symbol, 'B'}, {:symbol, 'C'}]
                ],
                yield_type: '::',
                lambda_codomain: {:symbol, 'D'}
              ]
            ]
          ]
        ]
      )
    end

    test "heading with multiple clauses" do
      text = "f(x:Y \\ x != 1,x > 0)"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [
                  args: [{:symbol, 'x'}],
                  doms: [{:symbol, 'Y'}]
                ],
                lambda_guards: [
                  {:appl, [op: :!=, x: {:symbol, 'x'}, y: 1]},
                  {:appl, [op: :>, x: {:symbol, 'x'}, y: 0]}
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "heading with sequenced domain" do
      text = "f(x:X) :: [X]"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [
                  args: [{:symbol, 'x'}],
                  doms: [symbol: 'X']
                ],
                yield_type: '::',
                lambda_codomain: {:list, [symbol: 'X']}
              ]
            ]
          ]
        ]
      )
    end

    test "heading with generic domain" do
      text = "f(x:_A \\ x*y>10) :: [_A]"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [symbol: 'x'], doms: [symbol: '_A']],
                lambda_guards: [
                  appl: [
                    op: :>,
                    x: {:appl, [op: :*, x: {:symbol, 'x'}, y: {:symbol, 'y'}]},
                    y: 10
                  ]
                ],
                yield_type: '::',
                lambda_codomain: {:list, [symbol: '_A']}
              ]
            ]
          ]
        ]
      )
    end

    test "heading with lambda" do
      text = "f(x:fn(z:Nat) :: z) :: Bool"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [
                  args: [symbol: 'x'],
                  doms: [
                    lambda: [
                      lambda_args: [args: [symbol: 'z'], doms: [symbol: 'Nat']],
                      yield_type: '::',
                      lambda_codomain: {:symbol, 'z'}
                    ]
                  ]
                ],
                yield_type: '::',
                lambda_codomain: {:symbol, 'Bool'}
              ]
            ]
          ]
        ]
      )
    end

    test "heading with par" do
      text = "f(x:X \\ x in (Y,Z))"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [
                  args: [{:symbol, 'x'}],
                  doms: [symbol: 'X']
                ],
                lambda_guards: [
                  appl: [op: :in, x: {:symbol, 'x'}, y: {:par, [symbol: 'Y', symbol: 'Z']}]
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "domain aliasing" do
      text = "Status<={`ok}"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              alias: [
                alias_name: [{:symbol, 'Status'}],
                alias_expr: {:set, [literal: 'ok']}
              ]
            ]
          ]
        ]
      )
    end

    test "multiple domain aliasing" do
      text = "Status,State<={`ok}"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              alias: [
                alias_name: [{:symbol, 'Status'}, {:symbol, 'State'}],
                alias_expr: {:set, [literal: 'ok']}
              ]
            ]
          ]
        ]
      )
    end

    test "comprehension aliasing" do
      text = "Day<={n:Nat,n=<30 \\ n}"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              alias: [
                alias_name: [{:symbol, 'Day'}],
                alias_expr:
                  {:set,
                   [
                     comprehension: [
                       bindings: [
                         binding: [
                           bind_symbol: {:symbol, 'n'},
                           bind_domain: {:symbol, 'Nat'}
                         ],
                         guard:
                           {:appl,
                            [
                              op: :"=<",
                              x: {:symbol, 'n'},
                              y: 30
                            ]}
                       ],
                       expr: {:symbol, 'n'}
                     ]
                   ]}
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "program structure" do
    test "two chapters" do
      text = "f(x:Y)\n;\ny()=>Y"

      tryparse(text,
        chapters: [
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [
                  args: [{:symbol, 'x'}],
                  doms: [{:symbol, 'Y'}]
                ]
              ]
            ]
          ],
          chapter: [
            head: [
              decl: [
                decl_ident: {:symbol, 'y'},
                yield_type: '=>',
                lambda_codomain: {:symbol, 'Y'}
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "comments handling" do
    test "can parse a comment" do
      text = """
      f()
      ---
      f>1
      " Here is a comment.
      f<2
      """

      tryparse(text,
        chapters: [
          chapter: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              [expr: {:appl, [op: :>, x: {:symbol, 'f'}, y: 1]}],
              {:comment, 'Here is a comment.'},
              [expr: {:appl, [op: :<, x: {:symbol, 'f'}, y: 2]}]
            ]
          ]
        ]
      )
    end
  end

  describe "module handling" do
    test "module declaration" do
      text = "module MOD\nf()"

      tryparse(text, [
        {:module, 'MOD'},
        chapters: [chapter: [head: [decl: [decl_ident: {:symbol, 'f'}]]]]
      ])
    end

    test "module import" do
      text = "import MOD\nf()"

      tryparse(text,
        imports: [import: 'MOD'],
        chapters: [chapter: [head: [decl: [decl_ident: {:symbol, 'f'}]]]]
      )
    end

    test "module declaration and import" do
      text = "module MOD\nimport MOD2\nf()"

      tryparse(text,
        module: 'MOD',
        imports: [import: 'MOD2'],
        chapters: [chapter: [head: [decl: [decl_ident: {:symbol, 'f'}]]]]
      )
    end

    test "multiple module import" do
      text = "import MOD,MOD2\nf()"

      tryparse(text,
        imports: [import: 'MOD', import: 'MOD2'],
        chapters: [chapter: [head: [decl: [decl_ident: {:symbol, 'f'}]]]]
      )
    end

    test "two import lines" do
      text = "import MOD\nimport MOD2\nf()"

      tryparse(text,
        imports: [import: 'MOD', import: 'MOD2'],
        chapters: [chapter: [head: [decl: [decl_ident: {:symbol, 'f'}]]]]
      )
    end
  end
end
