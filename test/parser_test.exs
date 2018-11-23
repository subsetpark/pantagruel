defmodule ParserTest do
  use ExUnit.Case

  describe "decl" do
    test "basic decl" do
      'f()'
      |> tryp(sections: [section: [head: [{:decl, decl_ident: {:symbol, 'f'}}]]])
    end

    test "decl with argument" do
      'f(x : X)'
      |> tryp(
        sections: [
          section: [
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
      'f(x, y : X, Y)'
      |> tryp(
        sections: [
          section: [
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
      'f() :: F'
      |> tryp(
        sections: [
          section: [
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
      'f(x, y : X, Y \\ x) => F'
      |> tryp(
        sections: [
          section: [
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
      'f(x, y : X, Y \\ x > y) => F'
      |> tryp(
        sections: [
          section: [
            head: [
              {:decl,
               [
                 decl_ident: {:symbol, 'f'},
                 lambda_args: [
                   args: [{:symbol, 'x'}, {:symbol, 'y'}],
                   doms: [{:symbol, 'X'}, {:symbol, 'Y'}]
                 ],
                 lambda_guards: [appl: [op: '>', x: {:symbol, 'x'}, y: {:symbol, 'y'}]],
                 yield_type: '=>',
                 lambda_codomain: {:symbol, 'F'}
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with unary guard" do
      'f(x:X \\ ~x)'
      |> tryp(
        sections: [
          section: [
            head: [
              {:decl,
               [
                 decl_ident: {:symbol, 'f'},
                 lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                 lambda_guards: [
                   appl: [op: '~', x: {:symbol, 'x'}]
                 ]
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with function application" do
      'f(x:X \\ f x)'
      |> tryp(
        sections: [
          section: [
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
      'f(x:X \\ (x + 1) x)'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                lambda_guards: [
                  appl: [
                    f: {:par, [appl: [op: '+', x: {:symbol, 'x'}, y: 1]]},
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
      'f(x:X \\ x > 1, x < 3)'
      |> tryp(
        sections: [
          section: [
            head: [
              {:decl,
               [
                 decl_ident: {:symbol, 'f'},
                 lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                 lambda_guards: [
                   appl: [op: '>', x: {:symbol, 'x'}, y: 1],
                   appl: [op: '<', x: {:symbol, 'x'}, y: 3]
                 ]
               ]}
            ]
          ]
        ]
      )
    end
  end

  describe "alias" do
    test "domain alias" do
      'S <= String'
      |> tryp(
        sections: [
          section: [
            head: [alias: [alias_name: [{:symbol, 'S'}], alias_expr: {:symbol, 'String'}]]
          ]
        ]
      )
    end

    test "multiple domain alias" do
      'S, T <= String'
      |> tryp(
        sections: [
          section: [
            head: [
              alias: [
                alias_name: [{:symbol, 'S'}, {:symbol, 'T'}],
                alias_expr: {:symbol, 'String'}
              ]
            ]
          ]
        ]
      )
    end

    test "alias of exp" do
      'Status <= {`ok}'
      |> tryp(
        sections: [
          section: [
            head: [
              alias: [alias_name: [symbol: 'Status'], alias_expr: {:set, ['ok']}]
            ]
          ]
        ]
      )
    end

    test 'expr aliasing' do
      'Day <= x =< 30'
      |> tryp(
        sections: [
          section: [
            head: [
              alias: [
                alias_name: [symbol: 'Day'],
                alias_expr: {:appl, [op: '=<', x: {:symbol, 'x'}, y: 30]}
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "containers" do
    test "empty list" do
      'f(x:X \\ [])'
      |> tryp(
        sections: [
          section: [
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
      'f(x:X \\ [x])'
      |> tryp(
        sections: [
          section: [
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
      'f(x:X \\ [x, x 1])'
      |> tryp(
        sections: [
          section: [
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
      'f()\ng()'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [decl_ident: {:symbol, 'f'}],
              decl: [decl_ident: {:symbol, 'g'}]
            ]
          ]
        ]
      )
    end

    test "comment and decl" do
      '"  ok\nf()'
      |> tryp(
        sections: [
          section: [
            head: [
              comment: 'ok',
              decl: [decl_ident: {:symbol, 'f'}]
            ]
          ]
        ]
      )
    end

    test "blank line" do
      'f()\n\ng()'
      |> tryp(
        sections: [
          section: [
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
      '"  ok'
      |> tryp(sections: [section: [head: [comment: 'ok']]])
    end
  end

  describe "precendence" do
    test "decl with precedence" do
      'f(x: Nat \\ f x > g y)'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'Nat'}]],
                lambda_guards: [
                  appl: [
                    op: '>',
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
      'f(x: Nat \\ f x y)'
      |> tryp(
        sections: [
          section: [
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
      'f(x: Nat \\ f ~ y)'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'Nat'}]],
                lambda_guards: [appl: [f: {:symbol, 'f'}, x: {:appl, [op: '~', x: {:symbol, 'y'}]}]]
              ]
            ]
          ]
        ]
      )
    end

    test "decl with unary associativity" do
      'f(x:X \\ ~ # z x)'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: {:symbol, 'f'},
                lambda_args: [args: [{:symbol, 'x'}], doms: [{:symbol, 'X'}]],
                lambda_guards: [
                  appl: [
                    f:
                      {:appl,
                       [
                         op: '~',
                         x:
                           {:appl,
                            [
                              op: '#',
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

  describe "section" do
    test "simple section" do
      'f()\n---\n1'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [expr: 1]
          ]
        ]
      )
    end

    test "two sections" do
      'f()\n;\ng()'
      |> tryp(
        sections: [
          section: [head: [decl: [decl_ident: {:symbol, 'f'}]]],
          section: [head: [decl: [decl_ident: {:symbol, 'g'}]]]
        ]
      )
    end

    test "section refinement" do
      'f()\n---\n1 <- 2'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [refinement: [pattern: 1, expr: 2]]
          ]
        ]
      )
    end

    test "section binary op" do
      'f()\n---\n1 + 2'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [expr: {:appl, [op: '+', x: 1, y: 2]}]
          ]
        ]
      )
    end

    test "section function application" do
      'f()\n---\nf x'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [expr: {:appl, [f: {:symbol, 'f'}, x: {:symbol, 'x'}]}]
          ]
        ]
      )
    end

    test "section list" do
      'f()\n---\n[f x]'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [expr: {:list, [appl: [f: {:symbol, 'f'}, x: {:symbol, 'x'}]]}]
          ]
        ]
      )
    end

    test "section with two lines" do
      'f()\n---\n1\n2'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [expr: 1, expr: 2]
          ]
        ]
      )
    end
  end

  describe "import" do
    test "import line" do
      'import F\nf()'
      |> tryp(
        imports: [import: ['F']],
        sections: [section: [head: [decl: [decl_ident: {:symbol, 'f'}]]]]
      )
    end

    test "multiple imports" do
      'import F, X\nf()'
      |> tryp(
        imports: [import: ['F', 'X']],
        sections: [section: [head: [decl: [decl_ident: {:symbol, 'f'}]]]]
      )
    end
  end

  describe "comprehensions" do
    test "set comprehension" do
      'f()\n---\n{x : X \\ x}'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              expr:
                {:set,
                 {:comprehension,
                  [
                    bindings: [
                      binding: [
                        bind_symbol: {:symbol, 'x'},
                        bind_op: :":",
                        bind_domain: {:symbol, 'X'}
                      ]
                    ],
                    expr: {:symbol, 'x'}
                  ]}}
            ]
          ]
        ]
      )
    end

    test "comprehension with guard" do
      'f()\n---\n{x : X, x > 1 \\ x}'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              expr:
                {:set,
                 {:comprehension,
                  [
                    bindings: [
                      binding: [
                        bind_symbol: {:symbol, 'x'},
                        bind_op: :":",
                        bind_domain: {:symbol, 'X'}
                      ],
                      guard: {:appl, [op: '>', x: {:symbol, 'x'}, y: 1]}
                    ],
                    expr: {:symbol, 'x'}
                  ]}}
            ]
          ]
        ]
      )
    end

    test "comprehension with from" do
      'f()\n---\n[x from X \\ x]'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              expr:
                {:list,
                 {:comprehension,
                  [
                    bindings: [
                      binding: [
                        bind_symbol: {:symbol, 'x'},
                        bind_op: :from,
                        bind_domain: {:symbol, 'X'}
                      ]
                    ],
                    expr: {:symbol, 'x'}
                  ]}}
            ]
          ]
        ]
      )
    end
  end

  describe "quantification" do
    test "existential quantification" do
      'f()\n---\nexists x : X \\ x > 1'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              expr:
                {:quantification,
                 [
                   quantifier: :exists,
                   bindings: [
                     binding: [
                       bind_symbol: {:symbol, 'x'},
                       bind_op: :":",
                       bind_domain: {:symbol, 'X'}
                     ]
                   ],
                   expr: {:appl, [op: '>', x: {:symbol, 'x'}, y: 1]}
                 ]}
            ]
          ]
        ]
      )
    end

    test "nested quantification" do
      'f()\n---\nexists x : X \\ all y from X \\ x > y'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              expr:
                {:quantification,
                 [
                   quantifier: :exists,
                   bindings: [
                     binding: [
                       bind_symbol: {:symbol, 'x'},
                       bind_op: :":",
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
                            bind_op: :from,
                            bind_domain: {:symbol, 'X'}
                          ]
                        ],
                        expr: {:appl, [op: '>', x: {:symbol, 'x'}, y: {:symbol, 'y'}]}
                      ]}
                 ]}
            ]
          ]
        ]
      )
    end
  end

  describe "objects" do
    test "object syntax" do
      '''
      f()
      ---
      f.y
      '''
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [expr: {:dot, [f: {:symbol, 'y'}, x: {:symbol, 'f'}]}]
          ]
        ]
      )
    end
  end

  describe "lambdas" do
    test "lambda parsing" do
      '''
      f()
      ---
      fn ()::D
      '''
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: {:symbol, 'f'}]],
            body: [
              expr:
                {:lambda,
                 [
                   yield_type: '::',
                   lambda_codomain: {:symbol, 'D'}
                 ]}
            ]
          ]
        ]
      )
    end
  end

  defp tryp(string, expected) do
    string = string ++ '\n'
    {:ok, tokens, _} = :lexer.string(string)
    {:ok, parsed} = :parser.parse(tokens)
    assert expected == parsed
  end
end
