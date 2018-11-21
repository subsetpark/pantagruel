defmodule ParserTest do
  use ExUnit.Case

  describe "decl" do
    test "basic decl" do
      'f()'
      |> tryp(sections: [section: [head: [{:decl, decl_ident: 'f'}]]])
    end

    test "decl with argument" do
      'f(x : X)'
      |> tryp(
        sections: [
          section: [
            head: [
              {:decl,
               [
                 decl_ident: 'f',
                 decl_args: [
                   args: ['x'],
                   doms: ['X']
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
                 decl_ident: 'f',
                 decl_args: [
                   args: ['x', 'y'],
                   doms: ['X', 'Y']
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
                 decl_ident: 'f',
                 yield_type: '::',
                 lambda_codomain: 'F'
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with value" do
      'f(x, y : X, Y . x) => F'
      |> tryp(
        sections: [
          section: [
            head: [
              {:decl,
               [
                 decl_ident: 'f',
                 decl_args: [args: ['x', 'y'], doms: ['X', 'Y']],
                 decl_guards: ['x'],
                 yield_type: '=>',
                 lambda_codomain: 'F'
               ]}
            ]
          ]
        ]
      )
    end

    test "full decl" do
      'f(x, y : X, Y . x > y) => F'
      |> tryp(
        sections: [
          section: [
            head: [
              {:decl,
               [
                 decl_ident: 'f',
                 decl_args: [args: ['x', 'y'], doms: ['X', 'Y']],
                 decl_guards: [appl: [op: '>', x: 'x', y: 'y']],
                 yield_type: '=>',
                 lambda_codomain: 'F'
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with unary guard" do
      'f(x:X . ~x)'
      |> tryp(
        sections: [
          section: [
            head: [
              {:decl,
               [
                 decl_ident: 'f',
                 decl_args: [args: ['x'], doms: ['X']],
                 decl_guards: [
                   appl: [op: '~', x: 'x']
                 ]
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with function application" do
      'f(x:X . f x)'
      |> tryp(
        sections: [
          section: [
            head: [
              {:decl,
               [
                 decl_ident: 'f',
                 decl_args: [args: ['x'], doms: ['X']],
                 decl_guards: [
                   appl: [f: 'f', x: 'x']
                 ]
               ]}
            ]
          ]
        ]
      )
    end

    test "decl with expression application" do
      'f(x:X . (x + 1) x)'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [args: ['x'], doms: ['X']],
                decl_guards: [
                  appl: [
                    f: {:par, [appl: [op: '+', x: 'x', y: 1]]},
                    x: 'x'
                  ]
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "decl with two guards" do
      'f(x:X . x > 1, x < 3)'
      |> tryp(
        sections: [
          section: [
            head: [
              {:decl,
               [
                 decl_ident: 'f',
                 decl_args: [args: ['x'], doms: ['X']],
                 decl_guards: [
                   appl: [op: '>', x: 'x', y: 1],
                   appl: [op: '<', x: 'x', y: 3]
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
          section: [head: [alias: [alias_name: ['S'], alias_expr: 'String']]]
        ]
      )
    end

    test "multiple domain alias" do
      'S, T <= String'
      |> tryp(
        sections: [
          section: [
            head: [
              alias: [alias_name: ['S', 'T'], alias_expr: 'String']
            ]
          ]
        ]
      )
    end
  end

  describe "containers" do
    test "empty list" do
      'f(x:X . [])'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [args: ['x'], doms: ['X']],
                decl_guards: [list: []]
              ]
            ]
          ]
        ]
      )
    end

    test "list" do
      'f(x:X . [x])'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [args: ['x'], doms: ['X']],
                decl_guards: [list: ['x']]
              ]
            ]
          ]
        ]
      )
    end

    test "list with multiple items" do
      'f(x:X . [x, x 1])'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [args: ['x'], doms: ['X']],
                decl_guards: [list: ['x', appl: [f: 'x', x: 1]]]
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
              decl: [decl_ident: 'f'],
              decl: [decl_ident: 'g']
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
              decl: [decl_ident: 'f']
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
              decl: [decl_ident: 'f'],
              decl: [decl_ident: 'g']
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
      'f(x: Nat . f x > g y)'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [args: ['x'], doms: ['Nat']],
                decl_guards: [
                  appl: [
                    op: '>',
                    x: {:appl, [f: 'f', x: 'x']},
                    y: {:appl, [f: 'g', x: 'y']}
                  ]
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "decl with function precedence" do
      'f(x: Nat . f x y)'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [args: ['x'], doms: ['Nat']],
                decl_guards: [appl: [f: {:appl, [f: 'f', x: 'x']}, x: 'y']]
              ]
            ]
          ]
        ]
      )
    end

    test "decl with function application on unary" do
      'f(x: Nat . f ~ y)'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [args: ['x'], doms: ['Nat']],
                decl_guards: [appl: [f: 'f', x: {:appl, [op: '~', x: 'y']}]]
              ]
            ]
          ]
        ]
      )
    end

    test "decl with unary associativity" do
      'f(x:X . ~ # z x)'
      |> tryp(
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [args: ['x'], doms: ['X']],
                decl_guards: [
                  appl: [
                    f:
                      {:appl,
                       [
                         op: '~',
                         x:
                           {:appl,
                            [
                              op: '#',
                              x: 'z'
                            ]}
                       ]},
                    x: 'x'
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
            head: [decl: [decl_ident: 'f']],
            body: [expr: 1]
          ]
        ]
      )
    end

    test "two sections" do
      'f()\n;\ng()'
      |> tryp(
        sections: [
          section: [head: [decl: [decl_ident: 'f']]],
          section: [head: [decl: [decl_ident: 'g']]]
        ]
      )
    end

    test "section refinement" do
      'f()\n---\n1 <- 2'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: 'f']],
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
            head: [decl: [decl_ident: 'f']],
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
            head: [decl: [decl_ident: 'f']],
            body: [expr: {:appl, [f: 'f', x: 'x']}]
          ]
        ]
      )
    end

    test "section list" do
      'f()\n---\n[f x]'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: 'f']],
            body: [expr: {:list, [appl: [f: 'f', x: 'x']]}]
          ]
        ]
      )
    end

    test "section with two lines" do
      'f()\n---\n1\n2'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: 'f']],
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
        sections: [section: [head: [decl: [decl_ident: 'f']]]]
      )
    end

    test "multiple imports" do
      'import F, X\nf()'
      |> tryp(
        imports: [import: ['F', 'X']],
        sections: [section: [head: [decl: [decl_ident: 'f']]]]
      )
    end
  end

  describe "comprehensions" do
    test "set comprehension" do
      'f()\n---\n{x : X . x}'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: 'f']],
            body: [
              expr:
                {:set,
                 {:comprehension,
                  [
                    bindings: [
                      binding: [
                        bind_symbol: 'x',
                        bind_op: :":",
                        bind_domain: 'X'
                      ]
                    ],
                    expr: 'x'
                  ]}}
            ]
          ]
        ]
      )
    end

    test "comprehension with guard" do
      'f()\n---\n{x : X, x > 1 . x}'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: 'f']],
            body: [
              expr:
                {:set,
                 {:comprehension,
                  [
                    bindings: [
                      binding: [
                        bind_symbol: 'x',
                        bind_op: :":",
                        bind_domain: 'X'
                      ],
                      guard: {:appl, [op: '>', x: 'x', y: 1]}
                    ],
                    expr: 'x'
                  ]}}
            ]
          ]
        ]
      )
    end

    test "comprehension with from" do
      'f()\n---\n[x from X . x]'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: 'f']],
            body: [
              expr:
                {:list,
                 {:comprehension,
                  [
                    bindings: [
                      binding: [
                        bind_symbol: 'x',
                        bind_op: :from,
                        bind_domain: 'X'
                      ]
                    ],
                    expr: 'x'
                  ]}}
            ]
          ]
        ]
      )
    end
  end

  describe "quantification" do
    test "existential quantification" do
      'f()\n---\nexists x : X . x > 1'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: 'f']],
            body: [
              expr:
                {:quantification,
                 [
                   quantifier: :exists,
                   bindings: [
                     binding: [
                       bind_symbol: 'x',
                       bind_op: :":",
                       bind_domain: 'X'
                     ]
                   ],
                   expr: {:appl, [op: '>', x: 'x', y: 1]}
                 ]}
            ]
          ]
        ]
      )
    end

    test "nested quantification" do
      'f()\n---\nexists x : X . all y from X . x > y'
      |> tryp(
        sections: [
          section: [
            head: [decl: [decl_ident: 'f']],
            body: [
              expr:
                {:quantification,
                 [
                   quantifier: :exists,
                   bindings: [
                     binding: [
                       bind_symbol: 'x',
                       bind_op: :":",
                       bind_domain: 'X'
                     ]
                   ],
                   expr:
                     {:quantification,
                      [
                        quantifier: :all,
                        bindings: [
                          binding: [
                            bind_symbol: 'y',
                            bind_op: :from,
                            bind_domain: 'X'
                          ]
                        ],
                        expr: {:appl, [op: '>', x: 'x', y: 'y']}
                      ]}
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
