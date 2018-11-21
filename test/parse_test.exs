defmodule Pantagruel.Test.LegacyParser do
  use ExUnit.Case

  defp tryparse(text, r) do
    with {:ok, tokens, _} <- :lexer.string(text ++ '\n'),
         {:ok, program} <- Pantagruel.Parse.program(tokens) do
      assert r == program
    end
  end

  describe "expression parsing" do
    test "parse two expressions" do
      text = 'f()\n---\nx != y\ny > 1'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f'
              ]
            ],
            body: [
              expr: {:appl, [op: '!=', x: 'x', y: 'y']},
              expr: {:appl, [op: '>', x: 'y', y: 1]}
            ]
          ]
        ]
      )
    end

    test "parse two expressions with connecting op" do
      text = 'f()\n---\nx != y\n or y > 1'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f'
              ]
            ],
            body: [
              expr: {:appl, [op: '!=', x: 'x', y: 'y']},
              expr: [intro_op: :or, appl: [op: '>', x: 'y', y: 1]]
            ]
          ]
        ]
      )
    end

    test "parse expression with domain" do
      text = 'f(x:Y . a:Y)'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [
                  args: ['x'],
                  doms: ['Y']
                ],
                decl_guards: [{:appl, [op: :":", x: 'a', y: 'Y']}]
              ]
            ]
          ]
        ]
      )
    end

    test "parse expression with relation in it" do
      text = 'f()\n---\nx <- y and y > 1'

      tryparse(text,
        sections: [
          section: [
            head: [decl: [decl_ident: 'f']],
            body: [
              expr: [
                refinement: [
                  pattern: 'x',
                  expr: {:appl, [op: '>', x: {:appl, [op: :and, x: 'y', y: 'y']}, y: 1]}
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "parse expression with multiple elements in the pattern" do
      text = 'f()\n---\nf x <- y'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f'
              ]
            ],
            body: [
              refinement: [
                pattern: {:appl, [f: 'f', x: 'x']},
                expr: 'y'
              ]
            ]
          ]
        ]
      )
    end

    test "parse guarded refinement" do
      text = 'f()\n---\nf x . x< 0 <- y\nf x<-1'

      tryparse(text,
        sections: [
          section: [
            head: [decl: [decl_ident: 'f']],
            body: [
              refinement: [
                pattern: {:appl, [f: 'f', x: 'x']},
                guard: [{:appl, [op: '<', x: 'x', y: 0]}],
                expr: 'y'
              ],
              refinement: [pattern: {:appl, [f: 'f', x: 'x']}, expr: 1]
            ]
          ]
        ]
      )
    end
  end

  describe "declaration parsing" do
    test "empty heading parsing" do
      text = 'f() => D'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                yield_type: '=>',
                lambda_codomain: 'D'
              ]
            ]
          ]
        ]
      )
    end

    test "dual heading parsing" do
      text = 'f()=>D\ng()=>E'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                yield_type: '=>',
                lambda_codomain: 'D'
              ],
              decl: [
                decl_ident: 'g',
                yield_type: '=>',
                lambda_codomain: 'E'
              ]
            ]
          ]
        ]
      )
    end

    test "basic heading parsing" do
      text = 'f(a,b:B,C) :: D'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [
                  args: ['a', 'b'],
                  doms: ['B', 'C']
                ],
                yield_type: '::',
                lambda_codomain: 'D'
              ]
            ]
          ]
        ]
      )
    end

    test "heading with multiple clauses" do
      text = 'f(x:Y.x != 1,x > 0)'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [
                  args: ['x'],
                  doms: ['Y']
                ],
                decl_guards: [
                  {:appl, [op: '!=', x: 'x', y: 1]},
                  {:appl, [op: '>', x: 'x', y: 0]}
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "heading with sequenced domain" do
      text = 'f(x:X) :: [X]'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [
                  args: ['x'],
                  doms: ['X']
                ],
                yield_type: '::',
                lambda_codomain: {:list, ['X']}
              ]
            ]
          ]
        ]
      )
    end

    test "heading with generic domain" do
      text = 'f(x:_A.x*y>10) :: [_A]'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [
                  args: ['x'],
                  doms: ["_A"]
                ],
                decl_guards: [
                  appl: [op: '>', x: {:appl, [op: :times, x: 'x', y: 'y']}, y: 10]
                ],
                yield_type: '::',
                lambda_codomain: {:list, ["_A"]}
              ]
            ]
          ]
        ]
      )
    end

    test "heading with lambda" do
      text = 'f(x:fn(z:Nat) :: z) :: Bool'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [
                  args: ['x'],
                  doms: [
                    lambda: [
                      args: ["z"],
                      doms: ["Nat"],
                      yield_type: '::',
                      lambda_codomain: "z"
                    ]
                  ],
                  yield_type: '::',
                  lambda_codomain: "Bool"
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "heading with par" do
      text = 'f(x:X.x from (Y,Z))'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [
                  args: ['x'],
                  doms: ['X']
                ],
                decl_guards: [
                  appl: [op: :from, x: 'x', y: {:par, ['Y', 'Z']}]
                ]
              ]
            ]
          ]
        ]
      )
    end

    test "domain aliasing" do
      text = 'Status<={`ok}'

      tryparse(text,
        sections: [
          section: [
            head: [
              alias: [
                alias_name: ["Status"],
                alias_expr: {:set, [literal: "ok"]}
              ]
            ]
          ]
        ]
      )
    end

    test "multiple domain aliasing" do
      text = 'Status,State<={`ok}'

      tryparse(text,
        sections: [
          section: [
            head: [
              alias: [
                alias_name: ["Status", "State"],
                alias_expr: {:set, [literal: "ok"]}
              ]
            ]
          ]
        ]
      )
    end

    test "comprehension aliasing" do
      text = 'Day<={n:Nat,n<=30.n}'

      tryparse(text,
        sections: [
          section: [
            head: [
              alias: [
                alias_name: ["Day"],
                alias_expr:
                  {:comprehension,
                   [
                     set: [
                       {:comp_bindings,
                        [
                          binding: [bind_symbol: 'n', bind_op: :":", bind_domain: "Nat"],
                          guard: {:appl, [op: :lte, x: 'n', y: 30]}
                        ]},
                       {:comp_expression, 'n'}
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
    test "two sections" do
      text = 'f(x:Y)\n;\ny()=>Y'

      tryparse(text,
        sections: [
          section: [
            head: [
              decl: [
                decl_ident: 'f',
                decl_args: [
                  args: ['x'],
                  doms: ['Y']
                ]
              ]
            ]
          ],
          section: [
            head: [
              decl: [
                decl_ident: 'y',
                yield_type: '=>',
                lambda_codomain: 'Y'
              ]
            ]
          ]
        ]
      )
    end
  end

  describe "comments handling" do
    test "can parse a comment" do
      text = 'f()\n---\nf>1\n\" Here is a comment.\nf<2'

      tryparse(text,
        sections: [
          section: [
            head: [decl: [decl_ident: 'f']],
            body: [
              expr: [appl: [op: '>', x: 'f', y: 1]],
              comment: ["Here is a comment."],
              expr: [appl: [op: '<', x: 'f', y: 2]]
            ]
          ]
        ]
      )
    end
  end

  describe "module handling" do
    test "module declaration" do
      text = 'module MOD\nf()'

      tryparse(text, [
        {:module, 'MOD'},
        sections: [section: [head: [decl: [decl_ident: 'f']]]]
      ])
    end

    test "module import" do
      text = 'import MOD\nf()'

      tryparse(text,
        imports: [import: ['MOD']],
        sections: [section: [head: [decl: [decl_ident: 'f']]]]
      )
    end

    test "module declaration and import" do
      text = 'module MOD\nimport MOD2\nf()'

      tryparse(text,
        module: 'MOD',
        imports: [import: ['MOD2']],
        sections: [section: [head: [decl: [decl_ident: 'f']]]]
      )
    end

    test "multiple module import" do
      text = 'import MOD,MOD2\nf()'

      tryparse(text,
        imports: [import: ['MOD', 'MOD2']],
        sections: [section: [head: [decl: [decl_ident: 'f']]]]
      )
    end

    test "two import lines" do
      text = 'import MOD\nimport MOD2\nf()'

      tryparse(text,
        imports: [import: ['MOD'], import: ['MOD2']],
        sections: [section: [head: [decl: [decl_ident: 'f']]]]
      )
    end
  end
end
