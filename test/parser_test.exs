defmodule ParserTest do
  use ExUnit.Case

  describe "declaration" do
    test "basic declaration" do
      'f()'
      |> tryp(head: [{:declaration, decl_ident: 'f'}])
    end

    test "declaration with argument" do
      'f(x : X)'
      |> tryp(
        head: [
          {:declaration,
           [
             decl_ident: 'f',
             decl_args: [
               args: ['x'],
               doms: ['X']
             ]
           ]}
        ]
      )
    end

    test "declaration with two argument" do
      'f(x, y : X, Y)'
      |> tryp(
        head: [
          {:declaration,
           [
             decl_ident: 'f',
             decl_args: [
               args: ['x', 'y'],
               doms: ['X', 'Y']
             ]
           ]}
        ]
      )
    end

    test "declaration with yield" do
      'f() :: F'
      |> tryp(
        head: [
          {:declaration,
           [
             decl_ident: 'f',
             decl_yield: '::',
             decl_domain: 'F'
           ]}
        ]
      )
    end

    test "declaration with value" do
      'f(x, y : X, Y . x) => F'
      |> tryp(
        head: [
          {:declaration,
           [
             decl_ident: 'f',
             decl_args: [args: ['x', 'y'], doms: ['X', 'Y']],
             decl_guards: ['x'],
             decl_yield: '=>',
             decl_domain: 'F'
           ]}
        ]
      )
    end

    test "full declaration" do
      'f(x, y : X, Y . x > y) => F'
      |> tryp(
        head: [
          {:declaration,
           [
             decl_ident: 'f',
             decl_args: [args: ['x', 'y'], doms: ['X', 'Y']],
             decl_guards: [appl: [op: '>', x: 'x', y: 'y']],
             decl_yield: '=>',
             decl_domain: 'F'
           ]}
        ]
      )
    end

    test "declaration with unary guard" do
      'f(x:X . ~x)'
      |> tryp(
        head: [
          {:declaration,
           [
             decl_ident: 'f',
             decl_args: [args: ['x'], doms: ['X']],
             decl_guards: [
               appl: [op: '~', x: 'x']
             ]
           ]}
        ]
      )
    end

    test "declaration with function application" do
      'f(x:X . f x)'
      |> tryp(
        head: [
          {:declaration,
           [
             decl_ident: 'f',
             decl_args: [args: ['x'], doms: ['X']],
             decl_guards: [
               appl: [f: 'f', x: 'x']
             ]
           ]}
        ]
      )
    end

    test "declaration with expression application" do
      'f(x:X . (x + 1) x)'
      |> tryp(
        head: [
          {:declaration,
           [
             decl_ident: 'f',
             decl_args: [args: ['x'], doms: ['X']],
             decl_guards: [
               appl: [f: [appl: [op: '+', x: 'x', y: 1]], x: 'x']
             ]
           ]}
        ]
      )
    end

    test "declaration with two guards" do
      'f(x:X . x > 1, x < 3)'
      |> tryp(
        head: [
          {:declaration,
           [
             decl_ident: 'f',
             decl_args: [args: ['x'], doms: ['X']],
             decl_guards: [
               appl: [op: '>', x: 'x', y: 1],
               appl: [op: '<', x: 'x', y: 3]
             ]
           ]}
        ]
      )
    end
  end

  describe "alias" do
    test "domain alias" do
      'S <= String'
      |> tryp(head: [alias: [alias_name: ['S'], alias_expr: 'String']])
    end

    test "multiple domain alias" do
      'S, T <= String'
      |> tryp(head: [alias: [alias_name: ['S', 'T'], alias_expr: 'String']])
    end
  end

  describe "containers" do
    test "empty list" do
      'f(x:X . [])'
      |> tryp(
        head: [
          declaration: [
            decl_ident: 'f',
            decl_args: [args: ['x'], doms: ['X']],
            decl_guards: [list: []]
          ]
        ]
      )
    end

    test "list" do
      'f(x:X . [x])'
      |> tryp(
        head: [
          declaration: [
            decl_ident: 'f',
            decl_args: [args: ['x'], doms: ['X']],
            decl_guards: [list: ['x']]
          ]
        ]
      )
    end

    test "list with multiple items" do
      'f(x:X . [x, x 1])'
      |> tryp(
        head: [
          declaration: [
            decl_ident: 'f',
            decl_args: [args: ['x'], doms: ['X']],
            decl_guards: [list: ['x', appl: [f: 'x', x: 1]]]
          ]
        ]
      )
    end
  end

  describe "head" do
    test "two declarations" do
      'f()\ng()'
      |> tryp(
        head: [
          declaration: [decl_ident: 'f'],
          declaration: [decl_ident: 'g']
        ]
      )
    end

    test "comment and declaration" do
      '"  ok\nf()'
      |> tryp(
        head: [
          comment: 'ok',
          declaration: [decl_ident: 'f']
        ]
      )
    end

    test "blank line" do
      'f()\n\ng()'
      |> tryp(
        head: [
          declaration: [decl_ident: 'f'],
          declaration: [decl_ident: 'g']
        ]
      )
    end
  end

  describe "comments" do
    test "comment with no newline" do
      '"  ok'
      |> tryp(head: [comment: 'ok'])
    end
  end

  describe "precendence" do
    test "declaration with precedence" do
      'f(x: Nat . f x > g y)'
      |> tryp(
        head: [
          declaration: [
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
      )
    end

    test "declaration with function precedence" do
      'f(x: Nat . f x y)'
      |> tryp(
        head: [
          declaration: [
            decl_ident: 'f',
            decl_args: [args: ['x'], doms: ['Nat']],
            decl_guards: [appl: [f: {:appl, [f: 'f', x: 'x']}, x: 'y']]
          ]
        ]
      )
    end

    test "declaration with function application on unary" do
      'f(x: Nat . f ~ y)'
      |> tryp(
        head: [
          declaration: [
            decl_ident: 'f',
            decl_args: [args: ['x'], doms: ['Nat']],
            decl_guards: [appl: [f: 'f', x: {:appl, [op: '~', x: 'y']}]]
          ]
        ]
      )
    end

    test "declaration with unary associativity" do
      'f(x:X . ~ # z x)'
      |> tryp(
        head: [
          declaration: [
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
      )
    end
  end

  describe "section" do
    test "simple section" do
      'f()\n1'
      |> tryp(
        head: [declaration: [decl_ident: 'f']],
        body: [expr: 1]
      )
    end

    test "section refinement" do
      'f()\n1 <- 2'
      |> tryp(
        head: [declaration: [decl_ident: 'f']],
        body: [refinement: [pattern: 1, expr: 2]]
      )
    end

    test "section binary op" do
      'f()\n1 + 2'
      |> tryp(
        head: [declaration: [decl_ident: 'f']],
        body: [expr: {:appl, [op: '+', x: 1, y: 2]}]
      )
    end

    test "section function application" do
      'f()\nf x'
      |> tryp(
        head: [declaration: [decl_ident: 'f']],
        body: [expr: {:appl, [op: '+', x: 1, y: 2]}]
      )
    end

    test "section with two lines" do
      'f()\n1\n2'
      |> tryp(
        head: [declaration: [decl_ident: 'f']],
        body: [expr: 1, expr: 2]
      )
    end
  end

  defp tryp(string, expected) do
    string = string ++ '\n'
    {:ok, tokens, _} = :lexer.string(string) |> IO.inspect()
    {:ok, parsed} = :parser.parse(tokens)
    assert expected == parsed
  end
end
