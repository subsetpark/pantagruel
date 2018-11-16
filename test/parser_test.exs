defmodule ParserTest do
  use ExUnit.Case

  describe "declaration" do
    test "basic declaration" do
      'f()'
      |> tryp(
        head: [{:declaration, decl_ident: 'f'}],
        body: []
      )
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
        ],
        body: []
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
        ],
        body: []
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
        ],
        body: []
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
        ],
        body: []
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
        ],
        body: []
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
        ],
        body: []
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
        ],
        body: []
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
        ],
        body: []
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
        ],
        body: []
      )
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
        ],
        body: []
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
        ],
        body: []
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
        ],
        body: []
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
        ],
        body: []
      )
    end

    test "comment and declaration" do
      '"  ok\nf()'
      |> tryp(
        head: [
          comment: 'ok',
          declaration: [decl_ident: 'f']
        ],
        body: []
      )
    end

    test "blank line" do
      'f()\n\ng()'
      |> tryp(
        head: [
          declaration: [decl_ident: 'f'],
          declaration: [decl_ident: 'g']
        ],
        body: []
      )
    end
  end

  describe "comments" do
    test "comment line" do
      '"  ok\n'
      |> tryp(head: [comment: 'ok'], body: [])
    end

    test "comment with no newline" do
      '"  ok'
      |> tryp(head: [comment: 'ok'], body: [])
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
        ],
        body: []
      )
    end

    test "declaration with function precedence" do
      'f(x: Nat . f x y)'
      |> tryp(
        head: [
          declaration: [
            decl_ident: 'f',
            decl_args: [args: ['x'], doms: ['Nat']],
            decl_guards: [
              appl: [
                f: 'f',
                x: {:appl, [f: 'x', x: 'y']}
              ]
            ]
          ]
        ],
        body: []
      )
    end
  end

  defp tryp(string, expected) do
    {:ok, tokens, _} = :lexer.string(string) |> IO.inspect()
    {:ok, parsed} = :parser.parse(tokens)
    assert expected == parsed
  end
end
