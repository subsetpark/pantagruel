defmodule ParserHelpers do
  import NimbleParsec

  def ignore_spaces(c \\ empty()) do
    c
    |> concat(
      [string(" "), string("\n")]
      |> choice
      |> optional
      |> ignore
    )
  end

  def join(c, joiner) do
    repeat(
      c
      |> ignore(joiner)
    )
    |> concat(c)
  end

  def space_join(c) do
    c
    |> repeat(
      ignore(string(" "))
      |> concat(c)
    )
  end

  def comma_join(c), do: join(c, string(","))

  def strings(ss), do: for(s <- ss, do: string(s))
end

defmodule Logexian.Parse do
  @moduledoc """
  Documentation for Logexian.
  """
  import NimbleParsec
  import ParserHelpers

  space = string(" ")
  newline = string("\n")
  # The section separator, pronounced "where".
  where =
    ignore(repeat(newline))
    |> string(";;\n")
    |> ignore(repeat(newline))

  # Any variable name. Lower-cased.
  identifier = utf8_string([?a..?z], min: 1)

  float =
    optional(string("-"))
    |> utf8_string([?0..?9], min: 1)
    |> string(".")
    |> utf8_string([?0..?9], min: 1)
    |> reduce({Enum, :join, [""]})

  value = choice([float, integer(min: 1), identifier])

  # The domain of a variable. Can be a type (including a function)
  # or a set of concrete values.
  domain =
    choice([
      utf8_string([?A..?Z, ?a..?z], min: 1),
      parsec(:lambda)
    ])

  log_and = choice(strings(["and", "∧"]))
  log_or = choice(strings(["or", "∨"]))
  log_op = choice([log_and, log_or])

  # The arguments to a function. Takes the form of
  #   x1, x2, ... xn : X1, X2 ... XN
  # Where the list before the colon is a list of variable bindings,
  # and the list after is a list of domains of the introduced variables.
  decl_args =
    tag(comma_join(identifier), :decl_args)
    |> ignore(string(":"))
    |> concat(
      tag(
        comma_join(domain),
        :decl_doms
      )
    )

  # The codomain of a function. Takes the form of
  #   :: D
  # or
  #   => D
  # Where :: is pronounced "of the type" and denotes the "return"
  # domain of a function, and => is pronounced "produces" and
  # denotes that the function is a type constructor.
  decl_yields =
    choice(strings(["::", "=>"]))
    |> unwrap_and_tag(:yield_type)
    |> concat(unwrap_and_tag(domain, :yield_domain))

  # A closed set of non-alphabetic binary
  # operators producing a boolean value.
  relation =
    choice(
      strings([
        "==",
        "!=",
        ">",
        "<",
        ">=",
        "<=",
        ":"
      ])
    )

  # A closed set of non-alphabetic binary operators where both arguments
  # and the value are all in the same domain.
  operator = choice(strings(["+", "-", "*", "/", "^"]))

  symbol = choice([parsec(:lambda), log_op, relation, operator, value, domain])

  # A closed set of binary operators between propositions.
  # =, pronounced "equals" or "if and only if", denotes strict
  # logical equivalence. "P -> Q", pronounced "P implies Q" or
  # "if P then Q", denotes non-strict, one-way equivalence, allowing
  # for non-pure function or for Q to be true without P.
  entailment = choice(strings(["=", "→"]))

  nested_subexpression =
    choice(
      for {l, r, name} <- [
            {"(", ")", :bunch},
            {"[", "]", :list},
            {"{", "}", :set},
            {"\"", "\"", :string}
          ],
          do:
            ignore(string(l))
            |> ignore(optional(space))
            |> parsec(:subexpression)
            |> ignore(optional(space))
            |> ignore(string(r))
            |> tag(name)
    )

  defparsec(
    :subexpression,
    choice([
      nested_subexpression,
      symbol |> optional(ignore(optional(space)) |> parsec(:subexpression))
    ])
  )

  expression =
    optional(unwrap_and_tag(log_op, :intro_op) |> ignore(space))
    |> optional(
      tag(parsec(:subexpression), :left)
      |> unwrap_and_tag(entailment, :op)
    )
    |> tag(parsec(:subexpression), :right)
    |> ignore(optional(space))
    |> tag(:expr)

  # A function from (0 or more) = N arguments in N domains,
  # with an optional codomain.
  fun =
    ignore(string("|"))
    |> optional(
      decl_args
      |> optional(
        ignore(space)
        |> ignore(log_and)
        |> ignore(space)
        |> concat(comma_join(expression))
      )
    )
    |> ignore(string("|"))
    |> concat(optional(decl_yields))

  # A function form, treated as a value or domain.
  defcombinatorp(
    :lambda,
    fun
    |> tag(:lambda)
  )

  # A statement introducing some symbol and defining it
  # as a function - including a type constructor.
  decl =
    identifier
    |> unwrap_and_tag(:decl_ident)
    |> ignore(optional(space))
    |> concat(fun)
    |> tag(:decl)

  # A series of one or more function declarations followed by
  # 0 or more expressions which should evaluate to true if the
  # specification holds.
  section =
    decl
    |> repeat(
      ignore(newline)
      |> concat(expression)
    )
    |> tag(:sect)

  # A series of one or more specification sections separated by ";;",
  # where each subsequent section defines any variables introduced
  # in the previous section.
  defparsec(
    :program,
    join(section, where)
  )
end
