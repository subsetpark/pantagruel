defmodule ParserHelpers do
  import NimbleParsec

  def join(c, joiner) do
    repeat(
      c
      |> ignore(joiner)
    )
    |> concat(c)
  end

  defp sensitive_join(c, joiner) do
    c
    |> repeat(
      ignore(joiner)
      |> concat(c)
    )
  end

  def comma_join(c), do: join(c, string(","))
  def newline_join(c), do: sensitive_join(c, string("\n"))
  def space_join(c), do: sensitive_join(c, string(" "))

  defp _string({s, r}) do
    s
    |> string
    |> replace(r)
  end

  defp _string(s), do: string(s)

  def strings(ss) do
    for(s <- ss, do: _string(s))
  end

  def nested(c) do
    choice(
      for {l, r, name} <- [
            {"(", ")", :bunch},
            {"[", "]", :list},
            {"{", "}", :set},
            {"\"", "\"", :string}
          ],
          do:
            ignore(string(l))
            |> concat(c)
            |> ignore(string(r))
            |> tag(name)
    )
  end
end

defmodule Pantagruel.Parse do
  @moduledoc """
  Documentation for Pantagruel.
  """
  import NimbleParsec
  import ParserHelpers

  # Constants
  space = string(" ")
  newline = string("\n")
  # The section separator, pronounced "where".
  where = string("\n;;\n") |> replace(:where)

  log_and = string("∧") |> replace(:and)
  log_or = string("∨") |> replace(:or)
  log_op = choice([log_and, log_or])

  # A closed set of non-alphabetic binary
  # operators producing a boolean value.
  relation =
    choice(
      strings([
        {"==", :equals},
        {"!=", :notequals},
        {">", :gt},
        {"<", :lt},
        {">=", :gte},
        {"<=", :lte},
        {":", :in}
      ])
    )

  # A closed set of non-alphabetic binary operators where both arguments
  # and the value are all in the same domain.
  operator = choice(strings(["+", "-", "*", "/", "^"]))

  # A closed set of binary operators between propositions.
  # =, pronounced "equals" or "if and only if", denotes strict
  # logical equivalence. "P -> Q", pronounced "P implies Q" or
  # "if P then Q", denotes non-strict, one-way equivalence, allowing
  # for non-pure function or for Q to be true without P.
  entailment = choice(strings([{"=", :iff}, {"→", :then}]))

  float =
    optional(string("-"))
    |> utf8_string([?0..?9], min: 1)
    |> string(".")
    |> utf8_string([?0..?9], min: 1)
    |> reduce({Enum, :join, [""]})

  # Any variable name. Lower-cased.
  identifier = utf8_string([?a..?z], min: 1)

  value = choice([float, integer(min: 1), identifier])

  symbol = choice([parsec(:lambda), log_op, relation, operator, value, parsec(:domain)])

  nested_subexpression =
    :subexpression
    |> parsec
    |> wrap
    |> comma_join
    |> optional
    |> nested

  expression =
    unwrap_and_tag(log_op, :intro_op)
    |> ignore(space)
    |> optional()
    |> optional(
      tag(parsec(:subexpression), :left)
      |> unwrap_and_tag(entailment, :op)
    )
    |> tag(parsec(:subexpression), :right)
    |> tag(:expr)

  defp recognize_expression(["exists", variable, "from", domain | subexpression]) do
    [:exists, variable, :from, domain, subexpression]
  end

  defp recognize_expression(["exists", variable, "in", domain | subexpression]) do
    [:exists, variable, :in, domain, subexpression]
  end

  defp recognize_expression(e) do
    e
  end

  defp recognize_expression(_rest, args, context, _line, _offset) do
    {args |> Enum.reverse() |> recognize_expression |> Enum.reverse(), context}
  end

  # The arguments to a function. Takes the form of
  #   x1, x2, ... xn : X1, X2 ... XN
  # Where the list before the colon is a list of variable bindings,
  # and the list after is a list of domains of the introduced variables.
  decl_args =
    tag(comma_join(identifier), :decl_args)
    |> ignore(string(":"))
    |> concat(
      tag(
        comma_join(parsec(:domain)),
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
    choice(
      strings([
        {"::", :yields},
        {"=>", :produces}
      ])
    )
    |> unwrap_and_tag(:yield_type)
    |> unwrap_and_tag(parsec(:domain), :yield_domain)

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
        |> concat(expression |> comma_join)
      )
    )
    |> ignore(string("|"))
    |> optional(decl_yields)

  # A statement introducing some symbol and defining it
  # as a function - including a type constructor.
  decl =
    identifier
    |> unwrap_and_tag(:decl_ident)
    |> concat(fun)
    |> tag(:decl)

  # A series of one or more function declarations followed by
  # 0 or more expressions which should evaluate to true if the
  # specification holds.
  section =
    newline_join(decl)
    |> optional(
      ignore(newline)
      |> concat(expression |> newline_join)
    )
    |> tag(:sect)

  # Combinators

  # A function form, treated as a value or domain.
  defcombinatorp(
    :lambda,
    fun
    |> tag(:lambda)
  )

  # The domain of a variable. Can be a type (including a function)
  # or a set of concrete values.
  defcombinatorp(
    :domain,
    [
      nested(parsec(:domain)),
      utf8_string([?A..?Z, ?a..?z], min: 1),
      parsec(:lambda)
    ]
    |> choice
  )

  defparsec(
    :subexpression,
    [
      # We have to explicitly include both paths to avoid left-recursion.
      nested_subexpression
      |> optional(space |> optional |> ignore |> parsec(:subexpression)),
      symbol
      |> optional(space |> optional |> ignore |> parsec(:subexpression))
    ]
    |> choice
    |> traverse(:recognize_expression)
  )

  # A series of one or more specification sections separated by ";;",
  # where each subsequent section defines any variables introduced
  # in the previous section.
  defparsec(
    :program,
    join(section, where)
  )
end
