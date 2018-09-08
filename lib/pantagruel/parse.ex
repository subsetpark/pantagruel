defmodule Pantagruel.Parse do
  @moduledoc """
  Documentation for Pantagruel.
  """
  import NimbleParsec
  import Pantagruel.Parse.Util

  # Constants
  space = string(" ")
  newline = string("\n")
  # The section separator, pronounced "where".
  where = string("\n;;\n") |> replace(:where)
  # Logical operators.
  log_and = string("∧") |> replace(:and)
  log_or = string("∨") |> replace(:or)
  such_that = string("⸳") |> replace(:suchthat)
  exists = string("∃") |> replace(:exists)
  forall = string("∀") |> replace(:forall)
  # A closed set of non-alphabetic # binary or unary functions.
  operator =
    choice([
      log_and,
      log_or
      | strings([
          {"==", :equals},
          {"!=", :notequals},
          {">=", :gte},
          {"<=", :lte},
          {">", :gt},
          {"<", :lt},
          # Denotes belonging to a domain.
          {":", :in},
          # Denotes membership in a concrete set.
          {"∈", :from},
          {"¬", :not},
          {"=", :iff},
          {"→", :then},
          "+",
          "-",
          "*",
          "/",
          "^"
        ])
    ])

  refinement = string("←") |> replace(:refined)
  # Number values
  float =
    string("-")
    |> optional
    |> utf8_string([?0..?9], min: 1)
    |> string(".")
    |> utf8_string([?0..?9], min: 1)
    |> reduce({Enum, :join, [""]})
    |> traverse(:parse_float)

  # Any sequence of lower cased characters, suitable for variable names
  # or atom literals.
  @identifier_ranges [?a..?z, ?., ?', ?_]
  # Unbreak syntax'
  identifier = utf8_string(@identifier_ranges, min: 1)
  # The full range of strings allowed inside of a string literal.
  spaced_text = utf8_string(@identifier_ranges ++ [?\s, ?A..?Z, ?0..?9], min: 1)
  # Literals
  back_quote = string("`")
  # A text value that evaluates to itself. Either an identifier prefixed
  # by backticks, or a full string delimited by backticks.
  literal =
    choice([
      ignore(back_quote) |> concat(spaced_text) |> ignore(back_quote),
      ignore(back_quote) |> concat(identifier)
    ])
    |> unwrap_and_tag(:literal)

  # The individual component elements of a subexpression.
  symbol =
    choice([
      float,
      integer(min: 1),
      literal,
      parsec(:lambda),
      operator,
      identifier,
      parsec(:domain)
    ])

  # A sequence of one or more symbols.
  nested_subexpression =
    :subexpression
    |> parsec
    |> wrap
    |> comma_join
    |> optional
    |> nested

  # A single proposition in the language. Takes the form of
  # L El ← Er
  # Where L is a logical operator like ∧ or ∨, El and Er are the
  # left and right subexpressions, and ← is the logical refinement operator.
  expression =
    unwrap_and_tag(choice([log_and, log_or]), :intro_op)
    |> optional
    |> concat(
      parsec(:subexpression)
      |> tag(:left)
      |> ignore(refinement)
      |> optional
    )
    |> concat(
      parsec(:subexpression)
      |> tag(:right)
    )
    |> tag(:expr)

  # The arguments to a function. Takes the form of
  #   x1, x2, ... xn : X1, X2 ... XN
  # Where the list before the colon is a list of variable bindings,
  # and the list after is a list of domains of the introduced variables.
  lambda_args =
    identifier
    |> comma_join()
    |> tag(:lambda_args)
    |> concat(
      string(":")
      |> ignore
    )
    |> concat(
      tag(
        comma_join(parsec(:domain)),
        :lambda_doms
      )
    )

  # The codomain of a function. Takes the form of
  #   :: D
  # or
  #   => D
  # Where :: is pronounced "of the type" and denotes the "return"
  # domain of a function, and => is pronounced "produces" and
  # denotes that the function is a type constructor.
  lambda_codomain =
    choice(
      strings([
        {"::", :function},
        {"=>", :constructor}
      ])
    )
    |> unwrap_and_tag(:yield_type)
    |> concat(
      parsec(:domain)
      |> unwrap_and_tag(:lambda_codomain)
    )

  # A function from (0 or more) = N arguments in N domains,
  # with an optional codomain.
  fun =
    string("|")
    |> ignore
    |> concat(
      lambda_args
      |> concat(
        such_that
        |> ignore
        |> concat(
          parsec(:subexpression)
          |> wrap
          |> comma_join
          |> tag(:predicate)
        )
        |> optional
      )
      |> optional
    )
    |> concat(
      string("|")
      |> ignore
    )
    |> concat(
      lambda_codomain
      |> optional
    )

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
    decl
    |> newline_join
    |> tag(:head)
    |> concat(
      newline
      |> ignore
      |> concat(
        expression
        |> newline_join
      )
      |> tag(:body)
      |> optional
    )
    |> tag(:section)

  comprehension =
    comma_join(parsec(:subexpression) |> wrap)
    |> wrap
    |> ignore(such_that)
    |> concat(parsec(:subexpression) |> wrap)
    |> nested
    |> tag(:comprehension)

  quantifier =
    choice([exists, forall])
    |> concat(comma_join(parsec(:subexpression) |> wrap) |> wrap)
    |> ignore(such_that)
    |> concat(parsec(:subexpression) |> wrap)
    |> tag(:quantifier)

  # Combinators

  # A function form, treated as a value or domain.
  defcombinatorp(
    :lambda,
    fun |> tag(:lambda)
  )

  # The domain of a variable. Can be a type (including a function)
  # or a set of concrete values.
  defcombinatorp(
    :domain,
    [
      parsec(:domain) |> nested,
      utf8_string([?A..?Z, ?a..?z, ?0..?9], min: 1),
      parsec(:lambda)
    ]
    |> choice
  )

  defparsec(
    :subexpression,
    choice([nested_subexpression, quantifier, comprehension, symbol])
    |> concat(
      space
      |> optional
      |> ignore
      |> parsec(:subexpression)
      |> optional
    )
  )

  # A series of one or more specification sections separated by ";;",
  # where each subsequent section defines any variables introduced
  # in the previous section.
  @spec program(String.t()) ::
          {:ok, Pantagruel.t(), binary(), map(), {pos_integer(), pos_integer()}, pos_integer()}
  defparsec(
    :program,
    section
    |> join(where)
    |> optional
  )

  defp parse_float(_rest, [arg], context, _line, _offset) do
    {[String.to_float(arg)], context}
  end
end
