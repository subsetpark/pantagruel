defmodule ParserHelpers do
  @moduledoc """
  Combinator snippets required by the Pantagruel parser.
  """
  import NimbleParsec

  @doc """
  Given a combinator C and a joiner J, match on
  CJCJ ... C
  """
  def join(c, joiner) do
    c
    |> repeat(
      ignore(joiner)
      |> concat(c)
    )
  end

  def comma_join(c), do: join(c, string(","))
  def newline_join(c), do: join(c, string("\n"))

  @doc """
  Convert a list of strings to string combinators. Also accepts {string,
  value} pairs and will concatenate a replace combinator to the recognized
  string.
  """
  @spec strings([String.t()] | [{String.t(), atom}]) :: [NimbleParsec.t()]
  def strings(ss) do
    f = fn
      {s, r} ->
        s
        |> string
        |> replace(r)

      s ->
        string(s)
    end

    Enum.map(ss, f)
  end

  @doc """
  Utility function for recognizing combinators nested inside of matched
  delimiters.
  """
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
  # left and right subexpressions, and ← is the logical entailment operator.
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
  decl_args =
    identifier
    |> comma_join()
    |> tag(:decl_args)
    |> concat(
      string(":")
      |> ignore
    )
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
        {"::", :function},
        {"=>", :constructor}
      ])
    )
    |> unwrap_and_tag(:yield_type)
    |> concat(
      parsec(:domain)
      |> unwrap_and_tag(:yield_domain)
    )

  # A function from (0 or more) = N arguments in N domains,
  # with an optional codomain.
  fun =
    string("|")
    |> ignore
    |> concat(
      decl_args
      |> concat(
        such_that
        |> ignore
        |> concat(
          expression
          |> comma_join
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
      decl_yields
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
    parsec(:subexpression)
    |> wrap
    |> ignore(such_that)
    |> concat(comma_join(parsec(:subexpression) |> wrap) |> wrap)
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
    [
      # We have to explicitly include both paths to avoid left-recursion.
      nested_subexpression
      |> concat(
        space
        |> optional
        |> ignore
        |> parsec(:subexpression)
        |> optional
      ),
      choice([quantifier, comprehension, symbol])
      |> concat(
        space
        |> optional
        |> ignore
        |> parsec(:subexpression)
        |> optional
      )
    ]
    |> choice
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
  )

  defp parse_float(_rest, [arg], context, _line, _offset) do
    {[String.to_float(arg)], context}
  end
end
