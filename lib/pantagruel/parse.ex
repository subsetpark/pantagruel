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
  # The two primary ways of binding inside of a body.
  quantifier = choice(strings([{"∃", :exists}, {"∀", :forall}]))
  # Logical operators.
  log_and = string("∧") |> replace(:and)
  log_or = string("∨") |> replace(:or)
  log_not = string("¬") |> replace(:not)
  log_op = choice([log_and, log_or, log_not])
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
        # Denotes belonging to a domain.
        {":", :in},
        # Denotes membership in a concrete set.
        {"∈", :from}
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
  # Number values
  float =
    string("-")
    |> optional
    |> utf8_string([?0..?9], min: 1)
    |> string(".")
    |> utf8_string([?0..?9], min: 1)
    |> reduce({Enum, :join, [""]})

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
      quantifier,
      parsec(:lambda),
      log_op,
      relation,
      operator,
      float,
      integer(min: 1),
      literal,
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
  # L El O Er
  # Where L is a logical operator like ∧ or ∨, El and Er are the
  # left and right subexpressions, and O is a logical entailment operator
  # describing the relationship between the two sides.
  expression =
    unwrap_and_tag(log_op, :intro_op)
    |> optional
    |> concat(
      parsec(:subexpression)
      |> tag(:left)
      |> concat(
        entailment
        |> unwrap_and_tag(:op)
      )
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
        log_and
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
      |> concat(
        space
        |> optional
        |> ignore
        |> parsec(:subexpression)
        |> optional
      ),
      symbol
      |> concat(
        space
        |> optional
        |> ignore
        |> parsec(:subexpression)
        |> optional
      )
    ]
    |> choice
    |> traverse(:parse_expression)
  )

  # A series of one or more specification sections separated by ";;",
  # where each subsequent section defines any variables introduced
  # in the previous section.
  @spec program(String.t()) :: Pantagruel.t()
  defparsec(
    :program,
    section
    |> join(where)
  )

  # Expression Parsing
  #
  # Functions for pattern recognition on subexpressions in order to
  # restructure fixed forms like set comprehensions and existential
  # qualification.
  @doc """
  Parse a set comprehension form, of the form
  E, Q1, Q2, ... Qn
  Where E is some expression, and Qn is a binding of the symbols used
  in that expression from some set.
  """
  defp parse_comprehension(collection, exp, acc, []) do
    [:comprehension, collection, Enum.reverse(acc), exp]
  end

  defp parse_comprehension(collection, exp, acc, [[var, :from, dom] | rest]) do
    parse_comprehension(collection, exp, [{var, dom} | acc], rest)
  end

  defp parse_expression([:exists, variable, direction, domain | subexpression])
       when direction == :from or direction == :in do
    [:exists, variable, direction, domain, subexpression]
  end

  defp parse_expression([{collection, [exp, [var, :from, dom] | rest]}]) do
    parse_comprehension(collection, exp, [{var, dom}], rest)
  end

  defp parse_expression(e), do: e

  defp parse_expression(_rest, args, context, _line, _offset) do
    {args
     |> Enum.reverse()
     |> parse_expression
     |> Enum.reverse(), context}
  end
end
