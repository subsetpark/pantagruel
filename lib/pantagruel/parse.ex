defmodule Pantagruel.Parse do
  @moduledoc """
  The parser takes a binary representing a Pantagruel program and
  constructs an AST. It expects the binary to have been preprocessed by
  the Scan module.
  """
  import NimbleParsec
  import Pantagruel.Parse.Util

  @type comment :: {:comment, [binary()]}
  @type head_stmt :: {:decl, keyword} | {:alias, keyword} | comment
  @type body_stmt :: {:expr, keyword} | comment
  @type section :: [head: [head_stmt], body: [body_stmt]]
  @typedoc """
  A Pantagruel AST consists of a list of *section* nodes, each of which
  represent one section of the program delimited by the `;` separator.

  A section has two components: the head and the body. The head
  contains declarations, either function declarations or domain alias
  declarations. The body contains expressions which should evaluate to
  true propositions about the program being described.

  A section must have at least one statement in its head. The body
  is optional.
  """
  @type t :: [section]
  @type combinator_resp ::
          {:ok, t(), binary(), map(), {pos_integer(), pos_integer()}, pos_integer()}

  newline = string("\n")
  space = string(" ")
  # Logical operators.
  log_and = string("∧") |> replace(:and)
  log_or = string("∨") |> replace(:or)
  # A closed set of non-alphabetic binary or unary functions.
  operator =
    choice([
      log_and,
      log_or
      | strings([
          {"=", :equals},
          {"!=", :notequals},
          {">=", :gte},
          {"<=", :lte},
          {">", :gt},
          {"<", :lt},
          # Denotes belonging to a domain.
          {":", :in},
          # Denotes membership in a concrete set.
          {"∈", :from},
          {"~", :not},
          {"↔", :iff},
          {"→", :then},
          {"+", :plus},
          {"-", :minus},
          {"*", :times},
          {"/", :divides},
          {"^", :exp},
          {"#", :card},
          {"\\", :insert},
          {"⊕", :xor}
        ])
    ])

  @binary_operators [
    :and,
    :or,
    :equals,
    :notequals,
    :gte,
    :lte,
    :gt,
    :lt,
    :in,
    :iff,
    :then,
    :from,
    :plus,
    :minus,
    :times,
    :divides,
    :exp,
    :insert,
    :xor
  ]

  @unary_operators [
    :not,
    :card
  ]

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

  defp parse_float(_rest, [arg], context, _line, _offset) do
    {[String.to_float(arg)], context}
  end

  # Any sequence of lower cased characters, suitable for variable names
  # or atom literals.
  @identifier_ranges [
    ?a..?z,
    ?A..?Z,
    ?0..?9,
    ??,
    ?.,
    ?',
    ?_,
    ?¿..?ƿ,
    {:not, ?×},
    {:not, ?÷},
    ?Α..?Ֆ,
    {:not, ?΢},
    {:not, ?԰}
  ]
  # 'Unbreak syntax
  identifier = utf8_string(@identifier_ranges, min: 1)
  # The full range of strings allowed inside of a string literal.
  spaced_text = utf8_string(@identifier_ranges ++ [?\s], min: 1)
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

  # The individual component elements of an expression.
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
  nested_expression =
    parsec(:expression)
    |> comma_join
    |> optional
    |> nested

  such_that = string("⸳") |> replace(:suchthat)
  # An expression of the form
  # P . G ← E
  # Where P is some pattern to match, `. G` is an optional guard
  # expression, and E is an expression that refines that pattern.
  guarded_refinement =
    parsec(:expression)
    |> unwrap_and_tag(:pattern)
    |> concat(
      such_that
      |> ignore
      |> parsec(:expression)
      |> unwrap_and_tag(:guard)
      |> optional
    )
    |> concat(
      refinement
      |> ignore
    )
    |> concat(parsec(:expression) |> unwrap_and_tag(:expr))
    |> tag(:refinement)

  # A single proposition in the language. Takes the form of
  # L E
  # Where L is a logical operator like ∧ or ∨, El and E is either
  # a guarded refinement or a single expression.
  statement =
    unwrap_and_tag(choice([log_and, log_or]), :intro_op)
    |> optional
    |> choice([guarded_refinement, parsec(:expression)])
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

  yield = string("∷") |> replace(:function)
  constructor = string("⇒") |> replace(:constructor)
  # The codomain of a function. Takes the form of
  #   :: D
  # or
  #   => D
  # Where :: is pronounced "of the type" and denotes the "return"
  # domain of a function, and => is pronounced "produces" and
  # denotes that the function is a type constructor.
  lambda_codomain =
    choice([yield, constructor])
    |> unwrap_and_tag(:yield_type)
    |> concat(
      parsec(:domain)
      |> unwrap_and_tag(:lambda_codomain)
    )

  # A function from (0 or more) = N arguments in <= N domains,
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
          parsec(:expression)
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

  # A header statement binding some symbol as a function or a type
  # constructor (which latter also binds the resulting type).
  decl =
    identifier
    |> unwrap_and_tag(:decl_ident)
    |> concat(fun)
    |> tag(:decl)

  # A header statement of the form
  # D => A
  # Where D is some concrete domain and A is new binding that refers to
  # that domain.
  domain_aliasing =
    parsec(:expression)
    |> unwrap_and_tag(:alias_expr)
    |> concat(
      constructor
      |> ignore
    )
    |> concat(
      parsec(:domain)
      |> comma_join
      |> tag(:alias_name)
    )
    |> tag(:alias)

  # Any line started with a `"`. Is not parsed, but included in the AST
  # for later reprinting.
  comment =
    string("\"")
    |> ignore()
    |> utf8_char([{:not, ?;}])
    |> utf8_string([{:not, ?\n}], min: 1)
    |> traverse(:join_comment)
    |> tag(:comment)

  defp join_comment(_rest, [string, char], context, _line, _offset) do
    {[String.trim(<<char>> <> string)], context}
  end

  # A series of one or more function declarations or domain aliases
  # followed by 0 or more expressions which should evaluate to true if
  # the specification holds.
  section =
    [decl, comment, domain_aliasing]
    |> choice
    |> newline_join
    |> tag(:head)
    |> concat(
      newline
      |> ignore
      |> concat(
        [statement, comment]
        |> choice
        |> newline_join
      )
      |> tag(:body)
      |> optional
    )
    |> tag(:section)

  # A expression of the form
  # {B1, B2, BN ⸳ E}
  # Where {} can stand for any of the group delimiters, B is a series
  # of variable bindings of the form `x ∈ X`/`x : X` or predicates on
  # introduced variables and E is a expression formed from the bound
  # variable. Represents the map and filter operations across any container.
  comprehension =
    parsec(:expression)
    |> comma_join()
    |> wrap
    |> ignore(such_that)
    |> concat(parsec(:expression))
    |> nested
    |> tag(:comprehension)

  exists = string("∃") |> replace(:exists)
  forall = string("∀") |> replace(:forall)

  # An expression of the form
  # Q B1, B2, B3 ⸳ E
  # Where Q is either the universal quantifier ∀ or the existential
  # quantifier ∃, B is a series of bindings/predicates as above, and
  # E is an expression that holds for all members of the sets/domains in
  # the bindings, as in ∀, or at least one, as in ∃.
  quantifier =
    choice([exists, forall])
    |> unwrap_and_tag(:quant_operator)
    |> concat(parsec(:expression) |> comma_join() |> tag(:quant_bindings))
    |> ignore(such_that)
    |> concat(parsec(:expression) |> unwrap_and_tag(:quant_expression))
    |> tag(:quantifier)

  # PRIVATE COMBINATORS

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
      parsec(:domain)
      |> comma_join()
      |> nested,
      identifier,
      parsec(:lambda)
    ]
    |> choice
  )

  # PARSE COMBINATORS

  # Some single recursive expression, consisting of a single expression
  # component, or a function application tree of expression components.
  defparsec(
    :expression,
    [nested_expression, quantifier, comprehension, symbol]
    |> choice()
    |> join(space |> optional)
    |> traverse(:parse_function_application)
  )

  # Parse a list of expressions, building up a function application tree
  # from the left.
  defp parse_function_application(_rest, expressions, context, _line, _offset) do
    parsed =
      expressions
      |> assoc_dots([])
      |> Enum.reverse()
      |> Enum.flat_map(&parse_dot_chain/1)
      |> Enum.reduce(&assoc/2)

    {[parsed], context}
  end

  # As a first pass, bind any dot expressions to the expression
  # immediately preceding.
  defp assoc_dots([], acc), do: Enum.reverse(acc)

  defp assoc_dots([e | rest], acc) when is_binary(e) do
    case String.starts_with?(e, ".") do
      true -> assoc_dots(rest, acc, e)
      false -> assoc_dots(rest, [e | acc])
    end
  end

  defp assoc_dots([e | rest], acc), do: assoc_dots(rest, [e | acc])

  defp assoc_dots([e | rest], acc, dot) do
    acc = [apply_f(dot, e) | acc]
    assoc_dots(rest, acc)
  end

  # Handle "foo.bar" dot-access expressions.
  defp parse_dot_chain(v) when is_binary(v) do
    [head | tail] = String.split(v, ".", trim: true)

    dot = &{:dot, "." <> &1}

    head =
      case String.starts_with?(v, ".") do
        true -> dot.(head)
        false -> head
      end

    # Dot access binds more tightly; parse first and return as an
    # expression.
    [head | Enum.map(tail, dot)] |> Enum.reduce(&assoc/2) |> List.wrap()
  end

  defp parse_dot_chain(v), do: [v]

  # Handle infix binary operators.
  defp assoc(x, [appl, binary_operator: op]), do: apply_f(op, appl, x)
  defp assoc(x, appl) when x in @binary_operators, do: [appl, binary_operator: x]
  # Handle prefix unary operators.
  defp assoc(x, appl) when appl in @unary_operators, do: apply_f(appl, x, nil)
  # Handle postfix dot-access operator.
  defp assoc({:dot, x}, appl), do: apply_f(x, appl)
  # Handle normal prefix function application.
  defp assoc(x, appl), do: apply_f(appl, x)

  # Create function application structures.
  defp apply_f(f, x), do: {:appl, [f: f, x: x]}
  defp apply_f(operator, x, nil), do: {:appl, operator: operator, x: x}
  defp apply_f(operator, x, y), do: {:appl, operator: operator, x: x, y: y}

  where =
    string(";")
    |> replace(:where)
    |> concat(newline |> repeat |> ignore)

  # A series of one or more specification sections separated by :where,
  # where each subsequent section defines any variables introduced
  # in the previous section.
  @spec program(String.t()) :: combinator_resp
  defparsec(
    :program,
    section
    |> concat(newline |> repeat |> ignore)
    |> join(where)
    |> optional
  )
end
