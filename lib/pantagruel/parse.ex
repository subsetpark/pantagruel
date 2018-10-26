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

  logical_operators =
    strings([
      {"∧", :and},
      {"∨", :or}
    ])

  binding_operators =
    strings([
      # Denotes belonging to a domain.
      {":", :in},
      # Denotes membership in a concrete set.
      {"∈", :from}
    ])

  lte = string("<=") |> replace(:lte)

  binary_operators = [
    lte
    | strings([
        {"=", :equals},
        {"!=", :notequals},
        {">=", :gte},
        {">", :gt},
        {"<", :lt},
        {"↔", :iff},
        {"→", :then},
        {"+", :plus},
        {"-", :minus},
        {"*", :times},
        {"/", :divides},
        {"^", :exp},
        {"\\", :insert},
        {"⊕", :xor},
        {"&", :intersection},
        {"|", :union}
      ])
  ]

  # A closed set of binary infix operators.
  operator =
    (binding_operators ++ logical_operators ++ binary_operators)
    |> choice

  # A closed set of unary prefix operators.
  unary_operator =
    strings([
      {"~", :not},
      {"#", :card}
    ])
    |> choice

  quantifier =
    strings([{"∃", :exists}, {"∀", :forall}])
    |> choice()

  refinement = string("←") |> replace(:refined)
  # Number values
  float =
    string("-")
    |> optional
    |> utf8_string([?0..?9], min: 1)
    |> string(".")
    |> utf8_string([?0..?9], min: 1)
    |> reduce({Enum, :join, [""]})
    |> traverse({Pantagruel.Parse.Expressions, :parse_float, []})

  # Any sequence of lower cased characters, suitable for variable names
  # or atom literals.
  @identifier_ranges [
    ?a..?z,
    ?A..?Z,
    ?0..?9,
    ??,
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
    unwrap_and_tag(logical_operators |> choice, :intro_op)
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
    string("λ")
    |> ignore
    |> optional
    |> string("(")
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
      string(")")
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
  # A <= D
  # Where D is some concrete domain and A is new binding that refers to
  # that domain.
  domain_aliasing =
    parsec(:domain)
    |> comma_join
    |> tag(:alias_name)
    |> concat(
      lte
      |> ignore
    )
    |> concat(
      parsec(:expression)
      |> unwrap_and_tag(:alias_expr)
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

  pared_identifiers = identifier |> comma_join() |> nested
  # An expression of the form
  #  X OP D
  # Where X is either a symbol or a comma-separated group of symbols,
  # OP is either the set or domain membership operator, and D is some
  # expression representing a domain.
  binding =
    [pared_identifiers, identifier]
    |> choice
    |> unwrap_and_tag(:bind_symbol)
    |> concat(binding_operators |> choice |> unwrap_and_tag(:bind_op))
    |> concat(parsec(:expression) |> unwrap_and_tag(:bind_domain))

  # An element in the first half of a quantification or comprehension,
  # either a binding form or an arbitrary expression acting as a guard on
  # the bindings.
  binding_or_guard =
    [
      binding |> tag(:binding),
      parsec(:expression) |> unwrap_and_tag(:guard)
    ]
    |> choice

  # A expression of the form
  # {B1, B2, BN ⸳ E}
  # Where {} can stand for any of the group delimiters, B is a series
  # of variable bindings of the form `x ∈ X`/`x : X` or predicates on
  # introduced variables and E is a expression formed from the bound
  # variable. Represents the map and filter operations across any container.
  comprehension =
    binding_or_guard
    |> comma_join()
    |> tag(:comp_bindings)
    |> ignore(such_that)
    |> concat(parsec(:expression) |> unwrap_and_tag(:comp_expression))
    |> nested
    |> tag(:comprehension)

  # An expression of the form
  # Q B1, B2, B3 ⸳ E
  # Where Q is either the universal quantifier ∀ or the existential
  # quantifier ∃, B is a series of bindings/predicates as above, and
  # E is an expression that holds for all members of the sets/domains in
  # the bindings, as in ∀, or at least one, as in ∃.
  quantification =
    quantifier
    |> unwrap_and_tag(:quantifier)
    |> concat(
      binding_or_guard
      |> comma_join
      |> tag(:quant_bindings)
    )
    |> ignore(such_that)
    |> concat(parsec(:expression) |> unwrap_and_tag(:quant_expression))
    |> tag(:quantification)

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
      parsec(:lambda),
      parsec(:domain)
      |> comma_join()
      |> nested,
      identifier
    ]
    |> choice
  )

  # PARSE COMBINATORS

  symbol =
    choice([
      integer(min: 1),
      literal,
      parsec(:lambda),
      operator,
      identifier,
      parsec(:domain)
    ])

  # An expression formed by joining subexpressions with dots, as in
  # `foo.bar.baz`, parsed as object/method-style access.
  dot_expression =
    [nested_expression, quantification, comprehension, symbol]
    |> choice()
    |> join(string("."), 1)
    |> traverse({Pantagruel.Parse.Expressions, :parse_dot_expression, []})

  # An expression of a prefix operator with a single operand.
  unary_expression =
    unary_operator
    |> unwrap_and_tag(:op)
    |> concat(
      space
      |> optional()
      |> ignore
    )
    |> concat(
      [float, dot_expression, nested_expression, comprehension, symbol]
      |> choice()
      |> unwrap_and_tag(:operand)
    )
    |> tag(:unary_exp)

  # Some single recursive expression, consisting of a single expression
  # component, or a function application tree of expression components.
  defparsec(
    :expression,
    [
      float,
      unary_expression,
      dot_expression,
      nested_expression,
      quantification,
      comprehension,
      symbol
    ]
    |> choice()
    |> join(space |> optional)
    |> traverse({Pantagruel.Parse.Expressions, :parse_function_application, []})
  )

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
