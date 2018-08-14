defmodule ParserHelpers do
  import NimbleParsec

  def ignore_spaces(c \\ empty()) do
    c
    |> ignore(
      choice([string(" "), string("\n")])
      |> repeat()
    )
  end

  def join(c, joiner) do
    repeat(
      c
      |> ignore_spaces()
      |> ignore(joiner)
      |> ignore_spaces()
    )
    |> concat(c)
    |> ignore_spaces()
  end

  def comma_join(c), do: join(c, string(","))

  def strings(ss), do: for(s <- ss, do: string(s))
end

defmodule Logexian do
  @moduledoc """
  Documentation for Logexian.
  """
  import NimbleParsec
  import ParserHelpers

  where = string(";;\n")

  identifier =
    ignore_spaces()
    |> ascii_string([?a..?z], min: 1)
    |> ignore_spaces()

  operator = choice(strings(["!=", "==", ">", "<", "<=", ">="]))

  log_and = choice(strings(["and", "∧"]))

  defparsec(
    :expression,
    choice([parsec(:relation), integer(min: 1), identifier])
  )

  defparsec(
    :relation,
    parsec(:expression)
    |> unwrap_and_tag(:left)
    |> concat(operator |> unwrap_and_tag(:operator))
    |> concat(
      parsec(:expression)
      |> unwrap_and_tag(:right)
    )
  )

  domain =
    ignore_spaces()
    |> ascii_string([?A..?Z, ?a..?z], min: 1)
    |> ignore_spaces()

  decl_args =
    tag(comma_join(identifier), :decl_args)
    |> ignore(string(":"))
    |> concat(tag(comma_join(domain), :decl_doms))
    |> ignore_spaces()

  decl_yields =
    ignore_spaces()
    |> strings(["::", "=>"])
    |> unwrap_and_tag(:yield_type)
    |> ignore_spaces()
    |> concat(unwrap_and_tag(domain, :yield_domain))

  decl =
    identifier
    |> unwrap_and_tag(:decl_ident)
    |> ignore(string("<"))
    |> optional(
      decl_args
      |> optional(
        log_and
        |> concat(comma_join(parsec(:expression)))
      )
    )
    |> debug()
    |> ignore(string(">"))
    |> concat(optional(decl_yields))
    |> ignore_spaces()
    |> tag(:decl)

  defparsec(
    :program,
    empty()
    |> ignore_spaces()
    |> concat(join(decl, where))
    |> ignore_spaces()
  )
end
