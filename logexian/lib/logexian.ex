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
      |> ignore(joiner)
      |> ignore_spaces()
    )
    |> times(
      ignore_spaces()
      |> concat(c)
      |> ignore_spaces(),
      min: 1
    )
  end

  def comma_join(c), do: join(c, string(","))

  def optional_bunch(c \\ empty()) do
    ignore(optional(string("(")))
    |> ignore_spaces()
    |> concat(c)
    |> comma_join
    |> ignore_spaces()
    |> ignore(optional(string(")")))
  end
end

defmodule Logexian do
  @moduledoc """
  Documentation for Logexian.
  """
  import NimbleParsec
  import ParserHelpers

  where = string(";;\n")

  identifier =
    ignore(repeat(string(" ")))
    |> ascii_string([?a..?z], min: 1)
    |> ignore_spaces()

  domain =
    ignore(repeat(string(" ")))
    |> ascii_string([?A..?Z, ?a..?z], min: 1)
    |> ignore_spaces()

  function_vars = tag(optional_bunch(identifier), :decl_args)
  function_doms = tag(optional_bunch(domain), :decl_doms)

  function_args =
    function_vars
    |> ignore(string(":"))
    |> concat(function_doms)

  decl_yields =
    ignore_spaces()
    |> choice(for s <- ["::", "=>"], do: string(s))
    |> unwrap_and_tag(:yield_type)
    |> ignore_spaces()
    |> concat(unwrap_and_tag(domain, :yield_domain))

  decl =
    identifier
    |> unwrap_and_tag(:decl_ident)
    |> ignore(string("<"))
    |> concat(optional(function_args))
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
