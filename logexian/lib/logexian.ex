defmodule ParserHelpers do
  import NimbleParsec
  defparsec :comma_join, string(",") |> optional |> ignore |> times(min: 1)
end
defmodule Logexian do
    @moduledoc """
  Documentation for Logexian.
  """
  import NimbleParsec
  import ParserHelpers


  where = ignore(optional(string(";;\n")))

  identifier = ignore(repeat(string(" ")))
  |> ascii_string([?a..?z], min: 1)
  |> ignore(repeat(string(" ")))

  domain = ignore(repeat(string(" ")))
  |> ascii_string([?A..?Z, ?a..?z], min: 1)
  |> ignore(repeat(string(" ")))

  function_vars = identifier
  |> comma_join
  |> wrap()

  function_doms = domain
  domain
  |> comma_join
  |> wrap()

  function_args = function_vars
  |> ignore(string(":"))
  |> concat(function_doms)

  decl = ignore(string("<"))
  |> concat(function_args)
  |> ignore(string(">"))

  defparsec :program, where |> concat(decl), debug: true
end
