defmodule Pantagruel.Guards do
  defguard is_container(c) when c in [:bunch, :list, :string, :set]
end

defmodule Pantagruel do
  import IO, only: [puts: 1]
  import Pantagruel.Format, only: [format_exp: 2, format_program: 2, format_section: 1]

  alias Pantagruel.{Scan, Parse, Eval}
  alias Pantagruel.Env.{UnboundVariablesError, SymbolExtractionError}

  @moduledoc """
  An interpreter for the Pantagruel language.

  Currently exposes a single feature, which is to evaluate a Pantagruel
  file, report any unbound variables, and then print a formatted
  representation of it.
  """
  @doc """
  Generate an AST representation of the given Pantagruel file.
  """
  def read!(filename) do
    filename
    |> File.read!()
    |> Scan.scan()
    |> Parse.program()
  end

  @doc """
  Given a filename, evaluate it as a Pantagruel program, print out the
  evaluated scope, and then pretty-print the program.
  """
  def main(filename) do
    case read!(filename) do
      {:ok, parsed, "", %{}, _, _} ->
        try do
          scope = Eval.eval(parsed)
          format_program(parsed, scope) |> puts
        rescue
          e in UnboundVariablesError -> handle_unbound_variables(e, parsed)
          e in SymbolExtractionError -> handle_bad_bindings(e, parsed)
        end

      {:ok, parsed, rest, _, {row, col}, _} ->
        parsed =
          Enum.reverse(parsed)
          |> hd()
          |> format_section()

        rest = String.trim(rest)

        puts("#{row}:#{col}: Parse error.\n\nParsed:\n#{parsed}\n\nRemaining:\n#{rest}")
    end
  end

  defp handle_unbound_variables(e, parsed) do
    puts("Eval error.\n\nUnbound variables:")
    Enum.each(e.unbound, &puts("- #{format_exp(&1, e.scopes)}"))
    puts_parsed(parsed)
  end

  defp handle_bad_bindings(e, parsed) do
    expr =
      {:quantifier, quant_operator: "…", quant_bindings: e.bindings, quant_expression: e.expr}

    puts(~s(Syntax error.\n\nExpected binding form. Found: "#{format_exp(expr, [])}"))
    puts_parsed(parsed)
  end

  defp puts_parsed(parsed), do: parsed |> format_program(for(_ <- parsed, do: %{})) |> puts()
end
