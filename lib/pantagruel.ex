defmodule Pantagruel do
  import IO, only: [puts: 1]
  import Pantagruel.Format

  alias Pantagruel.{Scan, Parse, Eval}

  @moduledoc """
  An interpreter for the Pantagruel language.

  Currently exposes a single feature, which is to evaluate a Pantagruel
  file, report any unbound variables, and then print a formatted
  representation of it.
  """

  @help """
  USAGE: pantagruel [-s] FILENAME
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
  def main(args) do
    case OptionParser.parse(args, aliases: [s: :scopes], strict: [scopes: :boolean]) do
      {_, [], _} ->
        IO.puts(@help)

      {flags, [filename], _} ->
        case read!(filename) do
          {:ok, parsed, "", %{}, _, _} ->
            case Eval.eval(parsed) do
              {:ok, scope} ->
                case flags do
                  [scopes: true] -> format_scopes(scope)
                  _ -> format_program(parsed)
                end
                |> puts

              {:error, {:unbound_variables, e}} ->
                handle_unbound_variables(e, parsed)
            end

          {:ok, parsed, rest, _, {row, col}, _} ->
            case parsed do
              [] ->
                puts("No Pantagruel source found.")

              _ ->
                parsed =
                  Enum.reverse(parsed)
                  |> hd()
                  |> format_section()

                rest = String.trim(rest)

                puts("#{row}:#{col}: Parse error.\n\nParsed:\n#{parsed}\n\nRemaining:\n#{rest}")
            end
        end
    end
  end

  defp handle_unbound_variables(e, parsed) do
    puts("Eval error.\n\nUnbound variables:")
    Enum.each(e.unbound, &puts("- #{format_exp(&1, e.scopes)}"))

    e.unbound
    |> Enum.filter(&match?({:quantification, _}, &1))
    |> case do
      [] ->
        :ok

      quantifiers ->
        quantifiers
        |> Enum.each(&handle_bad_bindings/1)
    end

    IO.puts("")

    format_program(parsed) |> puts
  end

  defp handle_bad_bindings(
         {:quantification, quantifier: _, quant_bindings: bindings, quant_expression: expr}
       ) do
    expr = {:quantification, quantifier: "…", quant_bindings: bindings, quant_expression: expr}

    puts(
      "Expected binding form. Found: \"#{format_exp(expr, [])}\"\nDid you forget to wrap your symbols in ()?"
    )
  end
end
