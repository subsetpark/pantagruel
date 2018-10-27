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
  Given a filename, evaluate it as a Pantagruel program, print out the
  evaluated scope, and then pretty-print the program.
  """
  def main(args) do
    args
    |> OptionParser.parse(aliases: [s: :scopes], strict: [scopes: :boolean])
    |> handle
  end

  defp handle({flags, [filename], _}) do
    filename
    |> File.read!()
    |> Scan.scan()
    |> Parse.program()
    |> handle_parse(flags)
  end

  defp handle({_, _, _}), do: IO.puts(@help)

  defp handle_parse({:ok, parsed, "", %{}, _, _}, flags) do
    Eval.eval(parsed)
    |> handle_eval(parsed, flags)
  end

  defp handle_parse({:ok, [], _, _, {_, _}, _}, _), do: puts("No Pantagruel source found.")

  defp handle_parse({:ok, parsed, rest, _, {row, col}, _}, _) do
    parsed =
      Enum.reverse(parsed)
      |> hd()
      |> format_section()

    rest = String.trim(rest)

    """
    #{row}:#{col}: Parse error.

    Parsed:
    #{parsed}

    Remaining:
    #{rest}
    """
    |> puts
  end

  defp handle_eval({:ok, scope}, _, scopes: true), do: format_scopes(scope) |> puts
  defp handle_eval({:ok, _}, parsed, _), do: format_program(parsed) |> puts

  defp handle_eval({:error, {tag, e}}, parsed, _) do
    handle_error(tag, e)
    puts_in_error_handling(parsed)
  end

  defp handle_error(:unbound_variables, e) do
    puts("Eval error.\n\nUnbound variables:")
    Enum.each(e.unbound, &puts("- #{format_exp(&1, e.scopes)}"))

    e.unbound
    |> Enum.filter(&match?({:quantification, _}, &1))
    |> handle_bad_bindings
  end

  defp handle_error(:domain_mismatch, e) do
    puts("Eval error.\n\nCould not match arguments with domains:")
    IO.inspect(e.args, label: "Arguments")
    IO.inspect(e.doms, label: "Domains")
  end

  defp puts_in_error_handling(parsed), do: IO.puts("\n#{format_program(parsed)}")

  defp handle_bad_bindings([]), do: :ok

  defp handle_bad_bindings(quantifiers) do
    quantifiers
    |> Enum.each(fn {
                      :quantification,
                      quantifier: _, quant_bindings: bindings, quant_expression: exp
                    } ->
      exp_str =
        {:quantification, quantifier: "…", quant_bindings: bindings, quant_expression: exp}
        |> format_exp([])

      """
      Expected binding form. Found:

      #{exp_str}

      Did you forget to wrap your symbols in ()?
      """
      |> puts
    end)
  end
end
