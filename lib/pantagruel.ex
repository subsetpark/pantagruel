defmodule Pantagruel do
  import IO, only: [puts: 1]
  import Pantagruel.Format

  alias Pantagruel.{Eval}

  @moduledoc """
  An interpreter for the Pantagruel language.

  Currently exposes a single feature, which is to evaluate a Pantagruel
  file, report any unbound variables, and then print a formatted
  representation of it.
  """

  @default_path "./pant"

  @help """
  USAGE: pantagruel [-s]  [-p PATH ...] FILENAME
  """

  @doc """
  Given a filename, evaluate it as a Pantagruel program, print out the
  evaluated scope, and then pretty-print the program.
  """
  def main(args) do
    args
    |> OptionParser.parse(
      aliases: [s: :scopes, p: :path],
      strict: [scopes: :boolean, path: :keep]
    )
    |> handle
  end

  defp handle({flags, [filename], _}) do
    filename
    |> File.open!([:utf8, :charlist])
    |> IO.read(:all)
    |> :lexer.string()
    |> handle_lex()
    |> handle_parse(flags)
  end

  defp handle({_, _, _}), do: IO.puts(@help)

  defp handle_lex({:ok, tokens, _linecount}), do: :parser.parse(tokens)

  defp handle_parse({:ok, []}, _), do: puts("No Pantagruel source found.")

  defp handle_parse({:ok, parsed}, flags) do
    # Paths to additional .pant file hierarchies can be passed in with
    # the :path flag. The default path will also always be checked.
    [@default_path | Keyword.get_values(flags, :path)]
    |> Pantagruel.Load.load()
    |> case do
      {:ok, asts} ->
        Eval.eval(parsed, asts)
        |> handle_eval(parsed, flags)

      {:error, _} = error ->
        handle_eval(error, parsed, nil)
    end
  end

  defp handle_eval({:ok, scope}, _, scopes: true), do: format_scopes(scope) |> puts
  defp handle_eval({:ok, _}, parsed, _), do: format_program(parsed) |> puts

  defp handle_eval({:error, {tag, e}}, parsed, _) do
    IO.puts("Eval error.")
    handle_error(tag, e)
    puts_in_error_handling(parsed)
  end

  defp handle_error(:unbound_variables, e) do
    puts("Unbound variables:")
    Enum.each(e.unbound, &puts("- #{format_exp(&1, e.scopes)}"))

    e.unbound
    |> Enum.filter(&match?({:quantification, _}, &1))
    |> handle_bad_bindings
  end

  defp handle_error(:domain_mismatch, e) do
    puts("Could not match arguments with domains:")
    IO.inspect(e.args, label: "Arguments")
    IO.inspect(e.doms, label: "Domains")
  end

  defp handle_error(:missing_import, e) do
    puts("Imported module could not be found: #{e.mod_name}")
  end

  defp handle_error(:module_shadow, e) do
    puts("Attempted to redfine defined module: #{e.mod_name}")
  end

  defp puts_in_error_handling(parsed), do: IO.puts("\n#{format_program(parsed)}")

  defp handle_bad_bindings([]), do: :ok

  defp handle_bad_bindings(quantifiers) do
    quantifiers
    |> Enum.each(fn {
                      :quantification,
                      quantifier: _, bindings: bindings, expr: exp
                    } ->
      exp_str =
        {:quantification, quantifier: "…", bindings: bindings, expr: exp}
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
