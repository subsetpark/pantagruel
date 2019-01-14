defmodule Pantagruel do
  import IO, only: [puts: 1]
  import Pantagruel.Format

  alias Pantagruel.{Eval, Parse, Bool}

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
      aliases: [s: :scopes, p: :path, r: :shell],
      strict: [scopes: :boolean, path: :keep, shell: :boolean]
    )
    |> handle
  end

  defp handle({flags, [filename], _}) do
    filename
    |> File.read!()
    |> Pantagruel.Scan.scan()
    |> :pant_lexer.string()
    |> Parse.handle_lex()
    |> handle_parse(flags)
  end

  defp handle({_, _, _}), do: IO.puts(@help)

  defp handle_parse({:error, {line, module, message}}, _) do
    IO.puts("Line #{line}: syntax error.")
    message |> module.format_error() |> IO.puts()
  end

  defp handle_parse({:ok, []}, _), do: puts("No Pantagruel source found.")

  defp handle_parse({:ok, parsed}, shell: true) do
    Bool.Convert.convert(parsed)
    |> format_lifted()
    |> puts
  end

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

  defp handle_eval({:error, e}, parsed, _) do
    IO.puts("Eval error.")
    handle_error(e)
    puts_in_error_handling(parsed)
  end

  defp handle_error({:unbound_variables, unbounds, scopes}) do
    puts("Unbound variables:")
    Enum.each(unbounds, &puts("- #{format_exp(&1, scopes)}"))

    unbounds
    |> Enum.filter(&match?({:quantification, _}, &1))
    |> handle_bad_bindings
  end

  defp handle_error({:missing_import, e}) do
    puts("Imported module could not be found: #{e.mod_name}")
  end

  defp handle_error({:module_shadow, e}) do
    puts("Attempted to redfine defined module: #{e.mod_name}")
  end

  defp puts_in_error_handling(parsed), do: IO.puts("\n#{format_program(parsed)}")

  defp handle_bad_bindings([]), do: :ok

  defp handle_bad_bindings(quantifiers) do
    quantifiers
    |> Enum.each(fn {:quantification, [_, bindings, exp]} ->
      exp_str = {:quantification, ["…", bindings, exp]} |> format_exp([])

      """
      Expected binding form. Found:

      #{exp_str}

      Did you forget to wrap your symbols in ()?
      """
      |> puts
    end)
  end
end
