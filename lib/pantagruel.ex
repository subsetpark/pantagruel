defmodule Pantagruel do
  import IO, only: [puts: 1]
  alias Pantagruel.Format
  use Witchcraft

  alias Pantagruel.{Eval, Parse, Bool}

  @moduledoc """
  An interpreter for the Pantagruel language.

  Currently exposes a single feature, which is to evaluate a Pantagruel
  file, report any unbound variables, and then print a formatted
  representation of it.
  """

  @default_path "./pantlib"

  @help """
  USAGE: pantagruel [-ecr]  [-p PATH ...] [-f FORMATTER] FILENAME
  """

  @doc """
  Given a filename, evaluate it as a Pantagruel program, print out the
  evaluated scope, and then pretty-print the program.
  """
  def main(args) do
    args
    |> OptionParser.parse(
      aliases: [e: :env, p: :path, r: :shell, c: :check, f: :formatter],
      strict: [env: :boolean, path: :keep, shell: :boolean, check: :boolean, formatter: :keep]
    )
    |> handle()
  end

  defp handle({flags, [filename], _}) do
    flags = Format.with_formatter(flags)

    case filename do
      "-" -> IO.read(:stdio, :all)
      name -> name |> File.read!()
    end
    |> Pantagruel.Scan.scan()
    |> :pant_lexer.string()
    |> Parse.handle_lex()
    |> handle_parse(flags)
    |> display(flags)
  end

  defp handle({_, _, _}), do: IO.puts(@help)

  defp handle_parse({:error, {line, module, message}}, _) do
    puts("Line #{line}: syntax error.")
    message |> module.format_error() |> puts()
  end

  defp handle_parse({:ok, []}, _), do: puts("No Pantagruel source found.")

  defp handle_parse({:ok, parsed}, flags) do
    case Keyword.get(flags, :shell) do
      true ->
        Bool.Convert.convert(parsed)
        |> Bool.Slurp.slurp()
        |> Pantagruel.Shell.new_state()
        |> shell(flags)

      _ ->
        # Paths to additional .pant file hierarchies can be passed in with
        # the :path flag. The default path will also always be checked.
        [@default_path | Keyword.get_values(flags, :path)]
        |> Pantagruel.Load.load()
        |> handle_load(parsed, flags)
    end
  end

  defp handle_load({:ok, asts}, parsed, flags) do
    parsed
    |> Eval.eval(asts)
    |> handle_eval(parsed, flags)
  end

  defp handle_load({:error, _} = error, parsed, _) do
    handle_eval(error, parsed, [])
  end

  defp handle_eval({:ok, scope}, parsed, flags) do
    case Keyword.get(flags, :check) do
      true -> {:stop, nil}
      _ -> {:continue, {parsed, scope}}
    end
  end

  defp handle_eval({:error, e}, parsed, flags) do
    puts("Eval error.")

    case Keyword.get(flags, :check) do
      true ->
        display(e, flags)
        {:stop, nil}

      _ ->
        {:error, e, parsed}
    end
  end

  defp display({:stop, _}, _), do: puts("ok")

  defp display({:continue, {parsed, scope}}, flags) do
    formatter = flags[:formatter]

    if Keyword.get(flags, :env) do
      scope
      |> formatter.format_env()
      |> puts
    end

    parsed
    |> formatter.format_program()
    |> puts
  end

  defp display({:error, e, parsed}, flags) do
    display(e, flags)
    display({:continue, {parsed, %{}}}, flags)
  end

  defp display({:unbound_variables, unbounds, scopes}, flags) do
    formatter = flags[:formatter]

    puts("Unbound variables:")
    Enum.each(unbounds, &puts("- #{formatter.format_exp(&1, scopes)}"))

    unbounds
    |> Enum.filter(&match?({:quantification, _}, &1))
    |> handle_bad_bindings(flags)
  end

  defp display({:missing_import, mod_name}, _) do
    puts("Imported module could not be found: #{mod_name}")
  end

  defp display({:module_load, e}, _), do: handle_parse(e.error, %{})

  defp display({:module_shadow, %{mod_name: mod_name}}, _) do
    puts("Attempted to redefine defined module: #{mod_name}")
  end

  defp handle_bad_bindings([], _), do: :ok

  defp handle_bad_bindings(quantifiers, flags) do
    formatter = flags[:formatter]

    quantifiers
    |> Enum.each(fn {:quantification, [_, bindings, exp]} ->
      exp_str = {:quantification, ["…", bindings, exp]} |> formatter.format_exp([])

      """
      Expected binding form. Found:

      #{exp_str}

      Did you forget to wrap your symbols in ()?
      """
      |> puts
    end)
  end

  defp shell({tree, assumed, refuted} = state, flags) do
    formatter = flags[:formatter]
    lift(assumed, &formatter.format_exp/1) |> IO.inspect(label: :ASSUMED)
    lift(refuted, &formatter.format_exp/1) |> IO.inspect(label: :REFUTED)

    tree
    |> formatter.format_program()
    |> puts

    input = IO.gets("> ")

    state
    |> Pantagruel.Shell.handle(input)
    |> shell(flags)
  end
end
