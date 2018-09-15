defmodule Pantagruel do
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
    File.read!(filename)
    |> Pantagruel.Scan.scan()
    |> Pantagruel.Parse.program()
  end

  def main(filename) do
    {:ok, parsed, "", %{}, _, _} = read!(filename)

    try do
      scope = Pantagruel.Eval.eval(parsed)

      Pantagruel.Format.format_program(parsed, scope)
      |> IO.puts()
    rescue
      e in Pantagruel.Env.UnboundVariablesError ->
        IO.puts("Eval error.\n")
        IO.puts("Unbound variables:")
        Enum.each(e.unbound, &IO.puts("- #{Pantagruel.Format.format_exp(&1, e.scopes)}"))

        Pantagruel.Format.format_program(parsed, for(_ <- parsed, do: []))
        |> IO.puts()
    end
  end
end
