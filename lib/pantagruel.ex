defmodule Pantagruel do
  def read!(filename) do
    File.read!(filename)
    |> Pantagruel.Scan.scan()
    |> Pantagruel.Parse.program()
  end

  def main(filename) do
    {:ok, parsed, "", %{}, _, _} = read!(filename)

    try do
      scope = Pantagruel.Eval.eval(parsed)

      Pantagruel.Print.print_program(parsed, scope)
      |> IO.puts()
    rescue
      e in Pantagruel.Env.UnboundVariablesError ->
        IO.puts("Eval error.\n")
        IO.puts("Unbound variables:")
        Enum.each(e.unbound, &IO.puts("- #{Pantagruel.Print.print_subexp(&1, e.scopes)}"))

        Pantagruel.Print.print_program(parsed, for(_ <- parsed, do: []))
        |> IO.puts()
    end
  end
end
