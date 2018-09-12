defmodule Pantagruel do
  def read!(filename) do
    File.read!(filename)
    |> Pantagruel.Scan.scan()
    |> Pantagruel.Parse.program()
  end

  def main(filename) do
    try do
      {:ok, parsed, "", %{}, _, _} = read!(filename)
      scope = Pantagruel.Eval.eval(parsed)

      Pantagruel.Print.print_program(parsed, scope)
      |> IO.puts()
    rescue
      e in Pantagruel.Env.UnboundVariablesError ->
        IO.puts("Unbound variables:")
        Enum.each(e.unbound, &IO.puts("- #{Pantagruel.Print.print_subexp(&1)}"))
    end
  end
end
