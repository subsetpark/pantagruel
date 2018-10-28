defmodule Mix.Tasks.Pantagruel.Eval do
  use Mix.Task
  alias Pantagruel.Eval

  def eval(filename) do
    {:ok, parsed, "", %{}, _, _} =
      filename
      |> Mix.Tasks.Pantagruel.Parse.parse()

    Eval.eval(parsed, [])
  end

  def run([filename]) do
    case eval(filename) do
      {:ok, r} ->
        IO.puts("Resulting environment:")
        IO.inspect(r)

      {:error, {:unbound_variables, e}} ->
        IO.puts(e.message)
        IO.inspect(e.unbound)
        exit("Invalid program.")

      {:error, {:domain_mismatch, e}} ->
        IO.puts(e.message)
        IO.inspect(e.args, label: :arguments)
        IO.inspect(e.doms, label: :domains)
        exit("Invalid program.")
    end
  end
end
