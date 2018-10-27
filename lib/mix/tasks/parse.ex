defmodule Mix.Tasks.Pantagruel.Parse do
  use Mix.Task

  def parse(filename) do
    Mix.Tasks.Pantagruel.Scan.scan(filename)
    |> Pantagruel.Parse.program()
  end

  def run([filename]) do
    {:ok, parsed, rest, %{}, _, _} =
      filename
      |> parse()

    IO.inspect(parsed, label: :ast)
    IO.inspect(rest, label: :rest)
  end
end
