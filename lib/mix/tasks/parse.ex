defmodule Mix.Tasks.Pantagruel.Parse do
  use Mix.Task

  def run([filename]) do
    parsed =
      try do
        filename
        |> Pantagruel.read!()
      rescue
        e in Pantagruel.Eval.State.UnboundVariablesError ->
          IO.puts(e.message)
          IO.inspect(e.unbound)
          exit("Invalid program.")
      end

    case parsed do
      {r, _, _} ->
        IO.puts("Resulting environment:")
        IO.inspect(r)

      other ->
        IO.puts("Parsing complete.")
        IO.inspect(other)
    end
  end
end

defmodule Mix.Tasks.Pantagruel.Scan do
  use Mix.Task

  def run([filename]) do
    scanned =
      filename
      |> File.read!()
      |> Pantagruel.Scan.scan()

    IO.inspect(scanned)
  end
end
