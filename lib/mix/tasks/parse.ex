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
      {:ok, prog, "", _, _, _} ->
        IO.inspect(prog)

      {:ok, prog, remaining, _, _, _} ->
        IO.inspect(prog)
        IO.puts("File not fully parsed!")
        IO.puts(remaining)
        IO.puts("---")

      {r, _, _} ->
        IO.puts("Resulting environment:")
        IO.inspect(r)
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
