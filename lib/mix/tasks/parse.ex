defmodule Mix.Tasks.Pantagruel.Parse do
  use Mix.Task

  def run([filename]) do
    parsed =
      filename
      |> Pantagruel.read!()

    case parsed do
      {:ok, prog, "", _, _, _} ->
        IO.inspect(prog)

      {:ok, prog, remaining, _, _, _} ->
        IO.inspect(prog)
        IO.puts("File not fully parsed!")
        IO.puts(remaining)
        IO.puts("---")

      %{} = r ->
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
