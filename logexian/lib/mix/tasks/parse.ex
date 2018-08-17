defmodule Mix.Tasks.Logexian.Parse do
  use Mix.Task

  def run([filename]) do
    parsed =
      filename
      |> Logexian.read!()

    case parsed do
      {:ok, prog, "", _, _, _} ->
        IO.inspect(prog)

      {:ok, prog, remaining, _, _, _} ->
        IO.inspect(prog)
        IO.puts("File not fully parsed!")
        IO.puts(remaining)
        IO.puts("---")

      r ->
        IO.puts("Parse error!")
        IO.inspect(r)
    end
  end
end

defmodule Mix.Tasks.Logexian.Scan do
  use Mix.Task

  def run([filename]) do
    scanned =
      filename
      |> File.read!()
      |> Logexian.Scan.scan()

    IO.inspect(scanned)
  end
end
