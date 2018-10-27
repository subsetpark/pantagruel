defmodule Mix.Tasks.Pantagruel.Scan do
  use Mix.Task

  def scan(filename) do
    filename
    |> File.read!()
    |> Pantagruel.Scan.scan()
  end

  def run([filename]) do
    scanned = scan(filename)
    IO.inspect(scanned)
  end
end
