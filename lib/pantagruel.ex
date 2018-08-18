defmodule Pantagruel do
  def read!(filename) do
    File.read!(filename)
    |> Pantagruel.Scan.scan
    |> Pantagruel.Parse.program
  end
end
