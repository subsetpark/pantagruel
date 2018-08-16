defmodule Logexian do
  def read!(filename) do
    File.read!(filename)
    |> Logexian.Scan.scan
    |> Logexian.Parse.program
  end
end
