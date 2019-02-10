defmodule Pantagruel.Scan do
  @moduledoc """
  String processing to prepare for tokenizing.
  """
  def scan(binary) do
    binary
    |> String.trim()
    |> scan([])
  end

  defp scan(<<>>, acc), do: Enum.reverse(acc)
  defp scan(<<last::utf8>>, acc), do: scan(<<>>, [?\n, last | acc])
  defp scan(<<c::utf8, rest::binary>>, acc), do: scan(rest, [c | acc])
end
