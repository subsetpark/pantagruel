defmodule Logexian.Scan do
  defp scan(<<>>, out, _) do
    out
    |> Enum.reverse()
    |> IO.iodata_to_binary()
  end

  defp scan(<<?\s, contents::binary>>, [?\s | out], true), do: scan(contents, [?\s | out], true)
  defp scan(<<?\n, contents::binary>>, out, _), do: scan(contents, [?\n | out], true)
  defp scan(<<_::utf8, contents::binary>>, out, false), do: scan(contents, out, false)
  defp scan(<<";;", contents::binary>>, out, true), do: scan(contents, [";;" | out], true)
  defp scan(<<?;, contents::binary>>, out, _), do: scan(contents, out, false)
  defp scan(<<c::utf8, contents::binary>>, out, true), do: scan(contents, [c | out], true)

  def scan(contents) do
    scan(contents, [], true)
  end
end
