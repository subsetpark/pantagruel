defmodule Logexian.Scan do
  defp scan(<<>>, out, _) do
    out
    |> String.reverse()
    # |> IO.iodata_to_binary()
  end

  # Character replacement
  defp scan(<<?\s::utf8, contents::binary>>, <<?\s::utf8, _::binary>>=acc, true), do: scan(contents, acc, true)
  defp scan(<<"->"::utf8, contents::binary>>, out, true), do: scan(contents, <<?→::utf8, out::binary>>, true)
  # Read-mode management
  defp scan(<<?\n::utf8, contents::binary>>, out, _), do: scan(contents, <<?\n::utf8, out::binary>>, true)
  defp scan(<<_::utf8, contents::binary>>, out, false), do: scan(contents, out, false)
  # Special-case ';;' from triggering comments.
  defp scan(<<";;"::utf8, contents::binary>>, out, true), do: scan(contents, <<";;"::utf8, out::binary>>, true)
  defp scan(<<?;::utf8, contents::binary>>, out, _), do: scan(contents, out, false)
  defp scan(<<c::utf8, contents::binary>>, out, true), do: scan(contents, <<c::utf8, out::binary>>, true)

  def scan(contents) do
    scan(contents, <<>>, true)
  end
end
