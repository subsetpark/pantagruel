defmodule Logexian.Scan do
  defp scan(<<>>, out, _) do
    out
    |> String.reverse()
    |> String.trim()
  end

  ## Space/newline consolidation
  defp scan(<<?\s::utf8, contents::binary>>, <<?\s::utf8, _::binary>>=acc, true), do: scan(contents, acc, true)
  defp scan(<<?\s::utf8, contents::binary>>, <<?\n::utf8, _::binary>>=acc, true), do: scan(contents, acc, true)
  defp scan(<<?\n::utf8, contents::binary>>, <<?\n::utf8, _::binary>>=acc, true), do: scan(contents, acc, true)
  defp scan(<<?\n::utf8, contents::binary>>, <<?\s::utf8, rest::binary>>, true), do: scan(contents, <<?\n::utf8, rest::binary>>, true)
  ## Character replacement
  # These will probably want to write the the scan buffer, rather than the accumulator,
  # so that space consolidation can take a pass at them.
  defp scan(<<" ->"::utf8, contents::binary>>, out, true), do: scan(<<?→::utf8, contents::binary>>, out, true)
  defp scan(<<"->"::utf8, contents::binary>>, out, true), do: scan(<<?→::utf8, contents::binary>>, out, true)
  defp scan(<<"...\n"::utf8, contents::binary>>, out, true), do: scan(<<?\s::utf8, contents::binary>>, out, true)
  ## Space elimintation
  defp scan(<<?\s::utf8, contents::binary>>, <<"::"::utf8, _::binary>>=acc, true), do: scan(contents, acc, true)
  defp scan(<<" ::"::utf8, contents::binary>>, out, true), do: scan(contents, <<"::"::utf8, out::binary>>, true)
  defp scan(<<?\s::utf8, contents::binary>>, <<?:::utf8, _::binary>>=acc, true), do: scan(contents, acc, true)
  defp scan(<<" :"::utf8, contents::binary>>, out, true), do: scan(contents, <<?:::utf8, out::binary>>, true)
  defp scan(<<?\s::utf8, contents::binary>>, <<?|::utf8, _::binary>>=acc, true), do: scan(contents, acc, true)
  defp scan(<<" |"::utf8, contents::binary>>, out, true), do: scan(contents, <<?|::utf8, out::binary>>, true)
  defp scan(<<?\s::utf8, contents::binary>>, <<?=::utf8, _::binary>>=acc, true), do: scan(contents, acc, true)
  defp scan(<<" ="::utf8, contents::binary>>, out, true), do: scan(contents, <<?=::utf8, out::binary>>, true)
  defp scan(<<?\s::utf8, contents::binary>>, <<?→::utf8, _::binary>>=acc, true), do: scan(contents, acc, true)
  defp scan(<<" →"::utf8, contents::binary>>, out, true), do: scan(contents, <<?→::utf8, out::binary>>, true)
  ## Read-mode management
  defp scan(<<?\n::utf8, contents::binary>>, <<?\n::utf8, _::binary>>=acc, _), do: scan(contents, acc, true)
  defp scan(<<?\n::utf8, contents::binary>>, out, _), do: scan(contents, <<?\n::utf8, out::binary>>, true)
  defp scan(<<_::utf8, contents::binary>>, out, false), do: scan(contents, out, false)
  ## Special-case ';;' from triggering comments.
  defp scan(<<";;"::utf8, contents::binary>>, out, true), do: scan(contents, <<";;"::utf8, out::binary>>, true)
  defp scan(<<?;::utf8, contents::binary>>, out, _), do: scan(contents, out, false)
  defp scan(<<c::utf8, contents::binary>>, out, true), do: scan(contents, <<c::utf8, out::binary>>, true)

  def scan(contents) do
    scan(contents, <<>>, true)
  end
end
