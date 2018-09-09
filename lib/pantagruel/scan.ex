defmodule Pantagruel.Scan do
  @moduledoc """
  Preprocessing for Pantagruel. Processes a text file in several passes:
  - Removes all commented areas.
  - Replaces specific character sequences with their unicode
    equivalents. This allows certain ascii combinations to be treated
    specially by the parser: arrows are combined into a single character,
    newline continuations are collapsed and removed, and a small set
    of identifier-legal characters, if encountered alone, are treated
    as keywords.
  - Unnecessary spaces and whitespace are removed.
  """

  defp replace_chars(<<>>, acc), do: String.reverse(acc)

  ## Special-case ';;' from triggering comments.
  defp replace_chars(<<";;"::utf8, contents::binary>>, out) do
    replace_chars(contents, <<";;"::utf8, out::binary>>)
  end

  defp replace_chars(<<";"::utf8, contents::binary>>, out) do
    replace_chars(contents, <<";\n"::utf8, out::binary>>)
  end

  defp replace_chars(<<"->"::utf8, contents::binary>>, out) do
    replace_chars(contents, <<?→::utf8, out::binary>>)
  end

  defp replace_chars(<<"<-"::utf8, contents::binary>>, out) do
    replace_chars(contents, <<?←::utf8, out::binary>>)
  end

  defp replace_chars(<<"...\n"::utf8, contents::binary>>, out) do
    replace_chars(contents, <<?\s::utf8, out::binary>>)
  end

  @delimiters [?\n, ?\s, ?(, ?), ?[, ?], ?{, ?}, ?"]
  # Unbreak syntax "

  defp replace_chars(<<l::utf8, "and"::utf8, r::utf8, contents::binary>>, out)
       when l in @delimiters and r in @delimiters do
    replace_chars(contents, <<r::utf8, "∧"::utf8, l::utf8, out::binary>>)
  end

  defp replace_chars(<<l::utf8, "or"::utf8, r::utf8, contents::binary>>, out)
       when l in @delimiters and r in @delimiters do
    replace_chars(contents, <<r::utf8, "∨"::utf8, l::utf8, out::binary>>)
  end

  defp replace_chars(<<l::utf8, "from"::utf8, r::utf8, contents::binary>>, out)
       when l in @delimiters and r in @delimiters do
    replace_chars(contents, <<r::utf8, "∈"::utf8, l::utf8, out::binary>>)
  end

  defp replace_chars(<<l::utf8, "in"::utf8, r::utf8, contents::binary>>, out)
       when l in @delimiters and r in @delimiters do
    replace_chars(contents, <<r::utf8, ":"::utf8, l::utf8, out::binary>>)
  end

  defp replace_chars(<<l::utf8, "exists"::utf8, r::utf8, contents::binary>>, out)
       when l in @delimiters and r in @delimiters do
    replace_chars(contents, <<r::utf8, "∃"::utf8, l::utf8, out::binary>>)
  end

  defp replace_chars(<<l::utf8, "all"::utf8, r::utf8, contents::binary>>, out)
       when l in @delimiters and r in @delimiters do
    replace_chars(contents, <<r::utf8, "∀"::utf8, l::utf8, out::binary>>)
  end

  defp replace_chars(<<l::utf8, "."::utf8, r::utf8, contents::binary>>, out)
       when l in @delimiters and r in @delimiters do
    replace_chars(contents, <<r::utf8, "⸳"::utf8, l::utf8, out::binary>>)
  end

  defp replace_chars(<<c::utf8, contents::binary>>, out) do
    replace_chars(contents, <<c::utf8, out::binary>>)
  end

  defp consolidate_whitespace(<<>>, acc), do: String.reverse(acc)

  defp consolidate_whitespace(<<?\s::utf8, contents::binary>>, <<?\s::utf8, _::binary>> = acc),
    do: consolidate_whitespace(contents, acc)

  defp consolidate_whitespace(<<?\s::utf8, contents::binary>>, <<?\n::utf8, _::binary>> = acc),
    do: consolidate_whitespace(contents, acc)

  defp consolidate_whitespace(<<?\n::utf8, contents::binary>>, <<?\n::utf8, _::binary>> = acc),
    do: consolidate_whitespace(contents, acc)

  defp consolidate_whitespace(<<?\n::utf8, contents::binary>>, <<?\s::utf8, rest::binary>>),
    do: consolidate_whitespace(contents, <<?\n::utf8, rest::binary>>)

  defp consolidate_whitespace(<<c::utf8, contents::binary>>, rest) do
    consolidate_whitespace(contents, <<c::utf8, rest::binary>>)
  end

  @operators [?→, ?∃, ?∀, ?∧, ?∨, ?∀, ?∈, ?¬, ?!, ?`, ?←, ?⸳]

  defp eliminate_spaces(<<>>, acc), do: String.reverse(acc)

  defp eliminate_spaces(<<?\s::utf8, contents::binary>>, <<c::utf8, _::binary>> = acc)
       # Excepting ?; doesn't seem necessary.
       when c in 40..45 or (c in 58..62 and c != ?;) or c in 91..94 or c in 123..126 or
              c in @operators do
    eliminate_spaces(contents, acc)
  end

  defp eliminate_spaces(<<" "::utf8, c::utf8, contents::binary>>, out)
       # Excepting ?; doesn't seem necessary.
       when c in 40..45 or (c in 58..62 and c != ?;) or c in 91..94 or c in 123..126 or
              c in @operators do
    eliminate_spaces(contents, <<c::utf8, out::binary>>)
  end

  defp eliminate_spaces(<<"=> "::utf8, contents::binary>>, out) do
    eliminate_spaces(contents, <<">="::utf8, out::binary>>)
  end

  defp eliminate_spaces(<<c::utf8, contents::binary>>, out) do
    eliminate_spaces(contents, <<c::utf8, out::binary>>)
  end

  @spec scan(String.t()) :: String.t()
  def scan(contents) do
    contents
    |> replace_chars(<<>>)
    |> consolidate_whitespace(<<>>)
    |> eliminate_spaces(<<>>)
    |> String.trim()
  end
end
