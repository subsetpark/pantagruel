defmodule Pantagruel.Scan do
  @moduledoc """
  Preprocessing for Pantagruel. Prepares a raw text file for parsing.
  """
  @doc """
  Processes a text file in several passes:
    - Removes all commented areas.
    - Replaces specific character sequences with their unicode
      equivalents. This allows certain ascii combinations to be treated
      specially by the parser: arrows are combined into a single character,
      newline continuations are collapsed and removed, and a small set
      of identifier-legal characters, if encountered alone, are treated
      as keywords.
    - Unnecessary spaces and whitespace are removed.
  """
  @spec scan(String.t()) :: String.t()
  def scan(contents) do
    contents
    |> replace_chars(<<>>, false)
    |> consolidate_whitespace(<<>>)
    |> eliminate_spaces(<<>>, false)
    |> String.trim()
  end

  @doc """
  Magic unicode character inserted into comments to indicate where to
  split lines.
  """
  def comment_continuation, do: <<0xE0B0::utf8>>

  defp replace_chars(<<>>, acc, _), do: String.reverse(acc)

  ## Special-case ';;' from triggering comments.
  defp replace_chars(<<";;"::utf8, contents::binary>>, out, false),
    do: replace_chars(contents, <<";;"::utf8, out::binary>>, false)

  # Comment continuation.
  defp replace_chars(<<"\n;"::utf8, c::utf8, contents::binary>>, out, true)
       when c != ?; do
    replace_chars(
      contents,
      <<c::utf8>> <> comment_continuation() <> <<out::binary>>,
      true
    )
  end

  # Start a new comment; insert a newline and turn on comment mode.
  defp replace_chars(<<";"::utf8, contents::binary>>, out, false),
    do: replace_chars(contents, <<";\n"::utf8, out::binary>>, true)

  defp replace_chars(<<"->"::utf8, contents::binary>>, out, t),
    do: replace_chars(contents, <<?→::utf8, out::binary>>, t)

  defp replace_chars(<<"=>"::utf8, contents::binary>>, out, t),
    do: replace_chars(contents, <<?⇒::utf8, out::binary>>, t)

  defp replace_chars(<<"<-"::utf8, contents::binary>>, out, t),
    do: replace_chars(contents, <<?←::utf8, out::binary>>, t)

  defp replace_chars(<<"::"::utf8, contents::binary>>, out, t),
    do: replace_chars(contents, <<?∷::utf8, out::binary>>, t)

  defp replace_chars(<<"...\n"::utf8, contents::binary>>, out, t),
    do: replace_chars(contents, <<?\s::utf8, out::binary>>, t)

  @delimiters [?\n, ?\s, ?(, ?), ?[, ?], ?{, ?}, ?"]
  # Unbreak syntax "

  defp replace_chars(<<l::utf8, "and"::utf8, r::utf8, contents::binary>>, out, t)
       when l in @delimiters and r in @delimiters,
       do: replace_chars(contents, <<r::utf8, "∧"::utf8, l::utf8, out::binary>>, t)

  defp replace_chars(<<l::utf8, "or"::utf8, r::utf8, contents::binary>>, out, t)
       when l in @delimiters and r in @delimiters,
       do: replace_chars(contents, <<r::utf8, "∨"::utf8, l::utf8, out::binary>>, t)

  defp replace_chars(<<l::utf8, "from"::utf8, r::utf8, contents::binary>>, out, t)
       when l in @delimiters and r in @delimiters,
       do: replace_chars(contents, <<r::utf8, "∈"::utf8, l::utf8, out::binary>>, t)

  defp replace_chars(<<l::utf8, "in"::utf8, r::utf8, contents::binary>>, out, t)
       when l in @delimiters and r in @delimiters,
       do: replace_chars(contents, <<r::utf8, ":"::utf8, l::utf8, out::binary>>, t)

  defp replace_chars(<<l::utf8, "exists"::utf8, r::utf8, contents::binary>>, out, t)
       when l in @delimiters and r in @delimiters,
       do: replace_chars(contents, <<r::utf8, "∃"::utf8, l::utf8, out::binary>>, t)

  defp replace_chars(<<l::utf8, "all"::utf8, r::utf8, contents::binary>>, out, t)
       when l in @delimiters and r in @delimiters,
       do: replace_chars(contents, <<r::utf8, "∀"::utf8, l::utf8, out::binary>>, t)

  defp replace_chars(<<l::utf8, "."::utf8, r::utf8, contents::binary>>, out, t)
       when l in @delimiters and r in @delimiters,
       do: replace_chars(contents, <<r::utf8, "⸳"::utf8, l::utf8, out::binary>>, t)

  # Turn off comment at a newline.
  defp replace_chars(<<"\n"::utf8, contents::binary>>, out, _),
    do: replace_chars(contents, <<"\n"::utf8, out::binary>>, false)

  defp replace_chars(<<c::utf8, contents::binary>>, out, t),
    do: replace_chars(contents, <<c::utf8, out::binary>>, t)

  defp consolidate_whitespace(<<>>, acc), do: String.reverse(acc)

  defp consolidate_whitespace(<<?\s::utf8, contents::binary>>, <<?\s::utf8, _::binary>> = acc),
    do: consolidate_whitespace(contents, acc)

  defp consolidate_whitespace(<<?\s::utf8, contents::binary>>, <<?\n::utf8, _::binary>> = acc),
    do: consolidate_whitespace(contents, acc)

  defp consolidate_whitespace(<<?\n::utf8, contents::binary>>, <<?\n::utf8, _::binary>> = acc),
    do: consolidate_whitespace(contents, acc)

  defp consolidate_whitespace(<<?\n::utf8, contents::binary>>, <<?\s::utf8, rest::binary>>),
    do: consolidate_whitespace(contents, <<?\n::utf8, rest::binary>>)

  defp consolidate_whitespace(<<c::utf8, contents::binary>>, rest),
    do: consolidate_whitespace(contents, <<c::utf8, rest::binary>>)

  @operators [?→, ?∃, ?∀, ?∧, ?∨, ?∀, ?∈, ?¬, ?!, ?`, ?←, ?⸳, ?⇒, ?∷]

  defp eliminate_spaces(<<>>, acc, _), do: String.reverse(acc)
  # Toggle comment mode and don't mess with spaces.
  defp eliminate_spaces(<<?;::utf8, contents::binary>>, out, false),
    do: eliminate_spaces(contents, <<?;::utf8, out::binary>>, true)

  # Exit comment mode.
  defp eliminate_spaces(<<?\n::utf8, contents::binary>>, out, true),
    do: eliminate_spaces(contents, <<?\n::utf8, out::binary>>, false)

  defp eliminate_spaces(<<?\s::utf8, contents::binary>>, <<c::utf8, _::binary>> = acc, false)
       when c in ?(..?- or c in ?:..?= or c in ?[..?^ or c in ?{..?~ or c in @operators,
       do: eliminate_spaces(contents, acc, false)

  defp eliminate_spaces(<<" "::utf8, c::utf8, contents::binary>>, out, false)
       when c in ?(..?- or c in ?:..?= or c in ?[..?^ or c in ?{..?~ or c in @operators,
       do: eliminate_spaces(contents, <<c::utf8, out::binary>>, false)

  defp eliminate_spaces(<<c::utf8, contents::binary>>, out, t),
    do: eliminate_spaces(contents, <<c::utf8, out::binary>>, t)
end
