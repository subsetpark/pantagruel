defmodule Pantagruel.Scan.Macros do
  @delimiters [?\n, ?\s, ?(, ?), ?[, ?], ?{, ?}]
  defguard are_delimiters(l, r) when l in @delimiters and r in @delimiters

  @doc """
  Replace `pattern` wherever it is found with `replace`.
  """
  defmacro defreplace(pattern, replace) do
    quote do
      defp replace_chars(<<unquote(pattern)::utf8, contents::binary>>, out, false),
        do: replace_chars(contents, <<unquote(replace)::utf8, out::binary>>, false)
    end
  end

  @doc """
  When `pattern` is surrounded by some delimiters, replace it with
  `replace`.

  Used to isolate keywords and complex operators so they're not caught
  in longer strings.
  """
  defmacro defreplacedelim(pattern, replace) do
    quote do
      defp replace_chars(
             <<l::utf8, unquote(pattern)::utf8, r::utf8, contents::binary>>,
             out,
             false
           )
           when are_delimiters(l, r),
           do:
             replace_chars(
               <<r::utf8, contents::binary>>,
               <<unquote(replace)::utf8, l::utf8, out::binary>>,
               false
             )
    end
  end
end

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
  import Pantagruel.Scan.Macros

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
  defreplace(";;", ";;")

  # Comment continuation.
  defp replace_chars(<<"\n;"::utf8, c::utf8, contents::binary>>, out, true) when c != ?; do
    out = <<c::utf8>> <> comment_continuation() <> <<out::binary>>
    replace_chars(contents, out, true)
  end

  # Start a new comment; insert a newline and turn on comment mode.
  defp replace_chars(<<";"::utf8, contents::binary>>, out, false),
    do: replace_chars(contents, <<";\n"::utf8, out::binary>>, true)

  defreplace("...\n", ?\s)
  defreplace("<->", "↔")
  defreplace("->", "→")
  defreplace("=>", "⇒")
  defreplace("<-", "←")
  defreplace("::", "∷")

  defreplacedelim("and", "∧")
  defreplacedelim("or", "∨")
  defreplacedelim("from", "∈")
  defreplacedelim("in", ":")
  defreplacedelim("exists", "∃")
  defreplacedelim("all", "∀")
  defreplacedelim("xor", "⊕")
  defreplacedelim(".", "⸳")

  # Turn off comment at a newline.
  defp replace_chars(<<"\n"::utf8, _::binary>> = comments, out, true),
    do: replace_chars(comments, out, false)

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

  @operators [?→, ?∃, ?∀, ?∧, ?∨, ?∀, ?∈, ?!, ?`, ?←, ?⸳, ?⇒, ?∷, ?↔]
  defguard is_operator(c)
           when c in ?(..?- or c in ?:..?= or c in ?[..?^ or c in ?{..?~ or c in @operators

  defp eliminate_spaces(<<>>, acc, _), do: String.reverse(acc)
  # Toggle comment mode and don't mess with spaces.
  defp eliminate_spaces(<<?;::utf8, contents::binary>>, out, false),
    do: eliminate_spaces(contents, <<?;::utf8, out::binary>>, true)

  # Exit comment mode.
  defp eliminate_spaces(<<?\n::utf8, contents::binary>>, out, true),
    do: eliminate_spaces(contents, <<?\n::utf8, out::binary>>, false)

  defp eliminate_spaces(<<?\s::utf8, contents::binary>>, <<c::utf8, _::binary>> = acc, false)
       when is_operator(c),
       do: eliminate_spaces(contents, acc, false)

  defp eliminate_spaces(<<" "::utf8, c::utf8, contents::binary>>, out, false) when is_operator(c),
    do: eliminate_spaces(contents, <<c::utf8, out::binary>>, false)

  defp eliminate_spaces(<<c::utf8, contents::binary>>, out, t),
    do: eliminate_spaces(contents, <<c::utf8, out::binary>>, t)
end
