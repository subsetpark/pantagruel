defmodule Pantagruel.Parse.Util do
  @moduledoc """
  Combinator snippets required by the Pantagruel parser.
  """
  import NimbleParsec

  @doc """
  Given a combinator C and a joiner J, match on
  CJCJ ... C
  """
  def join(c, joiner) do
    c
    |> repeat(
      ignore(joiner)
      |> concat(c)
    )
  end

  def comma_join(c), do: join(c, string(","))
  def newline_join(c), do: join(c, string("\n"))
  def space_join(c), do: join(c, string(" "))

  @doc """
  Convert a list of strings to string combinators. Also accepts {string,
  value} pairs and will concatenate a replace combinator to the recognized
  string.
  """
  @spec strings([String.t()] | [{String.t(), atom}]) :: [NimbleParsec.t()]
  def strings(ss) do
    f = fn
      {s, r} ->
        s
        |> string
        |> replace(r)

      s ->
        string(s)
    end

    Enum.map(ss, f)
  end

  @doc """
  Parse combinators nested inside of matched delimiters.
  """
  def nested(c) do
    choice(
      for {l, r, name} <- [
            {"(", ")", :par},
            {"[", "]", :list},
            {"{", "}", :set}
          ],
          do:
            ignore(string(l))
            |> concat(c)
            |> ignore(string(r))
            |> tag(name)
    )
  end
end
