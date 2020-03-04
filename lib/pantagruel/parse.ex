defmodule Pantagruel.Parse do
  @type t :: {:program, list}

  def handle_lex({:error, e, _}), do: {:error, e}
  def handle_lex({:ok, tokens, _linecount}), do: :pant_parser.parse(tokens)
end
