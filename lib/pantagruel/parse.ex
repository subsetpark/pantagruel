defmodule Pantagruel.Parse do
  def handle_lex({:ok, tokens, _linecount}), do: :parser.parse(tokens)

end
