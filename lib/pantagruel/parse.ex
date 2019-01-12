defmodule Pantagruel.Parse do
  def handle_lex({:ok, tokens, _linecount}), do: :pant_parser.parse(tokens)

end
