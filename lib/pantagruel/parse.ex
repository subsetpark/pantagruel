defmodule Pantagruel.Parse do
  def program(tokens) do
    :parser.parse(tokens)
  end
end
