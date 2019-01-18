defmodule Pantagruel.Macros do
  defmacro sym(s), do: {:symbol, s}
  defmacro exp(op, e), do: {:expr, [op, e]}
end
