defmodule Pantagruel.Guards do
  defguard is_container(c) when c in [:par, :list, :set]
end

