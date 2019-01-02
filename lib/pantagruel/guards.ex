defmodule Pantagruel.Guards do
  defguard is_container(c) when c in [:par, :list, :set]
  defguard is_relation(r) when r in [:conj, :disj, :impl, :iff, :xor]
end

