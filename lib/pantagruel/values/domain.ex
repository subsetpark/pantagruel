defmodule Pantagruel.Values.Domain do
  @moduledoc """
  A domain in an evaluated Pantagruel program, with a name and whatever
  domain it is an alias for (or itself, otherwise).
  """
  import Pantagruel.Guards
  defstruct(name: "", ref: "")

  @doc """
  Generic domains can be introduced by prepending their name with an
  underscore. This distinction allows generics to be referred to as a
  part of a function definition without being defined first.
  """
  def is_generic?(domain), do: String.starts_with?(domain, "_")

  @doc """
  Flatten nested or composite domains to retrieve the basic domains they
  are composed of.
  """
  def flatten_domain({e, items}) when is_container(e), do: items
  def flatten_domain({:lambda, decl}), do: flatten_domain(decl[:lambda_doms])
  def flatten_domain(items) when is_list(items), do: items
  def flatten_domain(item), do: [item]
end
