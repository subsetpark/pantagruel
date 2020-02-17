defmodule Pantagruel.Values.Domain do
  alias Pantagruel.Env
  import Pantagruel.Macros

  @moduledoc """
  A domain in an evaluated Pantagruel program, with a name and whatever
  domain it is an alias for (or itself, otherwise).
  """
  defstruct(name: "", ref: "")

  @doc """
  Generic domains can be introduced by prepending their name with an
  apostrophe. This distinction allows generics to be referred to as a
  part of a function definition without being defined first.
  """
  def is_generic?(sym([?\' | _])), do: true
  def is_generic?(sym(_)), do: false

  @doc """
  Flatten nested or composite domains to retrieve the basic domains they
  are composed of.
  """
  def flatten_domain({:cont, [_, items]}), do: items

  def flatten_domain({:lambda, [bindings, _, _]}) do
    {_, doms} = Env.args_and_domains(bindings)

    doms
    |> flatten_domain()
  end

  def flatten_domain(items) when is_list(items), do: items
  def flatten_domain(item), do: [item]
end
