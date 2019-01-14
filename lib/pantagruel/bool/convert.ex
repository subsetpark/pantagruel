defmodule Pantagruel.Bool.Convert do
  @moduledoc """
  Logic for lifting a Pantagruel AST into a Boolean algebra. Traverses a
  syntax tree, calling BoolAlg conversions on their equivalent Pantagruel
  nodes.
  """
  import BoolAlg
  alias Pantagruel.Parse.Node
  use Witchcraft

  @doc """
  Given an AST node, recurse and transform boolean operations into
  BoolAlg structs.
  """
  def convert(nil), do: nil
  def convert(list) when is_list(list), do: lift(list, &convert/1)
  def convert(value) when is_integer(value), do: value
  def convert(value) when is_atom(value), do: value
  def convert({:symbol, _} = s), do: s

  def convert(%Node{type: tag} = n) when tag in [:un_appl, :bin_appl] do
    n
    |> transform()
    |> lift(&convert/1)
  end

  def convert(%Node{} = n), do: lift(n, &convert/1)
  # Mappings between specific Pantagruel forms and their BoolAlg
  # equivalents.
  defp transform({_, [:or, x, y]}), do: disj(x, y)
  defp transform({_, [:and, x, y]}), do: conj(x, y)
  defp transform({_, [:"<->", x, y]}), do: iff(x, y)
  defp transform({_, [:->, x, y]}), do: impl(x, y)
  defp transform({_, [:"~", x]}), do: neg(x)
  defp transform(t), do: t
end
