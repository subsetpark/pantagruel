defmodule BoolAlg do
  @moduledoc """
  Boolean Algebra modelling. Implementing simple boolean logic as a tree
  of operations, implement term replacement and reduction over the tree.
  """
  import BoolAlg.Macros
  use Witchcraft

  defstruct op: nil, x: nil, y: nil

  @doc """
  Conjunction.
  """
  def conj(x, y), do: %__MODULE__{op: :conj, x: x, y: y}
  def x && y, do: conj(x, y)

  @doc """
  Disjunction.
  """
  def disj(x, y), do: %__MODULE__{op: :disj, x: x, y: y}
  def x || y, do: disj(x, y)

  @doc """
  Implication.
  """
  def impl(x, y), do: %__MODULE__{op: :impl, x: x, y: y}
  def x ~> y, do: impl(x, y)

  @doc """
  Mutual implication.
  """
  def iff(x, y), do: %__MODULE__{op: :iff, x: x, y: y}
  def x <~> y, do: iff(x, y)

  @doc """
  Exclusive-or.
  """
  def xor(x, y), do: %__MODULE__{op: :xor, x: x, y: y}
  def x <|> y, do: xor(x, y)

  @doc """
  Negation.
  """
  def neg(x), do: %__MODULE__{op: :not, x: x}

  @doc """
  Term rewriting by replacement with `true` and reduction.
  """
  def assert(t, p), do: replace(t, p, true) |> reduce_all()

  @doc """
  Term rewriting by replacement with `false` and reduction.
  """
  def refute(t, p), do: replace(t, p, false) |> reduce_all()

  @doc """
  Reduce a term until it can be reduced no further.
  """
  def reduce_all(t) do
    case reduce(t) do
      ^t -> t
      reduced -> reduce_all(reduced)
    end
  end

  # Term rewriting rules; given the presence of `true` or `false` in
  # a boolean operation, the term can be eliminated and the overall
  # operation simplified by reduction.
  deftrue(:conj, q)
  deftrue(:disj, true)
  deftrue(:iff, q)
  deftrue(:xor, neg(q))
  deftrue(:impl, q, true)
  deftrue(:not, false)
  deffalse(:conj, false)
  deffalse(:disj, q)
  deffalse(:iff, neg(q))
  deffalse(:xor, q)
  deffalse(:impl, true, neg(q))
  deffalse(:not, true)

  defp reduce(%BoolAlg{} = b), do: lift(b, &reduce/1)

  defp reduce({:cont, [:par, [val]]}), do: val

  defp reduce(t) when is_list(t), do: lift(t, &reduce/1)

  defp reduce({tag, values}) when is_list(values), do: {tag, lift(values, &reduce/1)}
  defp reduce(t), do: t

  defp replace(p, p, token), do: token
  defp replace({tag, values}, p, token), do: {tag, lift(values, &replace(&1, p, token))}
  defp replace(q, p, token) when is_list(q), do: lift(q, &replace(&1, p, token))
  defp replace(%BoolAlg{} = b, p, token), do: lift(b, &replace(&1, p, token))
  defp replace(q, _, _), do: q
end
