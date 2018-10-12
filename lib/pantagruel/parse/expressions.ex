defmodule Pantagruel.Parse.Expressions do
  alias Pantagruel.Parse

  @moduledoc """
  This module encodes the function application binding rules in
  Pantagruel.
  """
  @binary_operators [
    :and,
    :or,
    :equals,
    :notequals,
    :gte,
    :lte,
    :gt,
    :lt,
    :in,
    :iff,
    :then,
    :from,
    :plus,
    :minus,
    :times,
    :divides,
    :exp,
    :insert,
    :xor
  ]

  @unary_operators [
    :not,
    :card
  ]

  @doc """
  Given an AST representing a single expression (potentially consisting
  of multiple words), build a function application tree.
  """
  @spec parse_function_application(
          binary(),
          Parse.t(),
          map,
          pos_integer,
          pos_integer
        ) :: Parse.t()
  def parse_function_application(_rest, expressions, context, _line, _offset) do
    parsed =
      expressions
      |> Enum.reverse()
      |> Enum.reduce(&assoc/2)

    {[parsed], context}
  end

  @spec parse_dot_expression(
          binary(),
          Parse.t(),
          map,
          pos_integer,
          pos_integer
        ) :: Parse.t()
  def parse_dot_expression(_rest, expressions, context, _line, _offset) do
    expressions =
      expressions
      |> Enum.reverse()
      |> Enum.reduce(&dot/2)

    {[expressions], context}
  end

  defp dot(f, x), do: {:dot, f: f, x: x}
  # Handle infix binary operators.
  defp assoc(x, [appl, binary_operator: op]), do: apply_f(op, appl, x)
  defp assoc(x, appl) when x in @binary_operators, do: [appl, binary_operator: x]
  # Handle prefix unary operators.
  defp assoc(x, appl) when appl in @unary_operators, do: apply_f(appl, x, nil)
  # Handle normal prefix function application.
  defp assoc(x, appl), do: apply_f(appl, x)

  # Create function application structures.
  defp apply_f(f, x), do: {:appl, [f: f, x: x]}
  defp apply_f(operator, x, nil), do: {:appl, operator: operator, x: x}
  defp apply_f(operator, x, y), do: {:appl, operator: operator, x: x, y: y}
end
