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
      |> assoc_dots([])
      |> Enum.reverse()
      |> Enum.flat_map(&parse_dot_chain/1)
      |> Enum.reduce(&assoc/2)

    {[parsed], context}
  end

  # As a first pass, bind any dot expressions to the expression
  # immediately preceding.
  defp assoc_dots([], acc), do: Enum.reverse(acc)

  defp assoc_dots([<<"."::utf8, _::binary>> = e | rest], acc), do: assoc_dots(rest, acc, e)
  defp assoc_dots([e | rest], acc), do: assoc_dots(rest, [e | acc])

  defp assoc_dots([e | rest], acc, dot) do
    acc = [apply_f(dot, e) | acc]
    assoc_dots(rest, acc)
  end

  # Handle a single string representing chained dot-access, like
  # "foo.bar.baz".
  defp parse_dot_chain(v) when is_binary(v) do
    [head | tail] = String.split(v, ".", trim: true)

    dot = &{:dot, "." <> &1}

    head =
      case String.starts_with?(v, ".") do
        true -> dot.(head)
        false -> head
      end

    # Dot access binds more tightly; parse first and return as an
    # expression.
    [head | Enum.map(tail, dot)] |> Enum.reduce(&assoc/2) |> List.wrap()
  end

  defp parse_dot_chain(v), do: [v]

  # Handle infix binary operators.
  defp assoc(x, [appl, binary_operator: op]), do: apply_f(op, appl, x)
  defp assoc(x, appl) when x in @binary_operators, do: [appl, binary_operator: x]
  # Handle prefix unary operators.
  defp assoc(x, appl) when appl in @unary_operators, do: apply_f(appl, x, nil)
  # Handle postfix dot-access operator.
  defp assoc({:dot, x}, appl), do: apply_f(x, appl)
  # Handle normal prefix function application.
  defp assoc(x, appl), do: apply_f(appl, x)

  # Create function application structures.
  defp apply_f(f, x), do: {:appl, [f: f, x: x]}
  defp apply_f(operator, x, nil), do: {:appl, operator: operator, x: x}
  defp apply_f(operator, x, y), do: {:appl, operator: operator, x: x, y: y}
end
