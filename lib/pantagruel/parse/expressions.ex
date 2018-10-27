defmodule Pantagruel.Parse.Expressions do

  @moduledoc """
  This module contains all advanced parsing operations that have to be
  called by the parser.
  """
  @doc """
  Parse a string into a float.
  """
  def parse_float(_, [arg], context, _, _), do: {[String.to_float(arg)], context}

  def parse_integer(_, [arg], context, _, _) do
    {int, ""} =
      arg
      |> String.replace("_", "")
      |> Integer.parse()

    {[int], context}
  end

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
  @type parsed :: [term]

  @doc """
  Given a list of expressions representing function application, construct
  an AST.
  """
  @spec parse_function_application(
          binary(),
          parsed,
          map,
          pos_integer,
          pos_integer
        ) :: parsed
  def parse_function_application(_rest, expressions, context, _line, _offset),
    do: {expressions |> reduce(&assoc/2), context}

  @doc """
  Given a list of expressions representing an object/method-style function
  application, construct an AST.
  """
  @spec parse_dot_expression(
          binary(),
          parsed,
          map,
          pos_integer,
          pos_integer
        ) :: parsed
  def parse_dot_expression(_, expressions, context, _, _),
    do: {expressions |> reduce(&dot/2), context}

  defp reduce(expressions, fun) do
    expressions
    |> Enum.reverse()
    |> Enum.reduce(fun)
    |> List.wrap()
  end

  # Handle infix binary operators.
  defp assoc(x, [appl, binary_operator: op]), do: apply_f(op, appl, x)
  defp assoc(x, appl) when x in @binary_operators, do: [appl, binary_operator: x]
  # Handle normal prefix function application.
  defp assoc(x, appl), do: apply_f(appl, x)

  # Create function application structures.
  defp apply_f(f, x), do: {:appl, [f: f, x: x]}
  defp apply_f(operator, x, y), do: {:appl, operator: operator, x: x, y: y}

  defp dot(f, x), do: {:dot, f: f, x: x}
end
