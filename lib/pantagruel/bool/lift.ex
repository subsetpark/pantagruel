defmodule Pantagruel.Bool.Lift do
  import Kernel, except: [&&: 2, ||: 2]
  import Pantagruel.Guards
  import BoolAlg

  def lift(tree) do
    tree[:chapters]
    |> Enum.map(&lift_chapter/1)
  end

  defp lift_chapter({:chapter, chapter}) do
    Kernel.||(chapter[:body], [])
    |> Enum.reduce(true, &slurp/2)
    |> reduce()
  end

  defp slurp([{:intro_op, :or} | rest], x), do: x || lift_statement(rest)
  defp slurp([{:intro_op, _} | rest], x), do: x && lift_statement(rest)
  defp slurp({:comment, _}, x), do: x
  defp slurp(y, x), do: x && lift_statement(y)

  defp lift_statement(expr: expr), do: lift_expr(expr)
  defp lift_statement(statement), do: statement

  defp lift_expr({:appl, op: op, x: x, y: y}) do
    bool_map(op, lift_expr(x), lift_expr(y))
  end

  defp lift_expr({:appl, op: op, x: x}) do
    bool_map(op, lift_expr(x))
  end

  defp lift_expr({:appl, f: f, x: x}) do
    {:appl, f: lift_expr(f), x: lift_expr(x)}
  end

  defp lift_expr({c, exprs}) when is_container(c) do
    {c, Enum.map(exprs, &lift_expr/1)}
  end

  defp lift_expr({:dot, f: f, x: x}) do
    {:dot, f: lift_expr(f), x: lift_expr(x)}
  end

  defp lift_expr({:quantification, quantifier: quant, bindings: bindings, expr: expr}) do
    {:quantification,
     quantifier: quant, bindings: Enum.map(bindings, &lift_expr/1), expr: lift_expr(expr)}
  end

  defp lift_expr({:comprehension, bindings: bindings, expr: expr}) do
    {:comprehension, bindings: Enum.map(bindings, &lift_expr/1), expr: lift_expr(expr)}
  end

  defp lift_expr({:binding, bind_symbol: sym, bind_domain: dom}) do
    {:binding, bind_symbol: sym, bind_domain: lift_expr(dom)}
  end

  # TODO: Lift lambda predicates?
  defp lift_expr({:lambda, lambda}), do: lambda

  defp lift_expr({:symbol, _} = s), do: s
  defp lift_expr({:literal, _} = l), do: l
  defp lift_expr(n) when is_integer(n), do: n

  defp bool_map(:or, x, y), do: {:disj, x, y}
  defp bool_map(:and, x, y), do: {:conj, x, y}
  defp bool_map(:"<->", x, y), do: {:iff, x, y}
  defp bool_map(:->, x, y), do: {:impl, x, y}
  defp bool_map(o, x, y), do: {:appl, op: o, x: x, y: y}
  defp bool_map(:"~", x), do: {:not, x}
  defp bool_map(o, x), do: {:appl, op: o, x: x}
end
