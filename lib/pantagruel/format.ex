defmodule Pantagruel.Format do
  @moduledoc """
  Takes an evaluated Pantagruel program and generates a formatted text
  representation of it.
  """
  import Pantagruel.Guards
  alias Pantagruel.Values.{Domain, Variable, Lambda}
  alias Pantagruel.Env

  @type ast :: [term]
  @type t :: String.t()
  @doc """
  Generate a string representation of an evaluated program.
  """
  @spec format_program(ast) :: t
  def format_program(parsed), do: format_with(parsed, &format_section/1)

  @spec format_scopes(Env.t()) :: t
  def format_scopes(scopes), do: format_with(scopes, &format_scope/1)

  defp format_with(data, f) do
    bar = "***"

    data
    |> Enum.map(f)
    |> Enum.join("\n\n#{bar}\n\n")
  end

  @doc """
  Generate a string representation of a parsed program section.
  """
  def format_section({:section, section}) do
    Stream.concat(section[:head], section[:body] || [])
    # For now, assume we want markdown compatibility.
    |> Stream.map(&(format_line(&1) <> "  "))
    |> Enum.join("\n")
  end

  defguard is_symbol(s) when is_binary(s) or is_number(s) or is_atom(s)

  @doc """
  Print an individual expression.
  """
  @spec format_exp(any, [%{}]) :: t
  def format_exp(value, scope \\ [])

  def format_exp(%Domain{name: name, ref: ref}, scope) do
    "#{format_exp(name, scope)} ⇐ #{format_exp(ref, scope)}"
  end

  def format_exp(%Variable{name: name, domain: domain}, scope) do
    "#{format_exp(name, scope)} : #{format_exp(domain, scope)}"
  end

  def format_exp(s, []) when is_binary(s),
    do: Env.lookup_binding_name(s) |> String.replace("_", "-")

  def format_exp(s, []) when is_symbol(s), do: Env.lookup_binding_name(s)

  def format_exp(s, scopes) when is_symbol(s) do
    name = Env.lookup_binding_name(s)

    cond do
      Env.is_bound?(s, scopes) -> name
      true -> "*#{name}*"
    end
  end

  def format_exp({c, exps}, s) when is_container(c) do
    {l, r} = delimiters(c)
    inner_str = exp_join(exps, s)

    [l, inner_str, r]
    |> Enum.join()
  end

  def format_exp(
        {:quantification, quantifier: op, quant_bindings: binding, quant_expression: expr},
        s
      ) do
    [
      format_exp(op, s),
      exp_join(binding, s),
      "⸳",
      format_exp(expr, s)
    ]
    |> Enum.join(" ")
  end

  def format_exp(
        {:comprehension, [{container, [comp_bindings: bindings, comp_expression: expr]}]},
        s
      ) do
    binding_str = exp_join(bindings, s)
    expr_str = format_exp(expr)
    inner_str = "#{binding_str} ⸳ #{expr_str}"
    format_exp({container, [inner_str]}, s)
  end

  def format_exp({:binding, bind_symbol: symbol, bind_op: op, bind_domain: domain}, s) do
    "#{format_exp(symbol, s)} #{format_exp(op, s)} #{format_exp(domain, s)}"
  end

  def format_exp({:guard, expr}, s), do: format_exp(expr, s)
  def format_exp({:lambda, l}, s), do: format_lambda(l, scope: s)
  def format_exp(%Lambda{} = l, s), do: format_lambda(l, scope: s)
  def format_exp({:intro_op, op}, s), do: format_exp(op, s)
  def format_exp({:literal, literal}, _), do: "*#{literal}*"

  def format_exp({:refinement, refinement}, s) do
    right_str = format_exp(refinement[:expr], s)

    guard_str =
      refinement[:guard]
      |> format_guard(s)

    "#{format_exp(refinement[:pattern], s)}#{guard_str} ← #{right_str}"
  end

  def format_exp({:appl, operator: op, x: x, y: y}, s) do
    "#{format_exp(x, s)} #{format_exp(op, s)} #{format_exp(y, s)}"
  end

  def format_exp({:unary_exp, op: op, operand: x}, s) do
    "#{format_exp(op, s)}#{format_exp(x, s)}"
  end

  def format_exp({:appl, f: f, x: x}, s) do
    "#{format_exp(f, s)} #{format_exp(x, s)}"
  end

  def format_exp({:dot, f: f, x: x}, s) do
    "#{format_exp(x, s)}.#{format_exp(f, s)}"
  end

  def format_exp(exp, s) do
    exp
    |> Enum.map(&format_exp(&1, s))
    |> Enum.join(" ")
  end

  defp delimiters(:par), do: {"(", ")"}
  defp delimiters(:list), do: {"[", "]"}
  defp delimiters(:set), do: {"{", "}"}

  defp format_line({:decl, declaration}), do: format_lambda(declaration, decl: declaration)
  defp format_line({:comment, comment}), do: format_comment(comment)
  defp format_line({:expr, expression}), do: format_exp(expression)

  defp format_line({:alias, [alias_name: names, alias_expr: ref]}) do
    alias_names =
      Enum.map(names, &format_exp/1)
      |> Enum.join(", ")

    "#{alias_names} ⇐ #{format_exp(ref)}"
  end

  # Print the contents of the environment after program evaluation.
  defp format_scope(scope) do
    sort = fn
      %Domain{}, %Lambda{} -> true
      %Domain{}, %Variable{} -> true
      %Lambda{}, %Variable{} -> true
      %Lambda{type: :constructor}, %Lambda{type: :function} -> true
      %Lambda{type: :function}, %Lambda{type: :constructor} -> false
      %Lambda{type: t}, %Lambda{type: nil} when not is_nil(t) -> true
      %Domain{ref: a}, %Domain{ref: b} -> a <= b
      %{__struct__: t, name: a}, %{__struct__: t, name: b} -> a <= b
      _, _ -> false
    end

    scope |> Map.values() |> Enum.sort(sort) |> Enum.map(&format_exp/1) |> Enum.join("\n")
  end

  @spec format_lambda(any, keyword) :: t
  defp format_lambda(
         %Lambda{name: name, domain: domain, codomain: codomain, type: type},
         opts
       ) do
    scope = opts[:scope] || []

    prefix = lambda_prefix(name, scope)
    args = lambda_args(opts[:decl][:lambda_args], scope)
    dom = exp_join(domain, scope)
    pred = lambda_predicate(opts[:decl][:predicate], scope)
    yields = lambda_yields(type)
    codom = format_exp(codomain, scope)

    [prefix, "(", args, dom, pred, ")", yields, codom]
    |> Enum.join()
  end

  defp format_lambda(l, opts) do
    Lambda.from_declaration(l) |> format_lambda(opts)
  end

  defp lambda_prefix(nil, _), do: "λ"
  defp lambda_prefix(name, s), do: name |> format_exp(s)

  defp lambda_args(nil, _), do: ""
  defp lambda_args(args, s), do: exp_join(args, s) <> ":"

  defp lambda_predicate(nil, _), do: ""
  defp lambda_predicate(expr, s), do: " ⸳ #{exp_join(expr, s)}"

  defp lambda_yields(nil), do: ""
  defp lambda_yields(:function), do: " ∷ "
  defp lambda_yields(:constructor), do: " ⇒ "

  defp format_comment([comment]) do
    comment_str =
      comment
      |> String.split(Pantagruel.Scan.comment_continuation())
      |> Enum.map(&String.trim/1)
      |> Enum.join("\n> ")

    "\n> " <> comment_str <> "\n"
  end

  defp format_guard("", _), do: nil
  defp format_guard(guard, s), do: " ⸳ #{format_exp(guard, s)}"

  defp exp_join(exprs, s) do
    exprs
    |> Enum.map(&format_exp(&1, s))
    |> Enum.join(", ")
  end
end
