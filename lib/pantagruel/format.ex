defmodule Pantagruel.Format do
  @moduledoc """
  Takes an evaluated Pantagruel program and generates a formatted text
  representation of it.
  """
  import Pantagruel.Guards
  alias Pantagruel.Values.{Domain, Variable, Lambda}
  alias Pantagruel.Env
  alias Pantagruel.Eval.Module

  @type ast :: [term]
  @type t :: String.t()

  @doc """
  Generate a string representation of an evaluated program.
  """
  @spec format_program(ast) :: t
  def format_program(parsed), do: format_with(parsed, &format_section/1)

  @spec format_scopes(Env.t()) :: t
  def format_scopes(scopes), do: format_with(scopes, &format_scope/1)

  @bar "***"

  defp format_with(data, f) do
    data
    |> Enum.map(f)
    |> Enum.join("\n\n#{@bar}\n\n")
  end

  @doc """
  Generate a string representation of a parsed program section.
  """
  def format_section({:import, mod_name}), do: ": #{mod_name}"
  def format_section({:module, mod_name}), do: "# #{mod_name}"

  def format_section({:chapters, chapters}) do
    chapters
    |> Enum.map(&format_chapter/1)
  end

  defguard is_term(s) when is_number(s) or is_atom(s) or is_binary(s)

  @doc """
  Format an individual expression.
  """
  @spec format_exp(any, [%{}]) :: t
  def format_exp(value, scope \\ [])
  def format_exp(%Module{name: n}, _), do: "# #{n}"
  def format_exp(%Domain{name: n, ref: ref}, s), do: join_exp([n, "⇐", ref], s, " ")
  def format_exp(%Variable{name: n, domain: dom}, s), do: join_exp([n, ":", dom], s, " ")
  def format_exp({:symbol, _} = s, []), do: format_symbol(s)
  def format_exp(s, []) when is_term(s), do: Env.lookup_binding_name(s)
  def format_exp({:symbol, _} = s, scopes), do: format_symbol(s, scopes)
  def format_exp(s, scopes) when is_term(s), do: format_symbol(s, scopes)
  def format_exp({c, exps}, s) when is_container(c), do: format_container(c, exps, s)
  def format_exp({:quantification, q}, s), do: format_quantification(q, s)
  def format_exp({:comprehension, [{container, c}]}, s), do: format_comprehension(container, c, s)
  def format_exp({:binding, binding}, s), do: format_binding(binding, s)
  def format_exp({:guard, expr}, s), do: format_exp(expr, s)
  def format_exp({:lambda, l}, s), do: format_lambda(l, scope: s)
  def format_exp(%Lambda{} = l, s), do: format_lambda(l, scope: s)
  def format_exp({:intro_op, op}, s), do: format_exp(op, s)
  def format_exp({:literal, literal}, _), do: "*#{literal}*"
  def format_exp({:appl, op: op, x: x, y: y}, s), do: join_exp([x, op, y], s, " ")
  def format_exp({:appl, op: op, x: x}, s), do: join_exp([op, x], s)
  def format_exp({:appl, f: f, x: x}, s), do: join_exp([f, x], s, " ")
  def format_exp({:dot, f: f, x: x}, s), do: join_exp([x, f], s, ".")
  def format_exp(exp, s), do: join_exp(exp, s, " ")

  defp format_chapter({:chapter, chapter}) do
    Stream.concat(chapter[:head], chapter[:body] || [])
    # For now, assume we want markdown compatibility.
    |> Stream.map(&(format_line(&1) <> "  "))
    |> Enum.join("\n")
  end

  # Format the contents of the environment after program evaluation.
  defp format_scope(scope) do
    sort = fn
      %Module{}, _ -> true
      %Domain{}, %Lambda{} -> true
      %Domain{}, %Variable{} -> true
      %Lambda{}, %Variable{} -> true
      %Lambda{type: '=>'}, %Lambda{type: '::'} -> true
      %Lambda{type: '::'}, %Lambda{type: '=>'} -> false
      %Lambda{type: t}, %Lambda{type: nil} when not is_nil(t) -> true
      %Domain{ref: a}, %Domain{ref: b} -> a <= b
      %{__struct__: t, name: a}, %{__struct__: t, name: b} -> a <= b
      _, _ -> false
    end

    scope
    |> Map.values()
    |> Enum.filter(fn
      %Domain{name: name, ref: ref} -> ref != name
      _ -> true
    end)
    |> Enum.sort(sort)
    |> Enum.map(&format_exp/1)
    |> Enum.join("\n")
  end

  defp join_exp(exps, s, sep \\ "") do
    exps
    |> Enum.map(&format_exp(&1, s))
    |> Enum.join(sep)
  end

  defp format_symbol(s) do
    Env.lookup_binding_name(s)
    |> String.replace("_", "-")
  end

  defp format_line({:decl, declaration}), do: format_lambda(declaration, decl: declaration)
  defp format_line({:comment, comment}), do: format_comment(comment)
  defp format_line({:expr, expression}), do: format_exp(expression)
  defp format_line({:alias, alias}), do: format_alias(alias)

  defp format_line({:refinement, refinement}), do: format_refinement(refinement)

  defp format_refinement(refinement) do
    pat = refinement[:pattern] |> format_exp()
    guard = refinement[:guard] |> format_guard()
    exp = refinement[:expr] |> format_exp()

    "#{pat}#{guard} ← #{exp}"
  end

  defp format_comment(comment) do
    comment_str =
      comment
      |> to_string()
      |> String.split(Pantagruel.Scan.comment_continuation())
      |> Enum.map(&String.trim/1)
      |> Enum.join("\n> ")

    "\n> #{comment_str}\n"
  end

  defp format_alias(alias_name: names, alias_expr: ref) do
    alias_names = join_exp(names, [], ",")

    "#{alias_names} ⇐ #{format_exp(ref)}"
  end

  defp format_binding([bind_symbol: sym, bind_domain: d], s),
    do: join_exp([sym, ":", d], s, " ")

  defp format_guard(nil), do: ""
  defp format_guard(guard), do: " ⸳ #{format_exp(guard)}"

  defp format_symbol(s, scopes) do
    name = Env.lookup_binding_name(s)

    Env.is_bound?(s, scopes)
    |> if(do: name, else: "*#{name}*")
  end

  defp format_container(c, exps, s) do
    {l, r} = delimiters(c)
    inner_str = join_exp(exps, s, ",")

    [l, inner_str, r]
    |> Enum.join()
  end

  defp format_quantification([quantifier: op, bindings: binding, expr: expr], s) do
    q = format_exp(op, s)
    bind = join_exp(binding, s, ",")
    exp = format_exp(expr, s)

    [q, bind, "⸳", exp]
    |> Enum.join(" ")
  end

  defp format_comprehension(container, [comp_bindings: bindings, comp_expression: expr], s) do
    binding_str = join_exp(bindings, s, ",")
    expr_str = format_exp(expr)
    inner_str = "#{binding_str} ⸳ #{expr_str}"

    format_exp({container, [inner_str]}, s)
  end

  @spec format_lambda(any, keyword) :: t
  defp format_lambda(
         %Lambda{name: name, domain: domain, codomain: codomain, type: type},
         opts
       ) do
    scope = opts[:scope] || []

    prefix = lambda_prefix(name, scope)
    args = lambda_args(opts[:decl][:lambda_args][:args], scope)
    dom = join_exp(domain, scope, ",")
    pred = lambda_guard(opts[:decl][:lambda_guard], scope)
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
  defp lambda_args(args, s), do: join_exp(args, s, ",") <> ":"

  defp lambda_guard(nil, _), do: ""
  defp lambda_guard(expr, s), do: " ⸳ " <> join_exp(expr, s, ",")

  defp lambda_yields(nil), do: ""
  defp lambda_yields('::'), do: " ∷ "
  defp lambda_yields('=>'), do: " ⇒ "

  defp delimiters(:par), do: {"(", ")"}
  defp delimiters(:list), do: {"[", "]"}
  defp delimiters(:set), do: {"{", "}"}
end
