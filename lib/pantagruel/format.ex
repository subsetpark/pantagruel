defmodule Pantagruel.Format do
  @moduledoc """
  Takes an evaluated Pantagruel program and generates a formatted text
  representation of it.
  """
  import BoolAlg, only: [is_relation: 1]
  alias Pantagruel.Values.{Domain, Variable, Lambda}
  alias Pantagruel.Env
  alias Pantagruel.Eval.Module

  @type ast :: [term]
  @type section :: {:chapters, any} | {:imports, any} | {:module, any}
  @type t :: String.t()

  @doc """
  Generate a string representation of an evaluated program.
  """
  @spec format_program(ast) :: t
  def format_program({:program, [module, imports, chapters]}) do
    [
      format_module(module),
      format_imports(imports),
      format_chapters(chapters)
    ]
    |> Stream.reject(&(&1 == ""))
    |> Enum.join("\n\n")
  end

  @spec format_scopes(Env.t()) :: t
  def format_scopes(scopes), do: format_with(scopes, &format_scope/1)

  @spec format_lifted(ast) :: t
  def format_lifted(tree), do: format_with(tree, &format_exp/1)

  @bar "***"

  defp format_with(data, f) do
    data
    |> Stream.map(f)
    |> Enum.join("\n\n#{@bar}\n\n")
  end

  @doc """
  Generate a string representation of a parsed program section.
  """

  def format_module(nil), do: ""
  def format_module(mod_name), do: "# #{mod_name}"

  def format_imports(imports) do
    imports
    |> Stream.map(&format_import/1)
    |> Enum.join("\n")
  end

  def format_chapters(chapters), do: format_with(chapters, &format_chapter/1)

  def format_import({:import, mod_name}), do: ": #{mod_name}"
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

  def format_exp({op, l, r}, s) when is_relation(op) do
    "#{format_exp(l, s)} #{format_relation(op)} #{format_exp(r, s)}"
  end

  def format_exp({:not, exp}, s) do
    "#{format_exp(:not)} #{format_exp(exp, s)}"
  end

  def format_exp({:cont, [c, exps]}, s), do: format_container(c, exps, s)

  def format_exp({:quantification, [op, bindings, exp]}, s) do
    q = format_exp(op)
    bind = join_exp(bindings, s, ", ")
    exp = format_exp(exp, s)

    Enum.join([q, bind, "⸳", exp], " ")
  end

  def format_exp({:comprehension, [bindings, exp]}, s) do
    binding_str = join_exp(bindings, s, ",")
    exp_str = format_exp(exp)
    "#{binding_str} ⸳ #{exp_str}"
  end

  def format_exp({:binding, [sym, domain]}, s),
    do: join_exp([sym, {:symbol, ":"}, domain], s, "")

  def format_exp({:guard, expr}, s), do: format_exp(expr, s)
  def format_exp({:lambda, l}, s), do: format_lambda(l, scope: s)
  def format_exp(%Lambda{} = l, s), do: format_lambda(l, scope: s)
  def format_exp({:literal, literal}, _), do: "*#{literal}*"
  def format_exp({:bin_appl, [op, x, y]}, s), do: join_exp([x, op, y], s, " ")
  def format_exp({:un_appl, [op, x]}, s), do: join_exp([op, x], s)
  def format_exp({:f_appl, [f, x]}, s), do: join_exp([f, x], s, " ")
  def format_exp({:dot, [f, x]}, s), do: join_exp([x, f], s, ".")
  def format_exp({:refinement_exp, _} = r, s), do: format_case_expr(r, s)
  def format_exp(exp, s), do: join_exp(exp, s, " ")

  defp format_chapter({:chapter, [head, body]}) do
    [head, body]
    |> Stream.reject(&(&1 == []))
    |> Stream.intersperse([:sep])
    |> Stream.concat()
    # For now, assume we want markdown compatibility.
    |> Stream.map(&(format_line(&1) <> "  "))
    |> Enum.join("\n")
  end

  # Format the contents of the environment after program evaluation.
  defp format_scope(scope) do
    scope
    |> Map.values()
    |> Stream.filter(fn
      %Domain{name: name, ref: ref} -> ref != name
      _ -> true
    end)
    |> Enum.sort(fn
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
    end)
    |> Enum.map(&format_exp/1)
    |> Enum.join("\n")
  end

  defp join_exp(exps, s, sep \\ "") do
    exps
    |> Stream.map(&format_exp(&1, s))
    |> Enum.join(sep)
  end

  defp format_symbol(s) do
    Env.lookup_binding_name(s)
    |> String.replace("_", "-")
  end

  defp format_line(:sep), do: "...."
  defp format_line({:decl, declaration}), do: format_lambda(declaration, decl: declaration)

  defp format_line({:alias, [names, ref]}) do
    alias_names = join_exp(names, [], ",")

    "#{alias_names} ⇐ #{format_exp(ref)}"
  end

  defp format_line({:comment, comment}), do: format_comment(comment)

  defp format_line({:expr, [nil, expression]}), do: format_exp(expression)

  defp format_line({:expr, [intro_op, expression]}),
    do: "#{format_exp(intro_op)} #{format_exp(expression)}"

  defp format_line(%BoolAlg{op: op, x: x, y: y}) do
    "#{format_exp(x)} #{format_exp(op)} #{format_exp(y)}"
  end

  defp format_line({:refinement, [pat, case_exprs]}, scope \\ []) do
    case_exprs =
      case_exprs
      |> Enum.map(&("- " <> format_case_expr(&1, scope)))
      |> Enum.join("\n")

    pat = format_exp(pat, scope)

    "#{pat} ← \n#{case_exprs}"
  end

  defp format_case_expr({:refinement_exp, [guard, exp]}, scope) do
    guard = format_guard(guard, scope)
    exp = format_exp(exp, scope)
    "#{guard}#{exp}"
  end

  defp format_comment(comment) do
    comment_str =
      comment
      |> to_string()
      |> String.split(<<0xF8FF::utf8>>)
      |> Enum.map(&String.trim/1)
      |> Enum.join("\n> ")

    "\n> #{comment_str}\n"
  end

  defp format_guard(nil, _), do: ""
  defp format_guard(guard, scope), do: "#{format_exp(guard, scope)} ⸳ "

  defp format_symbol(s, scopes) do
    name = Env.lookup_binding_name(s)

    Env.is_bound?(s, scopes)
    |> if(do: name, else: "*#{name}*")
  end

  defp format_container(c, exps, s) when not is_list(exps), do: format_container(c, [exps], s)

  defp format_container(c, exps, s) do
    {l, r} = delimiters(c)
    inner_str = join_exp(exps, s, ",")

    Enum.join([l, inner_str, r])
  end

  @spec format_lambda(nil | Keyword.t() | map, keyword) :: t
  defp format_lambda(
         %Lambda{name: name, domain: domain, codomain: codomain, type: type},
         opts
       ) do
    scope = opts[:scope] || []

    body =
      case opts[:decl] do
        [_, body, _, _] -> join_exp(body, scope, ", ")
        _ -> join_exp(domain, scope, ", ")
      end

    prefix = lambda_prefix(name, scope)
    yields = lambda_yields(type)
    codom = format_exp(codomain, scope)

    [prefix, body, yields, codom]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  defp format_lambda(l, opts) do
    Lambda.from_declaration(l) |> format_lambda(opts)
  end

  defp format_relation(:conj), do: format_exp(:and)
  defp format_relation(:disj), do: format_exp(:or)
  defp format_relation(:xor), do: format_exp(:xor)
  defp format_relation(:impl), do: format_exp(:->)
  defp format_relation(:iff), do: format_exp(:"<->")

  defp lambda_prefix(nil, _), do: "λ"
  defp lambda_prefix(name, s), do: format_exp(name, s)

  defp lambda_yields(nil), do: ""
  defp lambda_yields('::'), do: "∷"
  defp lambda_yields('=>'), do: "⇒"

  defp delimiters(:par), do: {"(", ")"}
  defp delimiters(:list), do: {"[", "]"}
  defp delimiters(:set), do: {"{", "}"}
end
