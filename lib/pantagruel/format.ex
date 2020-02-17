defmodule Pantagruel.Format do
  @moduledoc """
  Takes an evaluated Pantagruel program and generates a formatted text
  representation of it.
  """
  alias Pantagruel.Values.{Domain, Variable, Lambda}
  alias Pantagruel.Env
  alias Pantagruel.Eval.Module

  import Pantagruel.Macros

  use Witchcraft

  @type ast :: [term]
  @type t :: String.t()

  @interpunct "|"

  @doc """
  Generate a string representation of an evaluated program.
  """
  @spec format_program({:program, [any, ...]}) :: t
  def format_program({:program, [module, imports, chapters]}) do
    [
      format_module(module),
      format_imports(imports),
      format_chapters(chapters)
    ]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join("\n\n")
  end

  @spec format_env(Env.t()) :: t
  def format_env(env), do: format_with(env, &format_scope/1)

  @bar "***"

  defp format_with(data, f) do
    data
    |> lift(f)
    |> Enum.join("\n\n#{@bar}\n\n")
  end

  @doc """
  Generate a string representation of a parsed program section.
  """

  def format_module(nil), do: ""
  def format_module(mod_name), do: "# #{mod_name}"

  def format_imports(imports) do
    imports
    |> lift(&format_import/1)
    |> Enum.join("\n")
  end

  def format_chapters(chapters), do: format_with(chapters, &format_chapter/1)

  def format_import(mod_name), do: ": *#{mod_name}*  "
  defguard is_term(s) when is_number(s) or is_atom(s) or is_binary(s)

  @doc """
  Format an individual expression.
  """
  @spec format_exp(any, [%{}]) :: t
  def format_exp(value, scope \\ [])

  def format_exp({:bin_appl, [op, x, y]}, s), do: join_exp([x, op, y], s, " ")
  def format_exp(%BoolAlg{} = b, s), do: format_bool_alg(b, s)
  def format_exp({:case_exp, _} = r, s), do: format_case_expr(r, s)
  def format_exp({:comment, comment}, _s), do: format_comment(comment)
  def format_exp({:cont, [c, exps]}, s), do: format_container(c, exps, s)
  def format_exp(%Domain{name: n, ref: ref}, s), do: join_exp([n, "⇐", ref], s, " ")
  def format_exp({:dot, [f, x]}, s), do: join_exp([x, f], s, ".")
  def format_exp({:f_appl, [f, x]}, s), do: join_exp([f, x], s, " ")
  def format_exp({:guard, expr}, s), do: format_exp(expr, s)
  def format_exp({:lambda, l}, s), do: format_lambda(l, scope: s)
  def format_exp(%Lambda{} = l, s), do: format_lambda(l, scope: s)
  def format_exp({:literal, literal}, _), do: "*#{literal}*"
  def format_exp(%Module{name: n}, _), do: "# #{n}"
  def format_exp({:not, exp}, s), do: "#{format_exp(:not)} #{format_exp(exp, s)}"
  def format_exp(:sep, _scope), do: "...."
  def format_exp(sym(_) = s, scopes), do: format_symbol(s, scopes)
  def format_exp({:un_appl, [op, x]}, s), do: join_exp([op, x], s)
  def format_exp(%Variable{name: n, domain: dom}, s), do: join_exp([n, ":", dom], s, " ")
  def format_exp(s, []) when is_term(s), do: Env.lookup_binding_name(s)
  def format_exp(s, scopes) when is_term(s), do: format_symbol(s, scopes)

  def format_exp({:decl, declaration}, s),
    do: format_lambda(declaration, decl: declaration, scope: s)

  def format_exp({:alias, [names, ref]}, s) do
    alias_names = join_exp(names, [], ",")

    "#{alias_names |> bold()} ⇐ #{format_exp(ref, s)}"
  end

  def format_exp(exp(nil, expression), s), do: format_exp(expression, s)

  def format_exp(exp(intro_op, expression), s),
    do: "#{format_exp(intro_op, s)} #{format_exp(expression, s)}"

  def format_exp({:refinement, [pat, case_exprs]}, scope),
    do: format_refinement(pat, case_exprs, scope)

  def format_exp({:quantification, [op, bindings, exp]}, s) do
    q = format_exp(op)
    bind = join_exp(bindings, s, ", ")
    exp = format_exp(exp, s)
    "#{q} #{bind} #{@interpunct} #{exp}"
  end

  def format_exp({:comprehension, [bindings, exp]}, s) do
    binding_str = join_exp(bindings, s, ", ")
    exp_str = format_exp(exp)
    "#{binding_str} #{@interpunct} #{exp_str}"
  end

  def format_exp({:binding, [sym, domain]}, s),
    do: join_exp([sym, sym(":"), domain], s, "")

  def format_exp(exp, s), do: join_exp(exp, s, " ")

  defp format_chapter({:chapter, [hd, []]}), do: do_format_chapter(hd)
  defp format_chapter({:chapter, [hd, body]}), do: do_format_chapter(hd ++ [:sep] ++ body)

  defp do_format_chapter(exps) do
    exps
    # For now, assume we want markdown compatibility.
    |> lift(&(format_exp(&1) <> line_ending(&1) <> "  "))
    |> Enum.join("\n")
  end

  defp line_ending(exp(_, _)), do: "."
  defp line_ending({:refinement, _}), do: "."
  defp line_ending(_), do: ""

  # Format the contents of the environment after program evaluation.
  defp format_scope(scope) do
    scope
    |> Map.from_struct()
    |> Map.values()
    |> Enum.filter(fn
      %Domain{name: name, ref: ref} -> ref != name
      nil -> false
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
    |> lift(&format_exp/1)
    |> Enum.join("\n")
  end

  defp join_exp(exps, s, sep \\ "") do
    exps
    |> lift(&format_exp(&1, s))
    |> Enum.join(sep)
  end

  defp format_case_expr({:case_exp, [guard, exp]}, scope) do
    guard = format_guard(guard, scope)
    exp = format_exp(exp, scope)
    "#{guard}#{exp}"
  end

  defp format_comment(comment) do
    comment_str =
      comment
      |> to_string()
      |> String.split(<<0xF8FF::utf8>>)
      |> lift(&String.trim/1)
      |> Enum.join("\n> ")

    "\n> #{comment_str}\n"
  end

  defp format_refinement(pat, [case_expr], scope) do
    case_expr = format_case_expr(case_expr, scope)
    pat = format_exp(pat, scope)

    "#{pat} ← #{case_expr}"
  end

  defp format_refinement(pat, case_exprs, scope) do
    case_exprs =
      case_exprs
      |> lift(&("- " <> format_case_expr(&1, scope)))
      |> Enum.join("\n")

    pat = format_exp(pat, scope)

    "#{pat} ← \n#{case_exprs}"
  end

  defp format_guard(nil, _), do: ""
  defp format_guard(true, _), do: ""
  defp format_guard(guard, scope), do: "#{format_exp(guard, scope)} #{@interpunct} "

  defp format_symbol(s, []), do: Env.lookup_binding_name(s) |> String.replace("_", "-")

  defp format_symbol(s, scopes) do
    name = Env.lookup_binding_name(s)

    if(Env.is_bound?(s, scopes), do: name, else: "*#{name}*")
  end

  defp format_container(c, exps, s) when not is_list(exps), do: format_container(c, [exps], s)

  defp format_container(c, exps, s) do
    {l, r} = delimiters(c)
    inner_str = join_exp(exps, s, ", ")

    "#{l}#{inner_str}#{r}"
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

    maybe_bold =
      case type do
        '=>' -> &bold/1
        _ -> & &1
      end

    codom = format_exp(codomain, scope) |> maybe_bold.()

    [prefix, body, yields, codom]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(" ")
  end

  defp format_lambda(l, opts), do: Lambda.from_declaration(l) |> format_lambda(opts)

  defp format_bool_alg(%BoolAlg{op: :not, x: x}, s), do: "#{format_exp(:"~")}#{format_exp(x, s)}"

  defp format_bool_alg(%BoolAlg{op: op, x: x, y: y}, s),
    do: "(#{format_exp(x, s)} #{format_relation(op)} #{format_exp(y, s)})"

  defp format_relation(:conj), do: format_exp(:and)
  defp format_relation(:disj), do: format_exp(:or)
  defp format_relation(:xor), do: format_exp(:xor)
  defp format_relation(:impl), do: format_exp(:->)
  defp format_relation(:iff), do: format_exp(:"<->")

  defp lambda_prefix(nil, _), do: "λ"
  defp lambda_prefix(name, s), do: format_exp(name, s) |> bold()

  defp lambda_yields(nil), do: ""
  defp lambda_yields('::'), do: "∷"
  defp lambda_yields('=>'), do: "⇒"

  defp delimiters(:par), do: {"(", ")"}
  defp delimiters(:sequence), do: {"[", "]"}
  defp delimiters(:set), do: {"{", "}"}

  defp bold(exp), do: "**#{exp}**"
end
