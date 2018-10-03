defmodule Pantagruel.Format do
  @moduledoc """
  Takes an evaluated Pantagruel program and generates a formatted text
  representation of it.
  """
  import Pantagruel.Guards
  alias Pantagruel.Eval.{Domain, Variable, Lambda}
  alias Pantagruel.Env

  @type t :: String.t()
  @doc """
  Generate a string representation of an evaluated program.
  """
  @spec format_program(Pantagruel.Parse.t()) :: t
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
    format_line = fn
      {:decl, declaration} ->
        format_lambda(declaration, decl: declaration)

      {:alias, [alias_expr: ref, alias_name: names]} ->
        alias_names =
          Enum.map(names, &format_exp/1)
          |> Enum.join(", ")

        "#{format_exp(ref)} ⇒ #{alias_names}"

      {:comment, comment} ->
        format_comment(comment)

      {:expr, expression} ->
        format_exp(expression)
    end

    Stream.concat(section[:head], section[:body] || [])
    |> Stream.map(format_line)
    |> Enum.join("\n")
  end

  defguard is_symbol(s) when is_binary(s) or is_number(s) or is_atom(s)

  @doc """
  Print an individual expression.
  """
  @spec format_exp(any, [%{}]) :: t
  def format_exp(value, scope \\ [])

  def format_exp(%Domain{name: name, ref: ref}, scope) do
    "#{format_exp(ref, scope)} ⇒ #{format_exp(name, scope)}"
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
    {l, r} =
      case c do
        :bunch -> {"(", ")"}
        :list -> {"[", "]"}
        :string -> {"\"", "\""}
        :set -> {"{", "}"}
      end

    inner_str = exp_join(exps, s)

    "#{l}#{inner_str}#{r}"
  end

  def format_exp(
        {:quantifier, quant_operator: op, quant_bindings: binding, quant_expression: expr},
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

  def format_exp({:comprehension, [{container, [binding, expr]}]}, s) do
    binding_str = exp_join(binding, s)
    expr_str = format_exp(expr)
    inner_str = "#{binding_str} ⸳ #{expr_str}"
    format_exp({container, [inner_str]}, s)
  end

  def format_exp({:lambda, l}, s), do: format_lambda(l, scope: s)
  def format_exp(%Lambda{} = l, s), do: format_lambda(l, scope: s)

  def format_exp({:intro_op, op}, s), do: format_exp(op, s)

  def format_exp({:literal, literal}, _) do
    cond do
      String.contains?(literal, " ") -> "`#{literal}`"
      true -> "`" <> literal
    end
  end

  def format_exp({:refinement, refinement}, s) do
    right_str = format_exp(refinement[:expr], s)

    guard_str =
      case refinement[:guard] do
        nil -> ""
        guard -> " ⸳ #{format_exp(guard, s)}"
      end

    "#{format_exp(refinement[:pattern], s)}#{guard_str} ← #{right_str}"
  end

  def format_exp({:appl, operator: op, x: x, y: y}, s) do
    "#{format_exp(x, s)} #{format_exp(op, s)} #{format_exp(y, s)}"
  end

  def format_exp({:appl, f: f, x: x}, s) do
    "#{format_exp(f, s)} #{format_exp(x, s)}"
  end

  def format_exp(exp, s) do
    exp
    |> Enum.map(&format_exp(&1, s))
    |> Enum.join(" ")
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

    name_str =
      case name do
        nil -> ""
        name -> "#{format_exp(name)} "
      end

    args_str =
      case opts[:decl][:lambda_args] do
        nil -> ""
        args -> exp_join(args, scope) <> ":"
      end

    dom_str = exp_join(domain, scope)

    predicate_str =
      case opts[:decl][:predicate] do
        nil -> ""
        expr -> " ⸳ #{exp_join(expr, scope)}"
      end

    yields_str =
      case type do
        nil -> ""
        :function -> " ∷ "
        :constructor -> " ⇒ "
      end

    [
      name_str,
      "|#{args_str}#{dom_str}#{predicate_str}|",
      yields_str,
      format_exp(codomain, scope)
    ]
    |> Enum.join()
  end

  defp format_lambda(l, opts) do
    Lambda.from_declaration(l) |> format_lambda(opts)
  end

  defp format_comment([comment]) do
    comment_str =
      comment
      |> String.split(Pantagruel.Scan.comment_continuation())
      |> Enum.map(&String.trim/1)
      |> Enum.join("\n> ")

    "\n> " <> comment_str <> "\n"
  end

  defp exp_join(exprs, s) do
    exprs
    |> Enum.map(&format_exp(&1, s))
    |> Enum.join(", ")
  end
end
