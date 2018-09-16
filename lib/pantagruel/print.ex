defmodule Pantagruel.Format do
  @moduledoc """
  Takes an evaluated Pantagruel program and generates a formatted text
  representation of it.
  """
  alias Pantagruel.Eval.{Domain, Variable, Lambda}
  alias Pantagruel.Env

  @type t :: String.t()
  @doc """
  Generate a string representation of an evaluated program.
  """
  @spec format_program(Pantagruel.Parse.t(), Env.t()) :: t
  def format_program(parsed, scopes) do
    bar = &String.duplicate(&1, 10)

    f = fn {section, scope} ->
      [format_scope(scope), format_section(section)]
      |> Enum.join("\n#{bar.("―")}\n")
    end

    Enum.zip(parsed, scopes)
    |> Enum.map(f)
    |> Enum.join("\n#{bar.("═")}\n")
  end

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

  def format_exp(symbol, scopes)
      when is_binary(symbol) or is_number(symbol) or is_atom(symbol) do
    name = Env.lookup_binding_name(symbol)

    case scopes do
      [] ->
        name

      s ->
        cond do
          Env.is_bound?(symbol, s) -> name
          true -> "*#{name}*"
        end
    end
  end

  def format_exp({container, exps}, s)
      when container in [:bunch, :list, :string, :set] do
    {l, r} =
      case container do
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
  defp format_scope(scope), do: scope |> Map.values() |> Enum.map(&format_exp/1) |> Enum.join("\n")

  defp format_section({:section, section}) do
    format_line = fn
      {:decl, declaration} -> format_lambda(declaration, decl: declaration)
      {:alias, [alias_expr: ref, alias_name: name]} -> "#{format_exp(ref)} ⇒ #{format_exp(name)}"
      {:comment, comment} -> format_comment(comment)
      {:expr, expression} -> format_exp(expression)
    end

    Stream.concat(section[:head], section[:body] || [])
    |> Stream.map(format_line)
    |> Enum.join("\n")
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
        name -> "#{name} : "
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
      |> Enum.join("\n")

    "\n" <> comment_str <> "\n"
  end

  defp exp_join(exprs, s) do
    exprs
    |> Enum.map(&format_exp(&1, s))
    |> Enum.join(", ")
  end
end
