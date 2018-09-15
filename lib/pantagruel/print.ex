defmodule Pantagruel.Print do
  alias Pantagruel.Eval.{Domain, Variable, Lambda}
  alias Pantagruel.Env

  @type t :: String.t()
  @doc """
  Generate a string representation of an evaluated program.
  """
  @spec print_program(Pantagruel.Parse.t(), Env.t()) :: t
  def print_program(parsed, scopes) do
    bar = &String.duplicate(&1, 10)

    f = fn {section, scope} ->
      [print_scope(scope), print_section(section)]
      |> Enum.join("\n#{bar.("―")}\n")
    end

    Enum.zip(parsed, scopes)
    |> Enum.map(f)
    |> Enum.join("\n#{bar.("═")}\n")
  end

  # Print the contents of the environment after program evaluation.
  defp print_scope(scope), do: Map.values(scope) |> Enum.map(&print_exp/1) |> Enum.join("\n")

  defp print_section({:section, section}) do
    print_line = fn
      {:decl, declaration} -> print_lambda(declaration, decl: declaration)
      {:alias, [value, domain]} -> "#{print_exp(value)} ⇒ #{print_exp(domain)}"
      {:comment, comment} -> print_comment(comment)
      {:expr, expression} -> print_exp(expression)
    end

    Stream.concat(section[:head], section[:body] || [])
    |> Stream.map(print_line)
    |> Enum.join("\n")
  end

  @spec print_lambda(any, keyword) :: t
  def print_lambda(lambda, opts \\ [])

  def print_lambda(
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
      print_exp(codomain, scope)
    ]
    |> Enum.join()
  end

  def print_lambda(l, opts) do
    Lambda.from_declaration(l) |> print_lambda(opts)
  end

  @doc """
  Print an individual expression component.
  """
  @spec print_exp(any, [%{}]) :: t
  def print_exp(value, scope \\ [])

  def print_exp(%Domain{name: name, alias: alias}, scope) do
    "#{print_exp(alias, scope)} ⇒ #{print_exp(name, scope)}"
  end

  def print_exp(%Variable{name: name, domain: domain}, scope) do
    "#{print_exp(name, scope)} : #{print_exp(domain, scope)}"
  end

  def print_exp(symbol, scopes)
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

  def print_exp({container, exps}, s)
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

  def print_exp(
        {:quantifier, quant_operator: op, quant_bindings: binding, quant_expression: expr},
        s
      ) do
    [
      print_exp(op, s),
      exp_join(binding, s),
      "⸳",
      print_exp(expr, s)
    ]
    |> Enum.join(" ")
  end

  def print_exp({:comprehension, [{container, [binding, expr]}]}, s) do
    binding_str = exp_join(binding, s)
    expr_str = print_exp(expr)
    inner_str = "#{binding_str} ⸳ #{expr_str}"
    print_exp({container, [inner_str]}, s)
  end

  def print_exp({:lambda, l}, s), do: print_lambda(l, scope: s)
  def print_exp(%Lambda{} = l, s), do: print_lambda(l, scope: s)

  def print_exp({:intro_op, op}, s), do: print_exp(op, s)

  def print_exp({:literal, literal}, _) do
    cond do
      String.contains?(literal, " ") -> "`#{literal}`"
      true -> "`" <> literal
    end
  end

  def print_exp({:refinement, refinement}, s) do
    right_str = print_exp(refinement[:expr], s)

    guard_str =
      case refinement[:guard] do
        nil -> ""
        guard -> " ⸳ #{print_exp(guard, s)}"
      end

    "#{print_exp(refinement[:pattern], s)}#{guard_str} ← #{right_str}"
  end

  def print_exp({:appl, operator: op, x: x, y: y}, s) do
    "#{print_exp(x, s)} #{print_exp(op, s)} #{print_exp(y, s)}"
  end

  def print_exp({:appl, f: f, x: x}, s) do
    "#{print_exp(f, s)} #{print_exp(x, s)}"
  end

  def print_exp(exp, s) do
    exp
    |> Enum.map(&print_exp(&1, s))
    |> Enum.join(" ")
  end

  defp print_comment([comment]) do
    comment_str =
      comment
      |> String.split(Pantagruel.Scan.comment_continuation())
      |> Enum.map(&String.trim/1)
      |> Enum.join("\n")

    "\n" <> comment_str <> "\n"
  end

  defp exp_join(exprs, s) do
    exprs
    |> Enum.map(&print_exp(&1, s))
    |> Enum.join(", ")
  end
end
