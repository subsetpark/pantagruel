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
  defp print_scope(scope) do
    for(
      {_k, v} <- scope,
      do:
        case v do
          %Lambda{} = l -> print_lambda(l)
          e -> print_subexp(e)
        end
    )
    |> Enum.join("\n")
  end

  defp print_section({:section, section}) do
    print_line = fn
      {:decl, declaration} -> print_lambda(declaration, decl: declaration)
      {:alias, [value, domain]} -> "#{print_subexp(value)} ⇒ #{print_subexp(domain)}"
      {:comment, comment} -> print_comment(comment)
      {:expr, expression} -> print_subexp(expression)
    end

    Stream.concat(section[:head], section[:body] || [])
    |> Stream.map(print_line)
    |> Enum.join("\n")
  end

  @spec print_lambda(any) :: t
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
        args -> subexp_join(args, scope) <> ":"
      end

    dom_str = subexp_join(domain, scope)

    predicate_str =
      case opts[:decl][:predicate] do
        nil -> ""
        subexpr -> " ⸳ #{subexp_join(subexpr, scope)}"
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
      print_subexp(codomain)
    ]
    |> Enum.join()
  end

  def print_lambda(l, opts) do
    Lambda.from_declaration(l) |> print_lambda(opts)
  end

  @doc """
  Print an individual expression component.
  """
  @spec print_subexp(any, [%{}]) :: t
  def print_subexp(value, scope \\ [])

  def print_subexp(%Domain{name: name, alias: alias}, _) do
    "#{print_subexp(alias)} ⇒ #{print_subexp(name)}"
  end

  def print_subexp(%Variable{name: name, domain: domain}, _) do
    "#{name} : #{domain}"
  end

  def print_subexp(symbol, scopes)
      when is_binary(symbol) or is_number(symbol) or is_atom(symbol) do
    name = Env.lookup_binding_name(symbol)

    case {scopes, Env.is_bound?(symbol, scopes)} do
      {[], _} -> name
      {_, true} -> name
      _ -> "*#{name}*"
    end
  end

  def print_subexp({container, subexps}, s)
      when container in [:bunch, :list, :string, :set] do
    {l, r} =
      case container do
        :bunch -> {"(", ")"}
        :list -> {"[", "]"}
        :string -> {"\"", "\""}
        :set -> {"{", "}"}
      end

    inner_str = subexp_join(subexps, s)

    "#{l}#{inner_str}#{r}"
  end

  def print_subexp(
        {:quantifier, quant_operator: op, quant_bindings: binding, quant_expression: expr},
        s
      ) do
    [
      print_subexp(op, s),
      subexp_join(binding, s),
      "⸳",
      print_subexp(expr, s)
    ]
    |> Enum.join(" ")
  end

  def print_subexp({:comprehension, [{container, [binding, expr]}]}, s) do
    binding_str = subexp_join(binding, s)
    expr_str = print_subexp(expr)
    inner_str = "#{binding_str} ⸳ #{expr_str}"
    print_subexp({container, [inner_str]}, s)
  end

  def print_subexp({:lambda, l}, s), do: print_lambda(l, s)

  def print_subexp({:intro_op, op}, s), do: print_subexp(op, s)

  def print_subexp({:literal, literal}, _) do
    cond do
      String.contains?(literal, " ") -> "`#{literal}`"
      true -> "`" <> literal
    end
  end

  def print_subexp({:refinement, refinement}, s) do
    right_str = print_subexp(refinement[:subexpr], s)

    guard_str =
      case refinement[:guard] do
        nil -> ""
        guard -> " ⸳ #{print_subexp(guard, s)}"
      end

    "#{print_subexp(refinement[:pattern], s)}#{guard_str} ← #{right_str}"
  end

  def print_subexp({:appl, operator: op, x: x, y: y}, s) do
    "#{print_subexp(x, s)} #{print_subexp(op, s)} #{print_subexp(y, s)}"
  end

  def print_subexp({:appl, f: f, x: x}, s) do
    "#{print_subexp(f, s)} #{print_subexp(x, s)}"
  end

  def print_subexp(subexp, s) do
    subexp
    |> Enum.map(&print_subexp(&1, s))
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

  defp subexp_join(exprs, s) do
    exprs
    |> Enum.map(&print_subexp(&1, s))
    |> Enum.join(", ")
  end
end
