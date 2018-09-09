defmodule Pantagruel.Print do
  alias Pantagruel.Eval.{Domain, Variable, Lambda}
  alias Pantagruel.Env

  @doc """
  Generate a string representation of an evaluated program.
  """
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

  # Re-print the parsed program.
  defp print_section({:section, section}) do
    print_line = fn
      {:expr, expression} ->
        right_str = print_subexp(expression[:right])

        case expression[:left] do
          nil -> right_str
          left -> "#{print_subexp(left)} ← #{right_str}"
        end

      {:comment, comment} ->
        print_comment(comment)
    end

    print_head = fn decls ->
      decls
      |> Enum.map(&print_lambda/1)
      |> Enum.join("\n")
    end

    print_body = fn exprs ->
      exprs
      |> Enum.map(print_line)
      |> Enum.join("\n")
    end

    [
      print_head.(section[:head]),
      print_body.(section[:body] || [])
    ]
    |> Enum.join("\n")
  end

  def print_lambda(lambda, decl \\ nil)

  def print_lambda(
        %Lambda{name: name, domain: domain, codomain: codomain, type: type},
        decl
      ) do
    name_str =
      case name do
        nil -> ""
        name -> "#{name} : "
      end

    args_str =
      case decl[:lambda_args] do
        nil -> ""
        args -> subexp_join(args) <> ":"
      end

    dom_str = subexp_join(domain)

    predicate_str =
      case decl[:predicate] do
        nil -> ""
        subexpr -> " ⸳ #{subexp_join(subexpr)}"
      end

    yields_str =
      case type do
        nil -> ""
        :function -> " :: "
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

  def print_lambda({:decl, declaration}, _),
    do: print_lambda(declaration, declaration)

  def print_lambda({:comment, comment}, _),
    do: print_comment(comment)

  def print_lambda(l, decl) do
    Lambda.from_declaration(l) |> print_lambda(decl)
  end

  # Print an individual expression component.
  defp print_subexp(nil), do: ""

  # TODO: This is just cribbed from Z. Can we do better?
  defp print_subexp(%Domain{name: name}) do
    "[#{name}]"
  end

  defp print_subexp(%Variable{name: name, domain: domain}) do
    "#{name} : #{domain}"
  end

  defp print_subexp(symbol) when is_binary(symbol) or is_number(symbol) or is_atom(symbol),
    do: Env.lookup_binding_name(symbol)

  defp print_subexp({container, subexps})
       when container in [:bunch, :list, :string, :set] do
    {l, r} =
      case container do
        :bunch -> {"(", ")"}
        :list -> {"[", "]"}
        :string -> {"\"", "\""}
        :set -> {"{", "}"}
      end

    inner_str = subexp_join(subexps)

    "#{l}#{inner_str}#{r}"
  end

  defp print_subexp({:quantifier, [quantifier, binding, expr]}) do
    [
      print_subexp(quantifier),
      subexp_join(binding),
      "⸳",
      print_subexp(expr)
    ]
    |> Enum.join(" ")
  end

  defp print_subexp({:comprehension, [{container, [binding, expr]}]}) do
    binding_str = subexp_join(binding)
    expr_str = print_subexp(expr)
    inner_str = "#{binding_str} ⸳ #{expr_str}"
    print_subexp({container, [inner_str]})
  end

  defp print_subexp({:lambda, l}), do: print_lambda(l)

  defp print_subexp({:literal, literal}) do
    cond do
      String.contains?(literal, " ") -> "`#{literal}`"
      true -> "`" <> literal
    end
  end

  defp print_subexp(subexp) do
    subexp
    |> Enum.map(&print_subexp/1)
    |> Enum.join(" ")
  end

  defp print_comment([comment]), do: "\n" <> comment <> "\n"

  defp subexp_join(exprs) do
    exprs
    |> Enum.map(&print_subexp/1)
    |> Enum.join(", ")
  end
end
