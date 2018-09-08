defmodule Pantagruel.Print do
  alias Pantagruel.Eval.Lambda
  alias Pantagruel.Env

  @doc """
  Generate a string representation of an evaluated program.
  """
  def print_program(parsed, scopes) do
    f = fn {section, scope} ->
      [print_scope(scope), print_section(section)]
      |> Enum.join("\n#{bar("―")}\n")
    end

    Enum.zip(parsed, scopes)
    |> Enum.map(f)
    |> Enum.join("\n#{bar("═")}\n")
  end

  defp print_scope(scope) do
    for(
      {_k, v} <- scope,
      do: print_scope_element(v)
    )
    |> Enum.join("\n")
  end

  defp print_scope_element(%Lambda{} = l), do: print_lambda(l)
  defp print_scope_element(e), do: String.Chars.to_string(e)

  defp print_section(nil), do: ""

  defp print_section({:section, section}) do
    print_head = fn decls ->
      decls
      |> Enum.map(&print_declaration/1)
      |> Enum.join("\n")
    end

    print_body = fn exprs ->
      exprs
      |> Enum.map(&print_subbody/1)
      |> Enum.join("\n")
    end

    [print_head.(section[:head]), print_body.(section[:body] || [])]
    |> Enum.join("\n")
  end

  defp print_declaration({:decl, declaration}) do
    declaration
    |> (fn d -> Lambda.from_declaration(d) end).()
    |> print_lambda(declaration[:lambda_args])
  end

  defp print_subbody({:expr, expression}) do
    right_str = print_subexpression(expression[:right])

    case expression[:left] do
      nil -> right_str
      left -> "#{print_subexpression(left)} ← #{right_str}"
    end
  end

  defp print_subexpression(symbol) when is_binary(symbol) or is_number(symbol) or is_atom(symbol),
    do: Env.lookup_binding_name(symbol)

  defp print_subexpression({container, subexprs})
       when container in [:bunch, :list, :string, :set] do
    {l, r} =
      case container do
        :bunch -> {"(", ")"}
        :list -> {"[", "]"}
        :string -> {"\"", "\""}
        :set -> {"{", "}"}
      end

    inner_str = subexp_join(subexprs)

    "#{l}#{inner_str}#{r}"
  end

  defp print_subexpression({:quantifier, [quantifier, binding, expr]}) do
    [
      print_subexpression(quantifier),
      subexp_join(binding),
      "⸳",
      print_subexpression(expr)
    ]
    |> Enum.join(" ")
  end

  defp print_subexpression({:comprehension, [{container, [binding, expr]}]}) do
    binding_str = subexp_join(binding)
    expr_str = print_subexpression(expr)
    inner_str = "#{binding_str} ⸳ #{expr_str}"
    print_subexpression({container, [inner_str]})
  end

  defp print_subexpression({:lambda, l}), do: print_declaration({:decl, l})

  defp print_subexpression({:literal, literal}) do
    cond do
      String.contains?(literal, " ") -> "`#{literal}`"
      true -> "`" <> literal
    end
  end

  defp print_subexpression(subexpr) do
    subexpr
    |> Enum.map(&print_subexpression/1)
    |> Enum.join(" ")
    |> String.Chars.to_string()
  end

  defp yields_string(nil, _), do: ""
  defp yields_string(_, nil), do: ""
  defp yields_string(:function, _), do: " :: "
  defp yields_string(:constructor, _), do: " ⇒ "

  defp codomain_string(nil), do: ""
  defp codomain_string(codomain), do: print_subexpression(codomain)

  def print_lambda(lambda, decl_args \\ nil) do
    name_str =
      case lambda.name do
        nil -> ""
        name -> "#{name} : "
      end

    args_str =
      case decl_args do
        nil -> ""
        args -> "#{subexp_join(args)}:"
      end

    dom_str = subexp_join(lambda.domain)

    name_str <>
      "|#{args_str}#{dom_str}|" <>
      yields_string(lambda.type, lambda.codomain) <> codomain_string(lambda.codomain)
  end

  defp bar(char), do: String.duplicate(char, 10)

  defp subexp_join(exprs) do
    exprs
    |> Enum.map(&print_subexpression/1)
    |> Enum.join(", ")
  end
end
