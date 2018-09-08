defmodule Pantagruel.Print do
  alias Pantagruel.Print
  alias Pantagruel.Eval.{Scope, Lambda}

  def to_string(parsed, scopes) do
    Enum.zip(parsed, scopes)
    |> Enum.map(&Print.to_string/1)
    |> Enum.join("\n#{String.duplicate("═", 10)}\n")
  end

  def to_string({section, scope}) do
    [print_scope(scope), print_section(section)]
    |> Enum.join("\n#{String.duplicate("―", 10)}\n")
  end

  def print_scope(scope) do
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
    [print_head(section[:head]), print_body(section[:body])]
    |> Enum.join("\n")
  end

  defp print_head(decls) do
    decls
    |> Enum.map(&print_subhead/1)
    |> Enum.join("\n")
  end

  defp print_subhead({:decl, declaration}) do
    declaration
    |> (fn d -> Lambda.from_declaration(d) end).()
    |> print_lambda(declaration[:lambda_args])
  end

  def print_body(nil), do: ""

  def print_body(exprs) do
    exprs
    |> Enum.map(&print_subbody/1)
    |> Enum.join("\n")
  end

  defp print_subbody({:expr, expression}) do
    right_str = print_subexpression(expression[:right])

    case expression[:left] do
      nil -> right_str
      left -> "#{print_subexpression(left)} ⇒ #{right_str}"
    end
  end

  defp print_subexpression(symbol) when is_binary(symbol) or is_number(symbol) or is_atom(symbol),
    do: Scope.translate_domain(symbol)

  defp print_subexpression({container, subexprs})
       when container in [:bunch, :list, :string, :set] do
    {l, r} =
      case container do
        :bunch -> {"(", ")"}
        :list -> {"[", "]"}
        :string -> {"\"", "\""}
        :set -> {"{", "}"}
      end

    inner_str =
      subexprs
      |> Enum.map(&print_subexpression/1)
      |> Enum.join(", ")

    "#{l}#{inner_str}#{r}"
  end

  defp print_subexpression({:quantifier, [quantifier, binding, expr]}) do
    IO.inspect({quantifier, binding})

    [
      print_subexpression(quantifier),
      Enum.map(binding, &print_subexpression/1)
      |> Enum.join(", "),
      "⸳",
      print_subexpression(expr)
    ]
    |> Enum.join(" ")
  end

  defp print_subexpression({:comprehension, [{container, [expr, binding]}]}) do
    binding_str =
      Enum.map(binding, &print_subexpression/1)
      |> Enum.join(", ")

    expr_str = print_subexpression(expr)
    inner_str = "#{expr_str} ⸳ #{binding_str}"
    print_subexpression({container, [inner_str]})
  end

  defp print_subexpression({:lambda, lambda}), do: print_subhead({:decl, lambda})

  defp print_subexpression({:literal, literal}) do
    cond do
      String.contains?(literal, " ") -> "`#{literal}`"
      true -> "`#{literal}"
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
        nil ->
          ""

        args ->
          (args
           |> Enum.map(&print_subexpression/1)
           |> Enum.join(", ")) <> ":"
      end

    dom_str =
      lambda.domain
      |> Enum.map(&print_subexpression/1)
      |> Enum.join(", ")

    name_str <>
      "|#{args_str}#{dom_str}|" <>
      yields_string(lambda.type, lambda.codomain) <> codomain_string(lambda.codomain)
  end
end
