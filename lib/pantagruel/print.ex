defmodule Pantagruel.Print do
  alias Pantagruel.Print
  alias Pantagruel.Eval.{Scope, Lambda}

  def to_string(parsed, scopes) do
    Enum.zip(parsed, scopes)
    |> Enum.map(&Print.to_string/1)
    |> Enum.join("\n\n;;\n\n")
  end

  def to_string({section, scope}) do
    [print_scope(scope), print_section(section)]
    |> Enum.join("\n―――\n")
  end

  def print_scope(scope) do
    for(
      {_k, v} <- scope,
      do: print_scope_element(v)
    )
    |> Enum.join("\n")
  end

  defp print_scope_element(%Lambda{} = l), do: Lambda.print_lambda(l)
  defp print_scope_element(e), do: String.Chars.to_string(e)

  defp print_section(nil), do: ""

  defp print_section({:section, section}) do
    [print_section(section[:head]), print_section(section[:body])]
    |> Enum.join("\n")
  end

  defp print_section(decl: declaration) do
    declaration
    |> (fn d -> Lambda.from_declaration(d) end).()
    |> Lambda.print_lambda(declaration[:lambda_args])
  end

  defp print_section(expr: expression) do
    right_str = print_subexpression(expression[:right])

    case expression[:left] do
      nil -> right_str
      left -> "#{print_subexpression(left)} ⇒ #{right_str}"
    end
  end

  defp print_subexpression(subexpr) do
    subexpr
    |> Scope.translate_domain()
    |> Enum.join(" ")
    |> String.Chars.to_string()
  end
end
