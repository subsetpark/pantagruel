defmodule Pantagruel.Print do
  def to_string(scopes) do
    for(
      scope <- scopes,
      do: Pantagruel.Eval.Scope.to_string(scope)
    )
    |> Enum.join("\n\n;;\n\n")
  end
end
