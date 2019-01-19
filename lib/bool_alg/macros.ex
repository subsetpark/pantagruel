defmodule BoolAlg.Macros do
  defmacro deftrue(operation, result, result2 \\ nil) do
    result2 = result2 || result

    quote do
      defp reduce(%__MODULE__{op: unquote(operation), x: true, y: var!(q)}), do: unquote(result)
      defp reduce(%__MODULE__{op: unquote(operation), x: var!(q), y: true}), do: unquote(result2)
    end
  end

  defmacro deffalse(operation, result, result2 \\ nil) do
    result2 = result2 || result

    quote do
      defp reduce(%__MODULE__{op: unquote(operation), x: false, y: var!(q)}), do: unquote(result)
      defp reduce(%__MODULE__{op: unquote(operation), x: var!(q), y: false}), do: unquote(result2)
    end
  end
end
