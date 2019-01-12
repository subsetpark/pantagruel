defmodule BoolAlg.Macros do
  defmacro deftrue(operation, result) do
    quote do
      defp reduce(%__MODULE__{
            op: unquote(operation),
            x: true,
            y: var!(q)
          }) do
        unquote(result)
      end

      defp reduce(%__MODULE__{
            op: unquote(operation),
            x: var!(q),
            y: true
          }) do
        unquote(result)
      end
    end
  end

  defmacro deftrue(operation, result, result2) do
    quote do
      defp reduce(%__MODULE__{
            op: unquote(operation),
            x: true,
            y: var!(q)
          }) do
        unquote(result)
      end

      defp reduce(%__MODULE__{
            op: unquote(operation),
            x: var!(q),
            y: true
          }) do
        unquote(result2)
      end
    end
  end

  defmacro deffalse(operation, result) do
    quote do
      defp reduce(%__MODULE__{
            op: unquote(operation),
            x: false,
            y: var!(q)
          }) do
        unquote(result)
      end

      defp reduce(%__MODULE__{
            op: unquote(operation),
            x: var!(q),
            y: false
          }) do
        unquote(result)
      end
    end
  end

  defmacro deffalse(operation, result, result2) do
    quote do
      defp reduce(%__MODULE__{
            op: unquote(operation),
            x: false,
            y: var!(q)
          }) do
        unquote(result)
      end

      defp reduce(%__MODULE__{
            op: unquote(operation),
            x: var!(q),
            y: false
          }) do
        unquote(result2)
      end
    end
  end
end
