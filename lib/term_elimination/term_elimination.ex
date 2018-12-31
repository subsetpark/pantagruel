defmodule TermMacros do
  defmacro deftrue(operation, result) do
    quote do
      def reduce({unquote(operation), true, var!(q)}), do: unquote(result)
      def reduce({unquote(operation), var!(q), true}), do: unquote(result)
    end
  end

  defmacro deftrue(operation, result, result2) do
    quote do
      def reduce({unquote(operation), true, var!(q)}), do: unquote(result)
      def reduce({unquote(operation), var!(q), true}), do: unquote(result2)
    end
  end

  defmacro deffalse(operation, result) do
    quote do
      def reduce({unquote(operation), false, var!(q)}), do: unquote(result)
      def reduce({unquote(operation), var!(q), false}), do: unquote(result)
    end
  end

  defmacro deffalse(operation, result, result2) do
    quote do
      def reduce({unquote(operation), false, var!(q)}), do: unquote(result)
      def reduce({unquote(operation), var!(q), false}), do: unquote(result2)
    end
  end
end

defmodule TermElimination do
  import TermMacros

  import Kernel,
    except: [
      &&: 2,
      ||: 2
    ]

  def x && y, do: {:conj, x, y}
  def x || y, do: {:disj, x, y}
  def x ~> y, do: {:impl, x, y}
  def x <~> y, do: {:iff, x, y}
  def x <|> y, do: {:xor, x, y}

  deftrue(:conj, q)
  deftrue(:disj, true)
  deftrue(:iff, q)
  deftrue(:xor, {:not, q})
  deftrue(:impl, q, true)
  deffalse(:conj, false)
  deffalse(:disj, q)
  deffalse(:iff, {:not, q})
  deffalse(:xor, q)
  deffalse(:impl, true, {:not, q})

  def reduce({op, l, r}), do: {op, reduce(l), reduce(r)}
  def reduce(t), do: t

  def assert(t, p), do: replace(t, p, true) |> reduce()
  def refute(t, p), do: replace(t, p, false) |> reduce()

  defp replace(p, p, token), do: token
  defp replace({op, l, r}, p, token), do: {op, replace(l, p, token), replace(r, p, token)}
  defp replace(q, p, _), do: q
end
