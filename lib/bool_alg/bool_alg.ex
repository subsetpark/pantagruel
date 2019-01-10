import TypeClass

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

defmodule BoolAlg do
  import TermMacros

  import Kernel,
    except: [
      &&: 2,
      ||: 2
    ]

  defstruct op: nil, x: nil, y: nil

  def x && y, do: %__MODULE__{op: :conj, x: x, y: y}
  def x || y, do: %__MODULE__{op: :disj, x: x, y: y}
  def x ~> y, do: %__MODULE__{op: :impl, x: x, y: y}
  def x <~> y, do: %__MODULE__{op: :iff, x: x, y: y}
  def x <|> y, do: %__MODULE__{op: :xor, x: x, y: y}

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

defimpl TypeClass.Property.Generator, for: BoolAlg do
  alias TypeClass.Property.Generator

  def generate(_) do
    %BoolAlg{op: Enum.random([:conj, :disj, :iff, :xor, :impl]), x: _node(), y: _node()}
  end

  defp _node do
    Enum.random([
      fn -> [Enum.random(?A..?Z)] |> :erlang.list_to_atom() end,
      fn -> Generator.generate(%BoolAlg{}) end
    ]).()
  end
end

definst Witchcraft.Functor, for: BoolAlg do
  def map(%{op: op, x: x, y: y}, fun), do: %BoolAlg{op: op, x: fun.(x), y: fun.(y)}
  def map(%{op: :not, x: x}, fun), do: %BoolAlg{op: :not, x: fun.(x)}
end
