import TypeClass

defmodule TermMacros do
  defmacro deftrue(operation, result) do
    quote do
      def reduce(%__MODULE__{
            op: unquote(operation),
            x: true,
            y: var!(q)
          }) do
        unquote(result)
      end

      def reduce(%__MODULE__{
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
      def reduce(%__MODULE__{
            op: unquote(operation),
            x: true,
            y: var!(q)
          }) do
        unquote(result)
      end

      def reduce(%__MODULE__{
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
      def reduce(%__MODULE__{
            op: unquote(operation),
            x: false,
            y: var!(q)
          }) do
        unquote(result)
      end

      def reduce(%__MODULE__{
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
      def reduce(%__MODULE__{
            op: unquote(operation),
            x: false,
            y: var!(q)
          }) do
        unquote(result)
      end

      def reduce(%__MODULE__{
            op: unquote(operation),
            x: var!(q),
            y: false
          }) do
        unquote(result2)
      end
    end
  end
end

defmodule BoolAlg do
  import TermMacros
  use Witchcraft

  import Kernel,
    except: [
      &&: 2,
      ||: 2
    ]

  defstruct op: nil, x: nil, y: nil

  def conj(x, y), do: %__MODULE__{op: :conj, x: x, y: y}
  def x && y, do: conj(x, y)

  def disj(x, y), do: %__MODULE__{op: :disj, x: x, y: y}
  def x || y, do: disj(x, y)

  def impl(x, y), do: %__MODULE__{op: :impl, x: x, y: y}
  def x ~> y, do: impl(x, y)

  def iff(x, y), do: %__MODULE__{op: :iff, x: x, y: y}
  def x <~> y, do: iff(x, y)

  def xor(x, y), do: %__MODULE__{op: :xor, x: x, y: y}
  def x <|> y, do: xor(x, y)

  def nott(x), do: %__MODULE__{op: :not, x: x}

  deftrue(:conj, q)
  deftrue(:disj, true)
  deftrue(:iff, q)
  deftrue(:xor, nott(q))
  deftrue(:impl, q, true)
  deffalse(:conj, false)
  deffalse(:disj, q)
  deffalse(:iff, nott(q))
  deffalse(:xor, q)
  deffalse(:impl, true, nott(q))

  def reduce(%BoolAlg{} = b), do: lift(b, &reduce/1)
  def reduce(t) when is_list(t), do: Enum.map(t, &reduce/1)
  def reduce({tag, values}) when is_list(values), do: {tag, Enum.map(values, &reduce/1)}
  def reduce(t), do: t

  def assert(t, p), do: replace(t, p, true) |> reduce()
  def refute(t, p), do: replace(t, p, false) |> reduce()

  defp replace(p, p, token), do: token

  defp replace(q, p, token) when is_list(q) do
    Enum.map(q, &replace(&1, p, token))
  end

  defp replace(%BoolAlg{} = b, p, token) do
    lift(b, &replace(&1, p, token))
  end

  defp replace({tag, values}, p, token) do
    {tag, Enum.map(values, &replace(&1, p, token))}
  end

  defp replace(q, _, _), do: q
end

defimpl TypeClass.Property.Generator, for: BoolAlg do
  alias TypeClass.Property.Generator

  def generate(_) do
    op = Enum.random([:conj, :disj, :iff, :xor, :impl])
    %BoolAlg{op: op, x: _node(), y: _node()}
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
