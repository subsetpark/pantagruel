import TypeClass

defimpl TypeClass.Property.Generator, for: BoolAlg do
  @moduledoc """
  Property generation for BoolAlg structs.
  """
  def generate(_) do
    op = Enum.random([:conj, :disj, :iff, :xor, :impl, :not])
    %BoolAlg{op: op, x: rand_prop(), y: rand_prop()}
  end

  defp rand_prop, do: [Enum.random(?A..?Z)] |> :erlang.list_to_atom()
end

definst Witchcraft.Functor, for: BoolAlg do
  @moduledoc """
  Implementation of the Functor typeclass for BoolAlg structs.
  """
  def map(%{x: x, y: y} = b, fun), do: %{b | x: fun.(x), y: fun.(y)}
  def map(%{op: :not, x: x} = b, fun), do: %{b | x: fun.(x)}
end
