defmodule Pantagruel.Parse do
  def handle_lex({:ok, tokens, _linecount}), do: :pant_parser.parse(tokens)
end

defmodule Pantagruel.Parse.Node do
  @moduledoc """
  AST Node Struct for Pantagruel.
  """
  defstruct type: nil, values: []
end

import TypeClass

defimpl TypeClass.Property.Generator, for: Pantagruel.Parse.Node do
  @moduledoc """
  Property generation for AST Nodes.
  """
  alias TypeClass.Property.Generator

  def generate(_) do
    %Pantagruel.Parse.Node{type: Generator.generate(:ok), values: Generator.generate([])}
  end
end

definst Witchcraft.Functor, for: Pantagruel.Parse.Node do
  @moduledoc """
  Implementation of the Functor typeclass for BoolAlg structs.
  """
  use Witchcraft
  def map(%{values: values} = b, fun), do: %{b | values: lift(values, fun)}
end

definst Witchcraft.Foldable, for: Pantagruel.Parse.Node do
  @moduledoc """
  Implementation of the Foldable typeclass for BoolAlg structs.
  """
  use Witchcraft

  def right_fold(%{values: values}, acc, f) do
    Enum.reduce(values, acc, f)
  end
end
