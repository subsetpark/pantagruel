defmodule Pantagruel.Bool.Slurp do
  @moduledoc """
  Logic to combine the lines of a Pantagruel program into a single Boolean
  expression.
  """
  import Kernel, except: [&&: 2, ||: 2]
  import BoolAlg

  @doc """
  Combine the lines of each chapter section in a program into single
  tree-expressions.
  """
  def slurp({:program, [module, imports, chapters]}) do
    chapters = Enum.map(chapters, &slurp/1)
    {:program, [module, imports, chapters]}
  end
  # Ingest successive lines into a single boolean expression. Expressions
  # beginning with `:or` are combined with a disjunction; other lines are
  # combined with a conjunction.
  def slurp({:chapter, chapter}), do: {:chapter, Enum.map(chapter, &slurp/1)}
  def slurp(section), do: [Enum.reduce(section, true, &slurp/2)]
  def slurp({:expr, [:or, term]}, expression), do: expression || term
  def slurp({:expr, [_, term]}, expression), do: expression && term
  def slurp(term, expression), do: expression && term
end
