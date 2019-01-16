defmodule Pantagruel.Bool.Slurp do
  @moduledoc """
  Logic to combine the lines of a Pantagruel program into a single
  Boolean expression.

  refinement:
  x, P <- y
  x, Q <- z
  x, R <- a

  assume: P
  ~Q and ~R and x <- y

  assume: Q
  ~P and ~R and x <- z

  slurp: x, P <- y
  (P or Q or R)
    and
  ((P and (x <- y)) or (Q and (x <- z)) or (R and (x <- a)))

  x <- (
    P \ y,
    Q \ z,
    R \ a
  )
  x <- P \ y
  x <- y
  """
  import Kernel, except: [&&: 2, ||: 2]
  import BoolAlg
  import Pantagruel.Macros

  use Witchcraft

  @doc """
  Combine the lines of each chapter section in a program into single
  tree-expressions.
  """
  def slurp({:program, [module, imports, chapters]}) do
    chapters = lift(chapters, &slurp/1) |> BoolAlg.reduce_all()
    {:program, [module, imports, chapters]}
  end

  # Ingest successive lines into a single boolean expression. Expressions
  # beginning with `:or` are combined with a disjunction; other lines are
  # combined with a conjunction.
  def slurp({:chapter, [hd, body]}) do
    body = [Enum.reduce(body, true, &slurp/2)]
    {:chapter, [hd, body]}
  end

  def slurp({:comment, _}, expression), do: expression
  def slurp(exp(:or, term), expression), do: expression || term
  def slurp(exp(_, term), expression), do: expression && term

  def slurp({:refinement, [pattern, case_exps]}, expression) do
    {case_disjunction, refinement_disjunction} =
      Enum.reduce(case_exps, {false, false}, &slurp_case_exps(&1, &2, pattern))

    expression && (case_disjunction && refinement_disjunction)
  end

  def slurp(term, expression), do: expression && term

  defp slurp_case_exps({:case_exp, [nil, exp]}, acc, pattern),
    do: slurp_case_exps({:case_exp, [true, exp]}, acc, pattern)

  defp slurp_case_exps({:case_exp, [the_case, exp]}, {case_disj, exp_disj}, pattern) do
    case_disj = case_disj || the_case
    case_refinement = the_case && bare_refinement(pattern, exp)
    {case_disj, exp_disj || case_refinement}
  end

  defp bare_refinement(pattern, exp),
    do:
      {:refinement,
       [
         pattern,
         [{:case_exp, [true, exp]}]
       ]}
end
