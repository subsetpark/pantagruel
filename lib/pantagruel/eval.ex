defmodule Pantagruel.Eval do
  alias Pantagruel.Eval.Lambda
  alias Pantagruel.Env

  defp bind_subexpression_variables({:quantifier, [:exists, bindings, expr]}, state) do
    bound =
      bindings
      |> Enum.reduce(state, fn [ident, _, domain], state2 ->
        Env.bind(state2, ident, domain)
      end)

    bind_subexpression_variables(expr, bound)
  end

  defp bind_subexpression_variables(_, state), do: state

  defp bind_expression_variables(state, expression) do
    (expression[:left] || [] ++ expression[:right] || [])
    |> Enum.reduce(state, &bind_subexpression_variables/2)
  end

  defp eval_declaration({:comment, _}, state), do: state
  # Bind all variables introduced in a function declaration and keep track
  # of unbound ones.
  defp eval_declaration({:decl, declaration}, {[scope | scopes], header_unbounds, unbounds}) do
    # Add any newly introduced variables to our set
    # of unbound variables and filter out any that have been
    # bound in the environment.
    # If this is a yielding function, check the codomain for binding.
    scope = Lambda.bind(scope, declaration)

    header_unbounds =
      case {declaration[:yield_type], declaration[:yield_domain]} do
        {:function, dom} when dom -> [dom]
        _ -> []
      end
      # If there is any precondition associated with the function, check
      # the symbols there for binding.
      |> Enum.concat(declaration[:expr][:right] || [])
      |> Enum.concat(declaration[:lambda_doms] || [])
      |> include_unbound_variables(header_unbounds)

    {[scope | scopes], header_unbounds, unbounds}
  end

  defp eval_body_expression({:comment, _}, state), do: state
  # Evaluate a section body expression. Track any unbound variables.
  defp eval_body_expression({:expr, expression}, {
         [scope | scopes],
         [unbounds, next_unbounds | rest]
       }) do
    scope = bind_expression_variables(scope, expression)

    next_unbounds =
      expression[:left] ||
        ([] ++ expression[:right])
        |> include_unbound_variables(next_unbounds)

    {[scope | scopes], [unbounds, next_unbounds | rest]}
  end

  # Evaluate a single section: evaluate the head, evaluate the body,
  # then check for any unbound variables.
  defp eval_section({:section, section}, state) do
    {scopes, header_unbounds, unbounds} =
      Enum.reduce(section[:head], new_state(state), &eval_declaration/2)

    :ok = Env.check_unbound(scopes, header_unbounds)

    {scopes, [unbounds | rest]} =
      Enum.reduce(section[:body] || [], {scopes, unbounds}, &eval_body_expression/2)

    :ok = Env.check_unbound(scopes, unbounds)

    {scopes, MapSet.new(), rest}
  end

  @doc """
  Evaluate a Pantagruel AST for variable binding correctness.
  """
  def eval(program) do
    # Force a check for unbound values. All symbols must be bound by the
    # end of the program; thus the final section cannot introduce any
    # unbound symbols.
    final_check = fn {scopes, _, [unbound]} ->
      :ok = Env.check_unbound(scopes, unbound)
      scopes
    end

    program
    |> Enum.reduce({[], MapSet.new(), [MapSet.new()]}, &eval_section/2)
    |> final_check.()
    |> Enum.reverse()
  end

  defp new_state({scopes, header_unbounds, unbounds}) do
    {[%{} | scopes], header_unbounds, unbounds ++ [MapSet.new()]}
  end

  def include_unbound_variables(variables, unbounds),
    do: MapSet.union(unbounds, MapSet.new(variables))
end
