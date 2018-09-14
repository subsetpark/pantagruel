defmodule Pantagruel.Eval do
  alias Pantagruel.Eval.{Lambda, Domain}
  alias Pantagruel.Env

  @doc """
  Evaluate a Pantagruel AST for variable binding, returning the resulting
  environment of all bound symbols.
  """
  @spec eval(Pantagruel.Parse.t()) :: Env.t()
  def eval(program) do
    eval_section = fn {:section, section}, state ->
      # Evaluate a single section:
      # 1. evaluate the head
      {scopes, header_unbounds, unbounds} =
        Enum.reduce(section[:head], new_state(state), &eval_declaration/2)

      # 2. check for any unbound symbols in the head
      :ok = Env.check_unbound(scopes, header_unbounds)

      # 3. evaluate the body
      {scopes, [unbounds | rest]} =
        Enum.reduce(section[:body] || [], {scopes, unbounds}, &eval_body_line/2)

      # 4. check for any unbound symbols from the *previous* section body (ie,
      #    which should have been defined in this section)
      :ok = Env.check_unbound(scopes, unbounds)

      {scopes, MapSet.new(), rest}
    end

    # Force a check for unbound values. All symbols must be bound by the
    # end of the program; thus the final section cannot introduce any
    # unbound symbols.
    final_check = fn {scopes, _, [unbound]} ->
      :ok = Env.check_unbound(scopes, unbound)
      scopes
    end

    program
    |> Enum.reduce({[], MapSet.new(), [MapSet.new()]}, eval_section)
    |> final_check.()
    |> Enum.reverse()
  end

  @container_types [:bunch, :set, :list, :string]
  @doc """
  Include symbols in a set of values to check for binding.
  """
  @spec include_for_binding_check(MapSet.t(), any) :: MapSet.t()
  def include_for_binding_check(unbounds, {container, contents})
      when container in @container_types do
    include_for_binding_check(unbounds, List.flatten(contents))
  end

  def include_for_binding_check(unbounds, {:appl, [operator: _, x: x, y: y]}) do
    unbounds
    |> include_for_binding_check(x)
    |> include_for_binding_check(y)
  end

  def include_for_binding_check(unbounds, {:appl, [f: f, x: x]}) do
    unbounds
    |> include_for_binding_check(f)
    |> include_for_binding_check(x)
  end

  def include_for_binding_check(unbounds, variables),
    do: MapSet.union(unbounds, MapSet.new(variables))

  # Evaluate the statement types ("declarations") found in section
  # headers.
  defp eval_declaration({:comment, _}, state), do: state

  defp eval_declaration(
         {:alias, [subexpression, yields]},
         {[scope | scopes], header_unbounds, unbounds}
       ) do
    scope = Domain.bind(scope, subexpression, yields)
    header_unbounds = include_for_binding_check(header_unbounds, subexpression)
    {[scope | scopes], header_unbounds, unbounds}
  end

  defp eval_declaration({:decl, declaration}, {[scope | scopes], header_unbounds, unbounds}) do
    scope = Lambda.bind(scope, declaration)
    # Check for binding: domain, predicate, and codomain.
    symbols =
      [
        # Replace a nil codomain with a dummy value that will always pass.
        declaration[:lambda_codomain] || 0
        | List.flatten(declaration[:predicate] || [])
      ]
      |> Enum.concat(declaration[:lambda_doms] || [])

    header_unbounds = include_for_binding_check(header_unbounds, symbols)

    {[scope | scopes], header_unbounds, unbounds}
  end

  defp eval_body_line({:comment, _}, state), do: state
  # Evaluate a line of a section body.
  defp eval_body_line({:expr, line}, state), do: eval_expression(line, state)

  defp eval_expression(expression, {
         [scope | scopes],
         [unbounds, next_unbounds | rest]
       }) do
    elements =
      case expression do
        [refinement: refinement] ->
          [refinement[:pattern], refinement[:guard], refinement[:subexpr]] |> Enum.filter(& &1)

        e ->
          e
      end

    # Include any introduced symbols into scope.
    scope = Enum.reduce(elements, scope, &bind_subexpression_variables/2)
    # Include all symbols into the binding check for the *next* section.
    next_unbounds = include_for_binding_check(next_unbounds, elements)

    {[scope | scopes], [unbounds, next_unbounds | rest]}
  end

  # Existence quantifiers don't just introduce variables for the scope of
  # their predicates; the introduce variables into global scope.
  defp bind_subexpression_variables(
         {:quantifier, quant_operator: :exists, quant_bindings: bindings, quant_expression: expr},
         state
       ) do
    bound =
      bindings
      |> Enum.reduce(state, fn {:appl, [operator: _, x: x, y: domain]}, s ->
        Env.bind(s, x, domain)
      end)

    bind_subexpression_variables(expr, bound)
  end

  # In this respect they're unique among expression types.
  defp bind_subexpression_variables(_, state), do: state
  # Extend the environment for a new section.
  defp new_state({scopes, header_unbounds, unbounds}) do
    {[%{} | scopes], header_unbounds, unbounds ++ [MapSet.new()]}
  end
end
