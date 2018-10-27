defmodule Pantagruel.Eval do
  @moduledoc """
  Evaluation of a Pantagruel program.

  Evaluation of a Pantagruel AST consists of walking the tree, binding any
  symbols that are introduced in binding forms (function declarations,
  quantifiers, domain aliases), and checking that all symbols have been
  properly bound.
  """
  import Pantagruel.Guards
  alias Pantagruel.Values.Domain
  alias Pantagruel.Env

  @typedoc """
  A Pantagruel AST is a sequence of *sections*, each of which consists
  of a header and optional body.

  The state of a running Pantagruel program as of any section consists
  of three elements:

  - The execution environment, a list of scopes, one for each section. The
  head of the environment corresponds to the current section. At any
  given section, any symbol introduced in that section will be bound
  into the scope at the head of the environment.

  - The header symbols to check for boundness. Any symbol used in a
  section header that is not in a declaration position is included here
  to be checked for boundness. Every symbol used in a header must be
  bound *by the end of that header*.

  - The body symbols to check for boundness. Any symbol used in a
  section body that is not in a declaration position is included here
  to be checked for boundness. Every symbol used in a body must be
  bound *by the end of the _following_ body*. Unlike with headers,
  symbols *may* be referred to before they are defined, but they must
  be defined in the following section at the latest.
  """
  @type t :: {Env.t(), MapSet.t(), [MapSet.t()]}

  @doc """
  Evaluate a Pantagruel AST for variable binding, returning the resulting
  environment of all bound symbols.
  """
  def eval(program) do
    try do
      eval_section = fn {:section, section}, state ->
        # Evaluate a single section:
        # 1. evaluate the head
        {scopes, header_unbounds, unbounds} =
          Enum.reduce(section[:head], new_state(state), &eval_header_statement/2)

        # 2. check for any unbound symbols in the head
        :ok = Env.check_unbound(scopes, header_unbounds)

        # 3. evaluate the body
        {scopes, [unbounds | rest]} =
          Enum.reduce(section[:body] || [], {scopes, unbounds}, &eval_body_statement/2)

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

      scopes =
        program
        |> Enum.reduce({[], MapSet.new(), [MapSet.new()]}, eval_section)
        |> final_check.()
        |> Enum.reverse()

      {:ok, scopes}
    rescue
      e in Env.UnboundVariablesError -> {:error, {:unbound_variables, e}}
    end
  end

  # Include symbols in a set of values to check for binding.
  @spec include_for_binding_check(MapSet.t(), any) :: MapSet.t()
  defp include_for_binding_check(unbounds, {e, contents}) when is_container(e),
    do: include_for_binding_check(unbounds, contents)

  defp include_for_binding_check(unbounds, variables),
    do: MapSet.union(unbounds, variables |> MapSet.new())

  # Evaluate the statement types ("declarations") found in section
  # headers. Header statements come in three forms:
  # 1. Comments, which are ignored;
  defp eval_header_statement({:comment, _}, state), do: state
  # 2. Domain aliases, which bind symbols as aliases to concrete domains;
  defp eval_header_statement(
         {:alias, [alias_name: alias_names, alias_expr: alias_exp]},
         {[scope | scopes], header_unbounds, unbounds}
       ) do
    scope =
      alias_names
      |> Enum.reduce(scope, fn name, scope ->
        Env.bind(scope, name, %Domain{name: name, ref: alias_exp})
      end)

    header_unbounds = include_for_binding_check(header_unbounds, [alias_exp])
    {[scope | scopes], header_unbounds, unbounds}
  end

  # 3. Function declarations, which bind new functions into scope.
  defp eval_header_statement({:decl, declaration}, {[scope | scopes], header_unbounds, unbounds}) do
    scope = Env.bind_lambda(scope, declaration)
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

  # Evaluate a line of a section body. Body statements can be either
  # comments, which are ignored, or expression statements.
  defp eval_body_statement({:comment, _}, state), do: state
  defp eval_body_statement({:expr, line}, state), do: eval_expression(line, state)

  defp eval_expression(expression, {
         [scope | scopes],
         [unbounds, next_unbounds | rest]
       }) do
    elements =
      case expression do
        [refinement: refinement] ->
          [refinement[:pattern], refinement[:guard], refinement[:expr]] |> Enum.filter(& &1)

        e ->
          e
      end

    # Include any introduced symbols into scope.
    scope = Enum.reduce(elements, scope, &bind_expression_variables/2)
    # Include all symbols into the binding check for the *next* section.
    next_unbounds = include_for_binding_check(next_unbounds, elements)

    {[scope | scopes], [unbounds, next_unbounds | rest]}
  end

  # Existence quantifiers don't just introduce variables for the scope of
  # their predicates; the introduce variables into global scope.
  defp bind_expression_variables(
         {:quantification, quantifier: :exists, quant_bindings: bindings, quant_expression: expr},
         state
       ) do
    bound =
      bindings
      |> Enum.reduce(state, fn {:binding, [bind_symbol: x, bind_op: _, bind_domain: domain]}, s ->
        Env.bind(s, x, domain)
      end)

    bind_expression_variables(expr, bound)
  end

  # In this respect they're unique among expression types.
  defp bind_expression_variables(_, state), do: state
  # Extend the environment for a new section.
  @spec new_state(t) :: t
  defp new_state({scopes, header_unbounds, unbounds}) do
    {[%{} | scopes], header_unbounds, unbounds ++ [MapSet.new()]}
  end
end
