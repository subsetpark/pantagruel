defmodule Pantagruel.Eval.Module do
  defstruct name: ""
end

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
  alias Pantagruel.Eval.Module

  defmodule MissingImportError do
    defexception message: "Requested module not found", mod_name: nil
  end

  @typedoc """
  A Pantagruel AST is a sequence of *chapters*, each of which consists
  of a header and optional body.

  The state of a running Pantagruel program as of any chapter consists
  of three elements:

  - The execution environment, a list of scopes, one for each chapter. The
  head of the environment corresponds to the current chapter. At any
  given chapter, any symbol introduced in that chapter will be bound
  into the scope at the head of the environment.

  - The header symbols to check for boundness. Any symbol used in a
  chapter header that is not in a declaration position is included here
  to be checked for boundness. Every symbol used in a header must be
  bound *by the end of that header*.

  - The body symbols to check for boundness. Any symbol used in a
  chapter body that is not in a declaration position is included here
  to be checked for boundness. Every symbol used in a body must be
  bound *by the end of the _following_ body*. Unlike with headers,
  symbols *may* be referred to before they are defined, but they must
  be defined in the following chapter at the latest.
  """
  @type t :: {Env.t(), MapSet.t(), [MapSet.t()]}

  @doc """
  Evaluate a Pantagruel AST for variable binding, returning the resulting
  environment of all bound symbols.
  """
  def eval(program, available_asts, opts \\ []) do
    # Set the current module name. Either from an argument to the function
    # or, if no name was specified, from a module name in the AST.
    mod_name =
      case {opts[:mod_name], program} do
        {nil, [{:module, name} | _]} -> name
        {name, _} -> name
      end

    try do
      # Populate the scope with any modules imported with the `import` statement.
      scopes = handle_imports(program[:imports], available_asts, [], MapSet.new())
      chapters = program[:chapters] || []

      eval_chapter = fn {:chapter, chapter}, state ->
        # Evaluate a single chapter:
        # 1. evaluate the head
        {state, header_unbounds, unbounds} =
          Enum.reduce(chapter[:head], new_state(state, mod_name), &eval_header_statement/2)

        # 2. check for any unbound symbols in the head
        :ok = Env.check_unbound(state, header_unbounds)

        # 3. evaluate the body
        {state, [unbounds | rest]} =
          Enum.reduce(chapter[:body] || [], {state, unbounds}, &eval_body_statement/2)

        # 4. check for any unbound symbols from the *previous* chapter body (ie,
        #    which should have been defined in this chapter)
        :ok = Env.check_unbound(state, unbounds)

        {state, MapSet.new(), rest}
      end

      # Force a check for unbound values. All symbols must be bound by the
      # end of the program; thus the final chapter cannot introduce any
      # unbound symbols.
      final_check = fn {scopes, _, [unbound]} ->
        :ok = Env.check_unbound(scopes, unbound)
        scopes
      end

      scopes =
        chapters
        |> Enum.reduce({scopes, MapSet.new(), [MapSet.new()]}, eval_chapter)
        |> final_check.()
        |> Enum.reverse()

      {:ok, scopes}
    rescue
      e in Env.UnboundVariablesError -> {:error, {:unbound_variables, e}}
      e in Env.DomainMismatchError -> {:error, {:domain_mismatch, e}}
      e in MissingImportError -> {:error, {:missing_import, e}}
    end
  end

  # Recursively evaluate any imported modules and bring the resulting
  # scopes along.
  defp handle_imports(
         [{:import, mod_name} | rest],
         available_asts,
         scopes,
         seen_mod_names
       ) do
    case {MapSet.member?(seen_mod_names, mod_name), available_asts} do
      {true, _} ->
        # Skip any module that's already been imported.
        handle_imports(rest, available_asts, scopes, seen_mod_names)

      {_, %{^mod_name => ast}} ->
        {:ok, evaled} = eval(ast, available_asts, mod_name: mod_name)

        handle_imports(
          rest,
          available_asts,
          evaled ++ scopes,
          MapSet.put(seen_mod_names, mod_name)
        )

      {_, %{}} ->
        raise MissingImportError, mod_name: mod_name
    end
  end

  defp handle_imports([], _, scopes, _), do: scopes
  defp handle_imports(nil, _, scopes, _), do: scopes

  # Include symbols in a set of values to check for binding.
  @spec include_for_binding_check(MapSet.t(), any) :: MapSet.t()
  defp include_for_binding_check(unbounds, {e, contents}) when is_container(e),
    do: include_for_binding_check(unbounds, contents)

  defp include_for_binding_check(unbounds, variables) when is_tuple(variables),
    do: MapSet.union(unbounds, [variables] |> MapSet.new())

  defp include_for_binding_check(unbounds, variables),
    do: MapSet.union(unbounds, variables |> MapSet.new())

  # Evaluate the statement types ("declarations") found in chapter
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
        | List.flatten(declaration[:lambda_guards] || [])
      ]
      |> Enum.concat(declaration[:lambda_args][:doms] || [])

    header_unbounds = include_for_binding_check(header_unbounds, symbols)

    {[scope | scopes], header_unbounds, unbounds}
  end

  # Evaluate a line of a chapter body. Body statements can be either
  # comments, which are ignored, or expression statements.
  defp eval_body_statement([{:intro_op, _} | clause], state),
    do: eval_body_statement(clause, state)

  defp eval_body_statement({:comment, _}, state), do: state
  defp eval_body_statement([{:expr, line}], state), do: eval_expression(:expr, line, state)

  defp eval_body_statement([{:refinement, [pattern: pattern, expr: expr]}], state) do
    eval_expression(:refinement, [pattern, expr], state)
  end

  defp eval_expression(expr_type, elements, {
         [scope | scopes],
         [unbounds, next_unbounds | rest]
       }) do
    # Include any introduced symbols into scope.
    scope = bind_expression(expr_type, elements, scope)
    # Include all symbols into the binding check for the *next* chapter.
    next_unbounds = include_for_binding_check(next_unbounds, elements)

    {[scope | scopes], [unbounds, next_unbounds | rest]}
  end

  defp bind_expression(:refinement, [pattern, expression], scope) do
    elements = [pattern, expression]
    # Include any introduced symbols into scope.
    Enum.reduce(elements, scope, &bind_expression_variables/2)
  end

  defp bind_expression(:expr, {:quantification, _} = q, scope) do
    bind_expression_variables(q, scope)
  end

  defp bind_expression(:expr, _, scope), do: scope

  # Existence quantifiers don't just introduce variables for the scope of
  # their predicates; the introduce variables into global scope.
  defp bind_expression_variables(
         {:quantification, quantifier: :exists, bindings: bindings, expr: expr},
         scope
       ) do
    bound =
      bindings
      |> Enum.reduce(scope, fn {:binding, [bind_symbol: x, bind_domain: domain]}, s ->
        Env.bind(s, x, domain)
      end)

    bind_expression_variables(expr, bound)
  end

  # In this respect they're unique among expression types.
  defp bind_expression_variables(_, state), do: state
  # Extend the environment for a new chapter.
  @spec new_state(t, :atom | nil) :: t
  defp new_state({scopes, header_unbounds, unbounds}, mod_name) do
    scope = if(is_nil(mod_name), do: %{}, else: %{__module__: %Module{name: mod_name}})
    {[scope | scopes], header_unbounds, unbounds ++ [MapSet.new()]}
  end
end
