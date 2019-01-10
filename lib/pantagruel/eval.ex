defmodule Pantagruel.Eval do
  @moduledoc """
  Evaluation of a Pantagruel program.

  Evaluation of a Pantagruel AST consists of walking the tree, binding any
  symbols that are introduced in binding forms (function declarations,
  quantifiers, domain aliases), and checking that all symbols have been
  properly bound.
  """
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
  @type error :: {:missing_import, any} | {:unbound_variables, any}

  @doc """
  Evaluate a Pantagruel AST for variable binding, returning the resulting
  environment of all bound symbols.
  """
  @spec eval(any, any, Keyword.t()) :: {:ok, [Env.t()]} | {:error, error}
  def eval({:program, [module_name, imports, chapters]}, available_asts, opts \\ []) do
    # Set the current module name. Either from an argument to the function
    # or, if no name was specified, from a module name in the AST.
    mod_name =
      case {opts[:mod_name], module_name} do
        {nil, name} -> name
        {name, _} -> name
      end

    with {:ok, scopes} <- handle_imports(imports, available_asts, [], MapSet.new()),
         {scopes, _, [unbound]} <-
           Enum.reduce(
             chapters,
             {scopes, MapSet.new(), [MapSet.new()]},
             &eval_chapter(&1, &2, mod_name)
           ),
         # Force a check for unbound values. All symbols must be bound by the
         # end of the program; thus the final chapter cannot introduce any
         # unbound symbols.
         :ok <- Env.check_unbound(scopes, unbound) do
      {:ok, Enum.reverse(scopes)}
    end
  end

  # Evaluate a single chapter:
  # 1. evaluate the head
  # 2. check for any unbound symbols in the head
  # 3. evaluate the body
  # 4. check for any unbound symbols from the *previous* chapter body (ie,
  # which should have been defined in this chapter)
  defp eval_chapter(_, {:error, _} = e, _), do: e

  defp eval_chapter({:chapter, [head, body]}, state, mod_name) do
    {state, header_unbounds, unbounds} =
      new_state(state, mod_name)
      |> eval_head(head)

    with :ok <- Env.check_unbound(state, header_unbounds),
         {state, [unbounds | rest]} <- eval_body(body, state, unbounds),
         :ok <- Env.check_unbound(state, unbounds) do
      {state, MapSet.new(), rest}
    end
  end

  defp handle_imports(
         [mod_name | rest],
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
        {:error, {:missing_import, mod_name}}
    end
  end

  defp handle_imports([], _, scopes, _), do: {:ok, scopes}

  defp eval_head(state, head), do: Enum.reduce(head, state, &eval_header_statement/2)

  defp eval_body(body, state, unbounds),
    do: Enum.reduce(body, {state, unbounds}, &eval_body_statement/2)

  # Include symbols in a set of values to check for binding.
  @spec include_for_binding_check(any, MapSet.t()) :: MapSet.t()
  defp include_for_binding_check(variables, unbounds) when is_list(variables) do
    MapSet.union(unbounds, variables |> MapSet.new())
  end

  defp include_for_binding_check(variables, unbounds),
    do: MapSet.union(unbounds, [variables] |> MapSet.new())

  # Evaluate the statement types found in chapter headers. Header statements
  # come in three forms:
  # 1. Comments, which are ignored;
  defp eval_header_statement({:comment, _}, state), do: state
  # 2. Domain aliases, which bind symbols as aliases to concrete domains;
  defp eval_header_statement(
         {:alias, [alias_names, alias_exp]},
         {[scope | scopes], header_unbounds, unbounds}
       ) do
    scope =
      alias_names
      |> Enum.reduce(scope, fn name, scope ->
        Env.bind(scope, name, %Domain{name: name, ref: alias_exp})
      end)

    header_unbounds =
      [alias_exp]
      |> include_for_binding_check(header_unbounds)

    {[scope | scopes], header_unbounds, unbounds}
  end

  # 3. Function declarations, which bind new functions into scope.
  defp eval_header_statement(
         {:decl, [_, bindings, _, codomain] = decl},
         {[scope | scopes], header_unbounds, unbounds}
       ) do
    scope = Env.bind_lambda(scope, decl)
    # Check for binding: domain, predicate, and codomain.
    {binding_pairs, variable_references} =
      bindings
      |> Enum.reduce({[], []}, &Env.extract_binding_symbols/2)

    doms = Enum.map(binding_pairs, &elem(&1, 1))

    header_unbounds =
      [doms, variable_references, codomain]
      |> List.flatten()
      |> include_for_binding_check(header_unbounds)

    {[scope | scopes], header_unbounds, unbounds}
  end

  # Evaluate a line of a chapter body. Body statements can be either
  # comments, which are ignored, or expression statements.
  # 1. Comments, which are ignored;
  defp eval_body_statement({:comment, _}, state), do: state

  # 2. Expressions;
  defp eval_body_statement({:expr, [_, expr]}, {
         [scope | scopes],
         [unbounds, next_unbounds | rest]
       }) do
    # Include any introduced symbols into scope.
    scope = Env.bind_expression_variables(expr, scope)
    # Include all symbols into the binding check for the *next* chapter.
    next_unbounds = include_for_binding_check(expr, next_unbounds)

    {[scope | scopes], [unbounds, next_unbounds | rest]}
  end

  # 3. Refinements, which are guarded patterns with refining expressions.
  defp eval_body_statement({:refinement, [_, _, expr]} = r, {
         [scope | scopes],
         [unbounds, next_unbounds | rest]
       }) do
    # Include any introduced symbols into scope.
    scope = Env.bind_expression_variables(expr, scope)
    # Include all symbols into the binding check for the *next* chapter.
    next_unbounds = include_for_binding_check(r, next_unbounds)

    {[scope | scopes], [unbounds, next_unbounds | rest]}
  end

  # Extend the environment for a new chapter.
  @spec new_state(t, :atom | nil) :: t
  defp new_state({scopes, header_unbounds, unbounds}, mod_name) do
    scope = if(is_nil(mod_name), do: %{}, else: %{__module__: %Module{name: mod_name}})
    {[scope | scopes], header_unbounds, unbounds ++ [MapSet.new()]}
  end
end
