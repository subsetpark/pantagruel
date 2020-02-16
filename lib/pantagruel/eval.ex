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

  import Pantagruel.Macros

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
  defstruct env: Env.new(), header_unbounds: MapSet.new(), unbounds: [MapSet.new()]
  @type t :: %__MODULE__{env: Env.t(), header_unbounds: MapSet.t(), unbounds: [MapSet.t()]}
  @type error :: {:missing_import, any} | {:unbound_variables, MapSet.t(), Env.t()}

  @doc """
  Evaluate a Pantagruel AST for variable binding, returning the resulting
  environment of all bound symbols.
  """
  @spec eval(Pantagruel.Parse.t(), any, Keyword.t()) :: {:ok, [Env.t()]} | {:error, error}
  def eval({:program, [module_name, imports, chapters]}, available_asts, opts \\ []) do
    # Set the current module name. Either from an argument to the function
    # or, if no name was specified, from a module name in the AST.
    mod_name = opts[:mod_name] || module_name

    with {:ok, env} <- handle_imports(imports, available_asts),
         %__MODULE__{env: env, unbounds: [unbound]} <-
           Enum.reduce(chapters, new_state(env), &eval_chapter(&1, &2, mod_name)),
         # Force a check for unbound values. All symbols must be bound by
         # the end of the program; thus the final chapter cannot introduce
         # any unbound symbols.
         :ok <- Env.check_unbound(env, unbound) do
      {:ok, Enum.reverse(env)}
    end
  end

  # Evaluate a single chapter:
  # 1. evaluate the head
  # 2. check for any unbound symbols in the head
  # 3. evaluate the body
  # 4. check for any unbound symbols from the *previous* chapter body (ie,
  # which should have been defined in this chapter)
  defp eval_chapter(_, {:error, _} = e, _), do: e

  defp eval_chapter({:chapter, [head, body]}, %__MODULE__{} = state, mod_name) do
    %{env: env, header_unbounds: header_unbounds} =
      state =
      state
      |> new_state(mod_name)
      |> eval_head(head)

    with :ok <- Env.check_unbound(env, header_unbounds),
         %{env: env, unbounds: [unbounds | rest]} = state <- eval_body(state, body),
         :ok <- Env.check_unbound(env, unbounds) do
      %{state | unbounds: rest}
    end
  end

  defp handle_imports(
         imports,
         available_asts,
         env \\ [],
         seen_mod_names \\ MapSet.new()
       )

  defp handle_imports(
         [mod_name | rest],
         available_asts,
         env,
         seen_mod_names
       ) do
    with false <- MapSet.member?(seen_mod_names, mod_name),
         %{^mod_name => ast} <- available_asts do
      {:ok, evaled} = eval(ast, available_asts, mod_name: mod_name)

      handle_imports(
        rest,
        available_asts,
        evaled ++ env,
        MapSet.put(seen_mod_names, mod_name)
      )
    else
      true ->
        handle_imports(rest, available_asts, env, seen_mod_names)

      %{} ->
        {:error, {:missing_import, mod_name}}
    end
  end

  defp handle_imports([], _, env, _), do: {:ok, env}

  defp eval_head(%__MODULE__{} = state, head),
    do: Enum.reduce(head, state, &eval_header_statement/2)

  defp eval_body(%__MODULE__{} = state, body),
    do: Enum.reduce(body, state, &eval_body_statement/2)

  # Include symbols in a set of values to check for binding.
  @spec include_for_binding_check(MapSet.t(), any) :: MapSet.t()
  defp include_for_binding_check(unbounds, variables) when is_list(variables),
    do: variables |> List.flatten() |> MapSet.new() |> MapSet.union(unbounds)

  defp include_for_binding_check(unbounds, variable),
    do: include_for_binding_check(unbounds, [variable])

  # Evaluate the statement types found in chapter headers. Header statements
  # come in three forms:
  # 1. Comments, which are ignored;
  defp eval_header_statement({:comment, _}, %__MODULE__{} = state), do: state
  # 2. Domain aliases, which bind symbols as aliases to concrete domains;
  defp eval_header_statement(
         {:alias, [alias_names, alias_exp]},
         %__MODULE__{
           env: [current_scope | env],
           header_unbounds: header_unbounds
         } = state
       ) do
    current_scope =
      Enum.reduce(
        alias_names,
        current_scope,
        &Env.bind(&2, &1, %Domain{name: &1, ref: alias_exp})
      )

    header_unbounds = include_for_binding_check(header_unbounds, alias_exp)
    %{state | env: [current_scope | env], header_unbounds: header_unbounds}
  end

  # 3. Function declarations, which bind new functions into scope.
  defp eval_header_statement(
         {:decl, [_, bindings, _, codomain] = decl},
         %__MODULE__{env: [current_scope | env], header_unbounds: header_unbounds} = state
       ) do
    {binding_pairs, variable_references} = Env.extract_binding_symbols(bindings)
    {_, doms} = Enum.unzip(binding_pairs)

    # Check for binding: domain, predicate, and codomain.
    header_unbounds =
      include_for_binding_check(header_unbounds, [doms, variable_references, codomain])

    %{state | env: [Env.bind_lambda(current_scope, decl) | env], header_unbounds: header_unbounds}
  end

  # Evaluate a line of a chapter body. Body statements can be either
  # comments, which are ignored, or expression statements.
  # 1. Comments, which are ignored;
  defp eval_body_statement({:comment, _}, state), do: state

  # 2. Expressions;
  defp eval_body_statement(
         exp(_, expr),
         %__MODULE__{
           env: [current_scope | env],
           unbounds: [current_unbounds, next_unbounds | rest]
         } = state
       ) do
    # Include any introduced symbols into scope.
    current_scope = Env.bind_expression_variables(current_scope, expr)
    # Include all symbols into the binding check for the *next* chapter.
    next_unbounds = include_for_binding_check(next_unbounds, expr)

    %{
      state
      | env: [current_scope | env],
        unbounds: [current_unbounds, next_unbounds | rest]
    }
  end

  # 3. Refinements, which are guarded patterns with refining expressions.
  defp eval_body_statement(
         {:refinement, [patt, case_exprs]},
         %__MODULE__{
           env: [current_scope | env],
           unbounds: [current_unbounds, next_unbounds | rest]
         } = state
       ) do
    # Include all symbols into the binding check for the *next* chapter.
    next_unbounds =
      case_exprs
      |> Enum.reduce(next_unbounds, &include_for_binding_check(&2, &1))
      |> include_for_binding_check(patt)

    %{
      state
      | env: [current_scope | env],
        unbounds: [current_unbounds, next_unbounds | rest]
    }
  end

  # Extend the environment for a new chapter.
  @spec new_state([%{}]) :: t
  defp new_state(env), do: %__MODULE__{env: env}

  @spec new_state(t, atom() | nil) :: t
  defp new_state(
         %__MODULE__{env: env, unbounds: unbounds} = state,
         mod_name
       ) do
    %{state | env: Env.extend(env, mod_name), unbounds: unbounds ++ [MapSet.new()]}
  end
end
