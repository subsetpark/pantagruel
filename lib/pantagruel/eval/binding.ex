defmodule Pantagruel.Eval.Binding do
  alias Pantagruel.Eval.{Variable, Scope}

  @starting_environment %{
    "Real" => %Variable{name: "ℝ", domain: "ℝ"},
    "Int" => %Variable{name: "ℤ", domain: "ℤ"},
    "Nat" => %Variable{name: "ℕ", domain: "ℕ"},
    "Nat0" => %Variable{name: "ℕ0", domain: "ℕ0"},
    :equals => %Variable{name: "==", domain: "ℝ"},
    :gt => %Variable{name: ">", domain: "ℝ"},
    :lt => %Variable{name: "<", domain: "ℝ"},
    :gte => %Variable{name: ">=", domain: "ℝ"},
    :lte => %Variable{name: "<=", domain: "ℝ"},
    "+" => %Variable{name: "+", domain: "ℝ"},
    "-" => %Variable{name: "-", domain: "ℝ"},
    "^" => %Variable{name: "^", domain: "ℝ"},
    :in => %Variable{name: ":", domain: "⊤"},
    :iff => %Variable{name: "=", domain: "𝔹"},
    :then => %Variable{name: "→", domain: "𝔹"}
  }

  defmodule UnboundVariablesError do
    defexception message: "Unbound variables remain", unbound: MapSet.new()
  end

  @doc """
  Decide if a variable is bound within a given state.
  """
  defp is_bound?(v, s), do: is_bound?(v, s, true)

  @doc """
  Boundness checking for literals.
  """
  defp is_bound?(v, _, _) when is_integer(v), do: true
  defp is_bound?(v, _, _) when is_float(v), do: true
  defp is_bound?([?` | _], _, _), do: true

  @doc """
  A non-value is always unbound within a null state.
  """
  defp is_bound?(_, nil, _), do: false

  @container_types [:string, :bunch, :set, :list]
  @doc """
  Boundness checking for container types.
  """
  defp is_bound?({container, []}, _, _)
       when container in @container_types,
       do: true

  defp is_bound?({container, contents}, scope, should_recurse)
       when container in @container_types do
    Enum.all?(contents, fn
      container_item when is_list(container_item) ->
        Enum.all?(container_item, &is_bound?(&1, scope, should_recurse))

      container_item ->
        is_bound?(container_item, scope, should_recurse)
    end)
  end

  @doc """
  Boundness checking for functions.
  """
  defp is_bound?({:lambda, lambda}, scope, should_recurse) do
    [
      lambda[:decl_doms] || [],
      lambda[:yield_domain] || [],
      lambda[:expr][:left] || [],
      lambda[:expr][:right] || []
    ]
    |> List.flatten()
    |> Enum.all?(
      &is_bound?(
        &1,
        # Lambdas introduce function arguments. Therefore they are bound
        # in (and only in) the recursive boundness check.
        Pantagruel.Eval.bind_lambda_args(scope, lambda),
        should_recurse
      )
    )
  end

  @doc """
  Boundness checking for for-all quantifiers.
  """
  defp is_bound?({:forall, [symbol, _, domain, expr]}, scope, should_recurse) do
    Enum.all?(
      [symbol, domain | expr],
      &is_bound?(
        &1,
        # Foralls introduce function arguments. Therefore they are bound
        # in (and only in) the recursive boundness check.
        Scope.bind(scope, symbol, domain),
        should_recurse
      )
    )
  end

  defp is_bound?({:exists, [symbol, _, domain, expr]}, scope, should_recurse) do
    Enum.all?(
      [symbol, domain | expr],
      &is_bound?(&1, scope, should_recurse)
    )
  end

  @doc """
  A non-composite value is bound if it's present in the current scope or
  the previous one. This allows for a flow where variables are referred
  to in one scope and then specified with :where.
  """
  defp is_bound?(variable, scope, should_recurse) do
    Map.has_key?(@starting_environment, variable) or Map.has_key?(scope.bindings, variable) or
      (should_recurse and is_bound?(variable, scope.parent, false))
  end

  @doc """
  Include new values into the data structures tracking unbound variables.
  """
  def include_and_filter_unbounds(variables, {scope, global_unbound, head_unbound}, scope_source) do
    is_unbound? = &(not is_bound?(&1, scope))
    union = &MapSet.union(&1, MapSet.new(variables))

    filter =
      &(&1
        |> Enum.filter(is_unbound?)
        |> MapSet.new())

    case scope_source do
      :head ->
        {
          scope,
          global_unbound |> filter.(),
          head_unbound |> union.() |> filter.()
        }

      :body ->
        {
          scope,
          global_unbound |> union.() |> filter.(),
          head_unbound |> filter.()
        }
    end
  end

  def check_unbound({scope, global_unbound, head_unbound} = state, force \\ false) do
    cond do
      MapSet.size(head_unbound) > 0 ->
        raise UnboundVariablesError, unbound: head_unbound

      (scope.parent || force) && MapSet.size(global_unbound) > 0 ->
        raise UnboundVariablesError, unbound: global_unbound

      true ->
        state
    end
  end
end
