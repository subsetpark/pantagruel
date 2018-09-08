defmodule Pantagruel.Eval.BindingChecks do
  alias Pantagruel.Eval.Scope

  defmodule UnboundVariablesError do
    defexception message: "Unbound variables remain", unbound: MapSet.new()
  end

  # Process some temporary bindings and check for boundness.
  defp check_with_bindings(expr, bindings, scopes) do
    bind_bindings = fn [symbol, _, domain], s ->
      Scope.bind(s, symbol, domain)
    end

    with inner_scope <- Enum.reduce(bindings, %{}, bind_bindings),
         scopes <- [inner_scope | scopes],
         symbols <- for([_, _, domain] <- bindings, do: domain) ++ expr do
      Enum.all?(symbols, &is_bound?(&1, scopes))
    end
  end

  @container_types [:string, :bunch, :set, :list]
  # Decide if a variable is bound within a given state.
  # Boundness checking for literals.
  defp is_bound?(v, _) when is_integer(v), do: true
  defp is_bound?(v, _) when is_float(v), do: true
  defp is_bound?({:literal, _}, _), do: true
  # A non-value is always unbound within a null state.
  defp is_bound?(_, []), do: false
  # Boundness checking for container types.
  defp is_bound?({container, []}, _)
       when container in @container_types,
       do: true

  defp is_bound?({container, contents}, scope)
       when container in @container_types do
    Enum.all?(contents, fn
      container_item when is_list(container_item) ->
        Enum.all?(container_item, &is_bound?(&1, scope))

      container_item ->
        is_bound?(container_item, scope)
    end)
  end

  # Boundness checking for functions.
  defp is_bound?({:lambda, lambda}, scope) do
    # Lambdas introduce function arguments. Therefore they are bound
    # in (and only in) the recursive boundness check.
    scope = [Pantagruel.Eval.Lambda.bind(%{}, lambda) | scope]

    [
      lambda[:lambda_doms] || [],
      lambda[:lambda_codomain] || [],
      lambda[:expr][:left] || [],
      lambda[:expr][:right] || []
    ]
    |> List.flatten()
    |> Enum.all?(&is_bound?(&1, scope))
  end

  # Boundness checking for for-all quantifiers.
  defp is_bound?({:quantifier, [_quantifier, bindings, expr]}, scope) do
    check_with_bindings(expr, bindings, scope)
  end

  defp is_bound?({:comprehension, [{_container, [bindings, expr]}]}, scope) do
    check_with_bindings(expr, bindings, scope)
  end

  # Check if a given variable is bound given the current scope. Search
  # in the scope or starting environment.
  defp is_bound?(variable, [scope | parent]) do
    # Allow arbitrary suffixes or prefixes of "'" to denote
    # successor/remainder variables.
    variable =
      if is_binary(variable) do
        String.trim(variable, "'")
      else
        variable
      end

    Map.has_key?(Scope.starting_environment(), variable) or Map.has_key?(scope, variable) or
      is_bound?(variable, parent)
  end

  def check_unbound(scopes, unbound) do
    case Enum.filter(unbound, &(!is_bound?(&1, scopes))) do
      [] ->
        :ok

      still_unbound ->
        raise UnboundVariablesError, unbound: MapSet.new(still_unbound)
    end
  end
end
