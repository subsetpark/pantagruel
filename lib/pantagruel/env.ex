defmodule Pantagruel.Env do
  alias Pantagruel.Eval.Variable

  defmodule UnboundVariablesError do
    defexception message: "Unbound variables remain", unbound: MapSet.new()
  end

  @starting_environment %{
    "Bool" => %Variable{name: "𝔹", domain: "𝔹"},
    "Real" => %Variable{name: "ℝ", domain: "ℝ"},
    "Int" => %Variable{name: "ℤ", domain: "ℤ"},
    "Nat" => %Variable{name: "ℕ", domain: "ℕ"},
    "Nat0" => %Variable{name: "ℕ0", domain: "ℕ0"},
    "String" => %Variable{name: "𝕊", domain: "𝕊"},
    :equals => %Variable{name: "=", domain: "ℝ"},
    :notequals => %Variable{name: "≠", domain: "ℝ"},
    :gt => %Variable{name: ">", domain: "ℝ"},
    :lt => %Variable{name: "<", domain: "ℝ"},
    :gte => %Variable{name: "≥", domain: "ℝ"},
    :lte => %Variable{name: "≤", domain: "ℝ"},
    "+" => %Variable{name: "+", domain: "ℝ"},
    "-" => %Variable{name: "-", domain: "ℝ"},
    "*" => %Variable{name: "×", domain: "ℝ"},
    "^" => %Variable{name: "^", domain: "ℝ"},
    :in => %Variable{name: ":", domain: "⊤"},
    :from => %Variable{name: "∈", domain: "⊤"},
    :iff => %Variable{name: "⇔", domain: "𝔹"},
    :then => %Variable{name: "→", domain: "𝔹"},
    :exists => %Variable{name: "∃", domain: "⊤"},
    :forall => %Variable{name: "∀", domain: "⊤"}
  }

  @doc """
  Introduce a new variable into this scope.
  """
  def bind(scope, {:bunch, elements}, value) do
    Enum.reduce(elements, scope, &bind(&2, hd(&1), value))
  end

  def bind(scope, name, value) do
    to_put =
      case value do
        %{} ->
          value

        domain ->
          %Variable{
            name: name,
            domain: lookup_binding_name(domain)
          }
      end

    Map.put(scope, name, to_put)
  end

  @doc """
  If a value has been defined in the starting environment, find the name
  it was bound under.
  """
  def lookup_binding_name(expr) when is_list(expr) do
    Enum.map(expr, &lookup_binding_name/1)
  end

  def lookup_binding_name(domain) when is_binary(domain) or is_atom(domain) do
    case @starting_environment do
      # Look up domain name if predefined.
      %{^domain => variable} -> variable.name
      _ -> domain
    end
  end

  def lookup_binding_name(expr), do: expr
  # Process some temporary bindings and check for boundness.
  defp check_with_bindings(expr, bindings, scopes) do
    bind_bindings = fn [symbol, _, domain], s ->
      bind(s, symbol, domain)
    end

    with inner_scope <- Enum.reduce(bindings, %{}, bind_bindings),
         scopes <- [inner_scope | scopes],
         symbols <- for([_, _, domain] <- bindings, do: domain) ++ expr do
      Enum.all?(symbols, &is_bound?(&1, scopes))
    end
  end

  @doc """
  Check a list of values for binding in the given scope, and raise if
  anything is unbound.
  """
  def check_unbound(scopes, candidates) do
    case Enum.filter(candidates, &(!is_bound?(&1, scopes))) do
      [] ->
        :ok

      unbound ->
        raise UnboundVariablesError, unbound: MapSet.new(unbound)
    end
  end

  @container_types [:string, :bunch, :set, :list]
  # Check whether a given value is currently bound in the given scope.
  defp is_bound?(v, _) when is_integer(v), do: true
  defp is_bound?(v, _) when is_float(v), do: true
  defp is_bound?({:literal, _}, _), do: true
  defp is_bound?(_, []), do: false

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

  defp is_bound?({:lambda, lambda}, scope) do
    # Lambdas introduce function arguments. Therefore they are bound in
    # (and only in) the recursive boundness check.
    scope = [Pantagruel.Eval.Lambda.bind(%{}, lambda) | scope]

    [
      lambda[:lambda_doms] || [],
      lambda[:lambda_codomain] || [],
      lambda[:predicate] || []
    ]
    |> List.flatten()
    |> Enum.all?(&is_bound?(&1, scope))
  end

  # Boundness checking for for-all quantifiers.
  defp is_bound?({:quantifier, [_, bindings, expr]}, scope),
    do: check_with_bindings(expr, bindings, scope)

  defp is_bound?({:comprehension, [{_, [bindings, expr]}]}, scope),
    do: check_with_bindings(expr, bindings, scope)

  defp is_bound?({:intro_op, _}, _), do: true

  defp is_bound?(variable, [scope | parent]) do
    # Allow arbitrary suffixes or prefixes of "'" to denote
    # successor/remainder variables.
    variable =
      if is_binary(variable) do
        String.trim(variable, "'")
      else
        variable
      end

    has_key?(scope, variable) or is_bound?(variable, parent)
  end

  defp has_key?(scope, variable),
    do: Map.has_key?(@starting_environment, variable) or Map.has_key?(scope, variable)
end
