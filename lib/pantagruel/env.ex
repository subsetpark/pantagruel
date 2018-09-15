defmodule Pantagruel.Env do
  @type scope :: map()
  @type t :: [scope]
  alias Pantagruel.Eval.Variable

  defmodule UnboundVariablesError do
    defexception message: "Unbound variables remain", unbound: MapSet.new(), scopes: []
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
    :plus => %Variable{name: "+", domain: "ℝ"},
    :minus => %Variable{name: "−", domain: "ℝ"},
    :times => %Variable{name: "×", domain: "ℝ"},
    :divides => %Variable{name: "÷", domain: "ℝ"},
    :exp => %Variable{name: "^", domain: "ℝ"},
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
  @spec bind(scope, any(), any()) :: scope
  def bind(scope, {:bunch, elements}, value) do
    Enum.reduce(elements, scope, &bind(&2, &1, value))
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

  def bind(scope, {name, value}), do: bind(scope, name, value)

  @doc """
  If a value has been defined in the starting environment, find the name
  it was bound under.
  """
  def lookup_binding_name(symbol) when is_list(symbol) do
    Enum.map(symbol, &lookup_binding_name/1)
  end

  def lookup_binding_name(symbol) when is_binary(symbol) or is_atom(symbol) do
    case @starting_environment do
      # Look up symbol name if predefined.
      %{^symbol => variable} -> variable.name
      _ -> symbol
    end
  end

  def lookup_binding_name(expr), do: expr

  @doc """
  Check a list of values for binding in the given scope, and raise if
  anything is unbound.
  """
  @spec check_unbound(t, [any]) :: :ok
  def check_unbound(scopes, candidates) do
    case Enum.filter(candidates, &(!is_bound?(&1, scopes))) do
      [] ->
        :ok

      unbound ->
        raise UnboundVariablesError, unbound: MapSet.new(unbound), scopes: scopes
    end
  end

  @container_types [:string, :bunch, :set, :list]
  # Check whether a given value is currently bound in the given scope.
  def is_bound?(v, _) when is_integer(v), do: true
  def is_bound?(v, _) when is_float(v), do: true
  def is_bound?({:literal, _}, _), do: true
  def is_bound?(_, []), do: false

  def is_bound?({container, []}, _)
      when container in @container_types,
      do: true

  def is_bound?({container, contents}, scope)
      when container in @container_types do
    Enum.all?(contents, fn
      container_item when is_list(container_item) ->
        Enum.all?(container_item, &is_bound?(&1, scope))

      container_item ->
        is_bound?(container_item, scope)
    end)
  end

  def is_bound?({:lambda, lambda}, scope) do
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
  def is_bound?({:quantifier, quantifier}, scope),
    do: check_with_bindings(quantifier[:quant_expression], quantifier[:quant_bindings], scope)

  def is_bound?({:comprehension, [{_, [bindings, expr]}]}, scope),
    do: check_with_bindings(expr, bindings, scope)

  def is_bound?({:intro_op, _}, _), do: true

  def is_bound?({:appl, f: f, x: x}, scopes), do: is_bound?(f, scopes) && is_bound?(x, scopes)

  def is_bound?({:appl, operator: _, x: x, y: y}, scopes),
    do: is_bound?(x, scopes) && is_bound?(y, scopes)

  def is_bound?(variable, [scope | parent]) do
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

  # Process some temporary bindings and check for boundness.
  defp check_with_bindings(expr, bindings, scopes) do
    binding_symbols = Enum.map(bindings, &extract_binding_symbol/1)
    binding_domains = Enum.map(bindings, &extract_binding_domain/1)

    inner_scope =
      Enum.zip(binding_symbols, binding_domains)
      |> Enum.reduce(%{}, &bind(&2, &1))

    scopes = [inner_scope | scopes]
    Enum.all?(binding_symbols, &is_bound?(&1, scopes)) && is_bound?(expr, scopes)
  end

  defp extract_binding_symbol({:appl, [operator: op, x: x, y: _]}) when op in [:from, :in], do: x

  defp extract_binding_domain({:appl, [operator: op, x: _, y: dom]}) do
    cond do
      op in [:from, :in] -> dom
      true -> extract_binding_domain(dom)
    end
  end

  defp has_key?(scope, variable),
    do: Map.has_key?(@starting_environment, variable) or Map.has_key?(scope, variable)
end
