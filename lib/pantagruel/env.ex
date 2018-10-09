defmodule Pantagruel.Env do
  @moduledoc """
  The evaluation environment for a Pantagruel program.
  """

  import Pantagruel.Guards
  alias Pantagruel.Eval.Variable

  @type scope :: map()
  @typedoc """
  An environment is a list of binding contexts, one scope for each section
  of the program. Symbols are bound into scope by being referred to in
  one of several declaration forms, at which point they are inserted
  into the scope for that section.
  """
  @type t :: [scope]

  defmodule UnboundVariablesError do
    defexception message: "Unbound variables remain", unbound: MapSet.new(), scopes: []
  end

  defmodule SymbolExtractionError do
    defexception message: "Expected binding expression form", expr: nil, bindings: nil, scopes: []
  end

  defmodule UndefinedAtomError do
    defexception message: "Received atom without string representation", atom: nil
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
    :not => %Variable{name: "¬", domain: "𝔹"},
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
    :iff => %Variable{name: "↔", domain: "𝔹"},
    :then => %Variable{name: "→", domain: "𝔹"},
    :and => %Variable{name: "∧", domain: "𝔹"},
    :or => %Variable{name: "∨", domain: "𝔹"},
    :exists => %Variable{name: "∃", domain: "⊤"},
    :forall => %Variable{name: "∀", domain: "⊤"},
    :card => %Variable{name: "#", domain: "⊤"}
  }

  @doc """
  Introduce a new variable into this scope.
  """
  @spec bind(scope, any(), any()) :: scope
  def bind(scope, {:par, elements}, value) do
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
      _ when is_binary(symbol) or is_nil(symbol) -> symbol
      _ when is_atom(symbol) -> raise UndefinedAtomError, atom: symbol
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

  @doc """
  Return whether a given value is bound in any of: the current scope,
  any of the previous scopes, the starting environment. Given any complex
  value, recurse into its component symbols and check them for binding.
  """
  def is_bound?(v, _) when is_integer(v), do: true
  def is_bound?(v, _) when is_float(v), do: true
  def is_bound?({:literal, _}, _), do: true
  def is_bound?(_, []), do: false

  def is_bound?({container, []}, _) when is_container(container),
    do: true

  def is_bound?({c, contents}, scope) when is_container(c) do
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

  # Boundness checking for :forall and :exists quantifiers.
  def is_bound?({:quantifier, quantifier}, scope) do
    # Introduce any internal bindings for the purpose of boundness
    # checking of the whole expression.
    check_with_bindings(quantifier[:quant_expression], quantifier[:quant_bindings], scope)
  end

  def is_bound?({:comprehension, [{_, [bindings, expr]}]}, scope),
    # Introduce any internal bindings for the purpose of boundness
    # checking of the whole expression.
    do: check_with_bindings(expr, bindings, scope)

  def is_bound?({:intro_op, _}, _), do: true

  def is_bound?({:appl, f: f, x: x}, scopes), do: is_bound?(f, scopes) && is_bound?(x, scopes)

  def is_bound?({:appl, operator: _, x: x, y: y}, scopes),
    do: is_bound?(x, scopes) && is_bound?(y, scopes)

  def is_bound?(variable, [scope | parent] = scopes) do
    f = &(has_key?(scope, &1) or is_bound?(&1, parent))

    cond do
      # Allow arbitrary suffixes or prefixes of "'" to denote
      # successor/remainder variables.
      is_binary(variable) ->
        case variable
             |> String.trim("'")
             |> String.split(".", trim: true) do
          [variable] -> f.(variable)
          variables -> Enum.all?(variables, &is_bound?(&1, scopes))
        end

      true ->
        f.(variable)
    end
  end

  # Process some temporary bindings and check for boundness, without
  # those bindings being valid outside of this context.
  defp check_with_bindings(expr, bindings, scopes) do
    case extract_bindings(bindings) do
      {:ok, bindings} ->
        # Bind the extracted symbols.
        inner_scope = Enum.reduce(bindings, %{}, &bind(&2, &1))
        scopes = [inner_scope | scopes]

        for({_, domain} <- bindings, do: domain)
        # Check the extract domains, as well as the expression itself.
        |> Enum.all?(&is_bound?(&1, scopes)) && is_bound?(expr, scopes)

      {:error, :malformed_bindings} ->
        raise SymbolExtractionError, expr: expr, bindings: bindings, scopes: scopes
    end
  end

  # Extract {symbol, domain} tuples from a list of binding expressions.
  defp extract_bindings(bindings) do
    try do
      bindings =
        for b <- bindings do
          {extract_binding_symbol(b), extract_binding_domain(b)}
        end

      {:ok, bindings}
    rescue
      # Raise if we've encountered an AST that we can't parse as a
      # binding expression.
      FunctionClauseError -> {:error, :malformed_bindings}
    end
  end

  # Given a binding pattern, return the symbol being bound.
  defp extract_binding_symbol({:appl, [operator: op, x: x, y: _]}) when op in [:from, :in], do: x

  # Given a binding pattern, return the domain being bound from.
  defp extract_binding_domain({:appl, [operator: op, x: _, y: dom]}) do
    cond do
      op in [:from, :in] -> dom
      true -> extract_binding_domain(dom)
    end
  end

  defp has_key?(scope, variable),
    do: Map.has_key?(@starting_environment, variable) or Map.has_key?(scope, variable)
end
