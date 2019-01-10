defmodule Pantagruel.Env do
  @moduledoc """
  The evaluation environment for a Pantagruel program.
  """

  alias Pantagruel.Values.{Variable, Domain, Lambda}

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

  @starting_environment %{
    {:symbol, 'Bool'} => %Variable{name: "𝔹", domain: "𝔹"},
    {:symbol, 'Real'} => %Variable{name: "ℝ", domain: "ℝ"},
    {:symbol, 'Int'} => %Variable{name: "ℤ", domain: "ℤ"},
    {:symbol, 'Nat'} => %Variable{name: "ℕ", domain: "ℕ"},
    {:symbol, 'Nat0'} => %Variable{name: "ℕ0", domain: "ℕ0"},
    {:symbol, 'String'} => %Variable{name: "𝕊", domain: "𝕊"},
    {:symbol, 'Nil'} => %Variable{name: "∅", domain: "⊤"},
    {:symbol, ":"} => %Variable{name: ":", domain: "⊤"},
    := => %Variable{name: "=", domain: "ℝ"},
    :!= => %Variable{name: "≠", domain: "ℝ"},
    :"~" => %Variable{name: "¬", domain: "𝔹"},
    :> => %Variable{name: ">", domain: "ℝ"},
    :< => %Variable{name: "<", domain: "ℝ"},
    :>= => %Variable{name: "≥", domain: "ℝ"},
    :"=<" => %Variable{name: "≤", domain: "ℝ"},
    :+ => %Variable{name: "+", domain: "ℝ"},
    :- => %Variable{name: "−", domain: "ℝ"},
    :* => %Variable{name: "×", domain: "ℝ"},
    :% => %Variable{name: "÷", domain: "ℝ"},
    :^ => %Variable{name: "^", domain: "ℝ"},
    :in => %Variable{name: "∈", domain: "⊤"},
    :"<->" => %Variable{name: "↔", domain: "𝔹"},
    :-> => %Variable{name: "→", domain: "𝔹"},
    :and => %Variable{name: "∧", domain: "𝔹"},
    :or => %Variable{name: "∨", domain: "𝔹"},
    :exists => %Variable{name: "∃", domain: "⊤"},
    :all => %Variable{name: "∀", domain: "⊤"},
    :"#" => %Variable{name: "#", domain: "⊤"},
    :& => %Variable{name: "∩", domain: "U"},
    :| => %Variable{name: "∪", domain: "U"}
  }

  @doc """
  Introduce a new variable into this scope.
  """
  @spec bind(scope, any(), any()) :: scope
  def bind(scope, {:par, elements}, value) do
    Enum.reduce(elements, scope, &bind(&2, &1, value))
  end

  def bind(scope, name, value) do
    to_put = make_variable(name, value)
    Map.put(scope, name, to_put)
  end

  def bind(scope, {name, value}), do: bind(scope, name, value)

  @spec bind_lambda(scope, Keyword.t()) :: scope
  def bind_lambda(scope, [symbol, bindings, yield_type, codomain]) do
    {binding_pairs, _} =
      bindings
      |> Enum.reduce({[], []}, &extract_binding_symbols/2)

    # Introduce any generic domains into the scope.
    scope =
      binding_pairs
      |> Enum.map(&elem(&1, 1))
      |> Enum.flat_map(&Domain.flatten_domain/1)
      |> Enum.filter(&Domain.is_generic?/1)
      |> Enum.reduce(scope, fn domain, scope ->
        bind(scope, domain, %Domain{name: domain, ref: domain})
      end)

    # If this is a type constructor, bind the codomain of the function.
    scope = bind_codomain(yield_type, scope, codomain)

    binding_pairs
    |> Enum.reduce(scope, fn {var, dom}, env ->
      env
      |> bind(var, dom)
    end)
    |> bind(symbol, Lambda.from_declaration([symbol, bindings, yield_type, codomain]))
  end

  # Existence quantifiers don't just introduce variables for the scope of
  # their predicates; the introduce variables into global scope.
  def bind_expression_variables(
        {:quantification, [:exists, bindings, expr]},
        scope
      ) do
    bound =
      bindings
      |> Enum.reduce(scope, &bind_binding/2)

    bind_expression_variables(expr, bound)
  end

  # In this respect they're unique among expression types.
  def bind_expression_variables(_, state), do: state
  # Recursively evaluate any imported modules and bring the resulting
  # scopes along.
  @doc """
  If a value has been defined in the starting environment, find the name
  it was bound under.
  """
  @spec lookup_binding_name(any) :: String.t()
  def lookup_binding_name(symbol) when is_list(symbol) do
    Enum.map(symbol, &lookup_binding_name/1)
  end

  def lookup_binding_name({:symbol, s} = symbol), do: do_lookup(symbol, s)
  def lookup_binding_name(symbol), do: do_lookup(symbol, symbol)

  @doc """
  Check a list of values for binding in the given scope, and raise if
  anything is unbound.
  """
  @spec check_unbound(t, [any]) :: :ok
  def check_unbound(scopes, candidates) do
    case Enum.filter(candidates, &(!is_bound?(&1, scopes))) do
      [] -> :ok
      unbound -> {:error, {:unbound_variables, MapSet.new(unbound), scopes}}
    end
  end

  @doc """
  Return whether a given value is bound in any of: the current scope,
  any of the previous scopes, the starting environment. Given any complex
  value, recurse into its component symbols and check them for binding.
  """
  @spec is_bound?(any, t) :: boolean
  def is_bound?(v, _) when is_integer(v), do: true
  def is_bound?(v, _) when is_float(v), do: true
  def is_bound?(v, _) when is_atom(v), do: true
  def is_bound?(nil, _), do: true
  def is_bound?({:literal, _}, _), do: true
  def is_bound?(_, []), do: false

  def is_bound?({:cont, [_, []]}, _), do: true
  def is_bound?({:cont, [_, contents]}, scope), do: is_bound?(contents, scope)

  def is_bound?({:refinement, [_, guard, _] = r}, scope) do
    new_scope = bind_expression_variables(guard, %{})
    scope = [new_scope | scope]

    List.flatten(r)
    |> Enum.all?(&is_bound?(&1, scope))
  end

  def is_bound?({:lambda, lambda}, scope) do
    # Lambdas introduce function arguments. Therefore they are bound in
    # (and only in) the recursive boundness check.
    scope = [bind_lambda(%{}, [nil | lambda]) | scope]

    lambda
    |> List.flatten()
    |> Enum.all?(&is_bound?(&1, scope))
  end

  # Boundness checking for :forall and :exists quantifications.
  def is_bound?(
        {:quantification, [_, bindings, expr]},
        scope
      ) do
    # Introduce any internal bindings for the purpose of boundness
    # checking of the whole expression.
    check_with_bindings(expr, bindings, scope)
  end

  def is_bound?({:comprehension, [bindings, expr]}, scope),
    # Introduce any internal bindings for the purpose of boundness
    # checking of the whole expression.
    do: check_with_bindings(expr, bindings, scope)

  def is_bound?({appl, [f, x]}, scopes) when appl in [:dot, :f_appl],
    do: is_bound?(f, scopes) && is_bound?(x, scopes)

  def is_bound?({:bin_appl, [_, x, y]}, scopes),
    do: is_bound?(x, scopes) && is_bound?(y, scopes)

  def is_bound?({:un_appl, [_, x]}, scopes),
    do: is_bound?(x, scopes)

  def is_bound?({:binding, [_, domain]}, scopes),
    do: is_bound?(domain, scopes)

  def is_bound?({:guard, expr}, scopes),
    do: is_bound?(expr, scopes)

  def is_bound?({:symbol, variable}, [scope | parent]) do
    symbol =
      {:symbol,
       variable
       |> :string.trim(:both, '\'')}

    has_key?(scope, symbol) or is_bound?(symbol, parent)
  end

  def is_bound?(es, scopes) when is_list(es), do: Enum.all?(es, &is_bound?(&1, scopes))

  defp do_lookup(symbol, other) do
    case @starting_environment do
      # Look up symbol name if predefined.
      %{^symbol => variable} -> variable.name
      _ when is_binary(other) -> other
      _ -> to_string(other)
    end
  end

  defp make_variable(_, %{} = v), do: v
  defp make_variable(name, domain), do: %Variable{name: name, domain: domain}

  defp bind_codomain('=>', scope, codomain) do
    bind(scope, codomain, %Domain{name: codomain, ref: codomain})
  end

  defp bind_codomain(_, scope, _), do: scope

  # Process some temporary bindings and check for boundness, without
  # those bindings being valid outside of this context.
  defp check_with_bindings(expr, bindings, scopes) do
    {binding_pairs, variable_references} =
      bindings
      |> Enum.reduce({[], []}, &extract_binding_symbols/2)

    # Bind the extracted symbols.
    inner_scope =
      binding_pairs
      |> Enum.reduce(%{}, &bind(&2, &1))

    scopes = [inner_scope | scopes]

    # Check the extract domains, as well as the expression itself.
    for({_, d} <- binding_pairs, do: d)
    |> Enum.concat(variable_references)
    |> Enum.all?(&is_bound?(&1, scopes)) && is_bound?(expr, scopes)
  end

  # Given a binding pattern, return the symbol being bound.
  def extract_binding_symbols(
        {:binding, [x, domain]},
        {binding_pairs, symbol_references}
      ) do
    {unbunch(x, domain) ++ binding_pairs, symbol_references}
  end

  def extract_binding_symbols({:guard, exprs}, {pairs, symbol_references}) do
    {pairs, [exprs | symbol_references]}
  end

  defp bind_binding({:binding, [x, d]}, s), do: bind(s, x, d)

  defp unbunch({:cont, [:par, elements]}, domain), do: for(e <- elements, do: {e, domain})

  defp unbunch(x, y), do: [{x, y}]

  defp has_key?(scope, variable),
    do: Map.has_key?(@starting_environment, variable) or Map.has_key?(scope, variable)
end
