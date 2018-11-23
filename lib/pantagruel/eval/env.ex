defmodule Pantagruel.Env do
  @moduledoc """
  The evaluation environment for a Pantagruel program.
  """

  import Pantagruel.Guards
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

  defmodule UndefinedAtomError do
    defexception message: "Received atom without string representation", atom: nil
  end

  defmodule DomainMismatchError do
    defexception message: "Domains cannot be matched with identifiers", args: [], doms: []
  end

  @starting_environment %{
    {:symbol, 'Bool'} => %Variable{name: "𝔹", domain: "𝔹"},
    {:symbol, 'Real'} => %Variable{name: "ℝ", domain: "ℝ"},
    {:symbol, 'Int'} => %Variable{name: "ℤ", domain: "ℤ"},
    {:symbol, 'Nat'} => %Variable{name: "ℕ", domain: "ℕ"},
    {:symbol, 'Nat0'} => %Variable{name: "ℕ0", domain: "ℕ0"},
    {:symbol, 'String'} => %Variable{name: "𝕊", domain: "𝕊"},
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
    :":" => %Variable{name: ":", domain: "⊤"},
    :from => %Variable{name: "∈", domain: "⊤"},
    :"<->" => %Variable{name: "↔", domain: "𝔹"},
    :-> => %Variable{name: "→", domain: "𝔹"},
    :and => %Variable{name: "∧", domain: "𝔹"},
    :or => %Variable{name: "∨", domain: "𝔹"},
    :exists => %Variable{name: "∃", domain: "⊤"},
    :all => %Variable{name: "∀", domain: "⊤"},
    :"#" => %Variable{name: "#", domain: "⊤"},
    :& => %Variable{name: "∪", domain: "U"},
    :| => %Variable{name: "∩", domain: "U"}
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

  def bind_lambda(scope, decl) do
    lambda_args = decl[:lambda_args]
    args = lambda_args[:args] || []
    doms = lambda_args[:doms] || []
    # Introduce any generic domains into the scope.
    scope =
      doms
      |> Enum.flat_map(&Domain.flatten_domain/1)
      |> Enum.filter(&Domain.is_generic?/1)
      |> Enum.reduce(scope, fn domain, scope ->
        bind(scope, domain, %Domain{name: domain, ref: domain})
      end)

    # If there are more arguments than domains, we will use the last
    # domain specified for all the extra arguments.
    padded_doms =
      case {length(doms), length(args)} do
        {longer, l} when longer > l ->
          raise DomainMismatchError, args: args, doms: doms

        {_, l} ->
          pad_list(doms, [], l)
      end

    # If this is a type constructor, bind the codomain of the function.
    scope = decl[:yield_type] |> bind_codomain(scope, decl[:lambda_codomain])

    Enum.zip(args, padded_doms)
    |> Enum.reduce(scope, fn {var, dom}, env ->
      env
      |> bind(var, dom)
    end)
    |> bind(decl[:decl_ident], Lambda.from_declaration(decl, doms))
  end

  @doc """
  If a value has been defined in the starting environment, find the name
  it was bound under.
  """
  def lookup_binding_name(symbol) when is_list(symbol) do
    Enum.map(symbol, &lookup_binding_name/1)
  end

  def lookup_binding_name(symbol) when is_atom(symbol) do
    case @starting_environment do
      # Look up symbol name if predefined.
      %{^symbol => variable} -> variable.name
      _ -> to_string(symbol)
    end
  end

  def lookup_binding_name({:symbol, s} = symbol) do
    case @starting_environment do
      # Look up symbol name if predefined.
      %{^symbol => variable} -> variable.name
      _ when is_binary(s) -> s
      _ -> to_string(s)
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
    scope = [bind_lambda(%{}, lambda) | scope]

    [
      lambda[:lambda_args][:doms] || [],
      lambda[:lambda_codomain] || [],
      lambda[:lambda_guards] || []
    ]
    |> List.flatten()
    |> Enum.all?(&is_bound?(&1, scope))
  end

  # Boundness checking for :forall and :exists quantifications.
  def is_bound?(
        {:quantification, [quantifier: _, bindings: bindings, expr: expr]},
        scope
      ) do
    # Introduce any internal bindings for the purpose of boundness
    # checking of the whole expression.
    check_with_bindings(expr, bindings, scope)
  end

  def is_bound?({:comprehension, [bindings: bindings, expr: expr]}, scope),
    # Introduce any internal bindings for the purpose of boundness
    # checking of the whole expression.
    do: check_with_bindings(expr, bindings, scope)

  def is_bound?({:intro_op, _}, _), do: true

  def is_bound?({_, f: f, x: x}, scopes),
    do: is_bound?(f, scopes) && is_bound?(x, scopes)

  def is_bound?({:appl, op: _, x: x, y: y}, scopes),
    do: is_bound?(x, scopes) && is_bound?(y, scopes)

  def is_bound?({:appl, op: _, x: x}, scopes),
    do: is_bound?(x, scopes)

  def is_bound?({:symbol, variable}, [scope | parent]) do
    variable =
      variable
      |> :string.trim(:both, '\'')

    to_check = {:symbol, variable}
    has_key?(scope, to_check) or is_bound?(to_check, parent)
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
    {binding_pairs, variable_references} = extract_bindings(bindings)
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

  # Extract {symbol, domain} tuples from a list of binding expressions.
  defp extract_bindings(bindings) do
    bindings
    |> Enum.reduce({[], []}, &extract_binding_symbols/2)
  end

  # Given a binding pattern, return the symbol being bound.
  defp extract_binding_symbols(
         {:binding, [bind_symbol: x, bind_op: _, bind_domain: domain]},
         {binding_pairs, symbol_references}
       ) do
    {unbunch(x, domain) ++ binding_pairs, symbol_references}
  end

  defp extract_binding_symbols({:guard, exprs}, {pairs, symbol_references}) do
    {pairs, [exprs | symbol_references]}
  end

  defp unbunch({:par, elements}, domain) do
    for e <- elements, do: {e, domain}
  end

  defp unbunch(x, y), do: [{x, y}]

  defp has_key?(scope, variable),
    do: Map.has_key?(@starting_environment, variable) or Map.has_key?(scope, variable)

  defp pad_list(_, acc, l) when length(acc) == l, do: Enum.reverse(acc)
  defp pad_list([last], acc, l), do: pad_list([last], [last | acc], l)
  defp pad_list([item | rest], acc, l), do: pad_list(rest, [item | acc], l)
end
