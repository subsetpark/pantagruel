defmodule Pantagruel.Env do
  @moduledoc """
  The evaluation environment for a Pantagruel program.
  """

  alias Pantagruel.Values.{Variable, Domain, Lambda}
  alias Pantagruel.Eval.Module

  import Pantagruel.Macros

  defmodule Scope do
    defstruct __module__: nil
    @type t :: %__MODULE__{}
  end

  @typedoc """
  An environment is a list of binding contexts, one scope for each section
  of the program. Symbols are bound into scope by being referred to in
  one of several declaration forms, at which point they are inserted
  into the scope for that section.
  """
  @type t :: [Scope.t()]

  defmodule UnboundVariablesError do
    defexception message: "Unbound variables remain", unbound: MapSet.new(), env: []
  end

  @starting_environment %{
    sym('Bool') => %Variable{name: "𝔹", domain: "𝔹"},
    sym('Nat') => %Variable{name: "ℕ", domain: "ℕ"},
    sym('Nat0') => %Variable{name: "ℕ0", domain: "ℕ0"},
    sym('Int') => %Variable{name: "ℤ", domain: "ℤ"},
    sym('Rat') => %Variable{name: "ℚ", domain: "ℚ"},
    sym('Real') => %Variable{name: "ℝ", domain: "ℝ"},
    sym('String') => %Variable{name: "𝕊", domain: "𝕊"},
    sym('Nil') => %Variable{name: "∅", domain: "⊤"},
    sym('Any') => %Variable{name: "⊤", domain: "⊤"},
    sym('Set') => %Variable{name: "𝒫", domain: "⊤"},
    sym('Seq') => %Variable{name: "𝒮", domain: "⊤"},
    sym(":") => %Variable{name: ":", domain: "⊤"},
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
    :and => %Variable{name: "∧", domain: "𝔹"},
    :or => %Variable{name: "∨", domain: "𝔹"},
    :xor => %Variable{name: "⊕", domain: "𝔹"},
    :-> => %Variable{name: "→", domain: "𝔹"},
    :"<->" => %Variable{name: "↔", domain: "𝔹"},
    :exists => %Variable{name: "∃", domain: "⊤"},
    :all => %Variable{name: "∀", domain: "⊤"},
    :"#" => %Variable{name: "#", domain: "⊤"},
    :& => %Variable{name: "∩", domain: "U"},
    :| => %Variable{name: "∪", domain: "U"}
  }

  @doc """
  Start a new execution environment.
  """
  @spec new() :: t()
  def new, do: []

  @doc """
  Extend an exection environment with a new scope.
  """
  @spec extend(t(), atom()) :: t()
  def extend(env, nil), do: [%Scope{} | env]
  def extend(env, mod_name), do: [%Scope{__module__: %Module{name: mod_name}} | env]

  @doc """
  Introduce a new variable into this scope.
  """
  def bind(scope, {name, value}), do: bind(scope, name, value)

  @spec bind(Scope.t(), any(), any()) :: Scope.t()
  def bind(scope, {:par, elements}, value), do: Enum.reduce(elements, scope, &bind(&2, &1, value))
  def bind(scope, name, value), do: Map.put(scope, name, make_variable(name, value))

  @spec bind_lambda(Scope.t(), Keyword.t()) :: Scope.t()
  def bind_lambda(scope \\ %{}, [symbol, bindings, yield_type, codomain]) do
    lambda_value = Lambda.from_declaration([symbol, bindings, yield_type, codomain])
    {binding_pairs, _} = extract_binding_symbols(bindings)

    # Introduce any generic domains into the scope.
    scope =
      binding_pairs
      |> Stream.map(&elem(&1, 1))
      |> Stream.flat_map(&Domain.flatten_domain/1)
      |> Stream.filter(&Domain.is_generic?/1)
      |> Enum.reduce(scope, &bind(&2, &1, %Domain{name: &1, ref: &1}))
      # If this is a type constructor, bind the codomain of the function.
      |> bind_codomain(yield_type, codomain)

    binding_pairs
    |> Enum.reduce(scope, fn {var, dom}, env -> bind(env, var, dom) end)
    |> bind(symbol, lambda_value)
  end

  # Existence quantifiers don't just introduce variables for the scope of
  # their predicates; the introduce variables into global scope.
  def bind_expression_variables(scope, {:quantification, [:exists, bindings, expr]}) do
    bindings
    |> Enum.reduce(scope, &bind_binding/2)
    |> bind_expression_variables(expr)
  end

  # In this respect they're unique among expression types.
  def bind_expression_variables(state, _), do: state
  # Recursively evaluate any imported modules and bring the resulting
  # env along.
  @doc """
  If a value has been defined in the starting environment, find the name
  it was bound under.
  """
  @spec lookup_binding_name(any) :: String.t()
  def lookup_binding_name(sym(s) = symbol), do: do_lookup(symbol, s)
  def lookup_binding_name(symbol), do: do_lookup(symbol, symbol)

  @doc """
  Check a list of values for binding in the given scope, and raise if
  anything is unbound.
  """
  @spec check_unbound(t, [any]) :: :ok | {:error, {:unbound_variables, MapSet.t(), t}}
  def check_unbound(env, candidates) do
    case Enum.reject(candidates, &is_bound?(&1, env)) do
      [] -> :ok
      unbound -> {:error, {:unbound_variables, MapSet.new(unbound), env}}
    end
  end

  @doc """
  Return whether a given value is bound in any of: the current scope,
  any of the previous env, the starting environment. Given any complex
  value, recurse into its component symbols and check them for binding.
  """
  @spec is_bound?(any, t) :: boolean
  def is_bound?(v, _) when is_integer(v), do: true
  def is_bound?(v, _) when is_float(v), do: true
  def is_bound?(v, _) when is_atom(v), do: true
  def is_bound?(nil, _), do: true
  def is_bound?({:literal, _}, _), do: true
  def is_bound?(_, []), do: false
  def is_bound?({:cont, [_, contents]}, env), do: is_bound?(contents, env)

  def is_bound?({:case_exp, [guard, _] = exp}, env) do
    new_scope = bind_expression_variables(%{}, guard)
    env = [new_scope | env]

    is_bound?(exp, env)
  end

  def is_bound?({:lambda, lambda}, env) do
    # Lambdas introduce function arguments. Therefore they are bound in
    # (and only in) the recursive boundness check.
    env = [bind_lambda([nil | lambda]) | env]

    is_bound?(lambda, env)
  end

  def is_bound?({:quantification, [_, bindings, expr]}, env),
    do: check_with_bindings(expr, bindings, env)

  def is_bound?({:comprehension, [bindings, expr]}, env),
    do: check_with_bindings(expr, bindings, env)

  def is_bound?({appl, [f, x]}, env) when appl in [:dot, :f_appl],
    do: is_bound?(f, env) and is_bound?(x, env)

  def is_bound?({:bin_appl, [_, x, y]}, env),
    do: is_bound?(x, env) and is_bound?(y, env)

  def is_bound?({:un_appl, [_, x]}, env),
    do: is_bound?(x, env)

  def is_bound?({:binding, [_, domain]}, env),
    do: is_bound?(domain, env)

  def is_bound?({:guard, expr}, env),
    do: is_bound?(expr, env)

  def is_bound?(sym(variable), [scope | parent]) do
    # Trim off trailing apostrophes, so "successor" or "primed" variables
    # (like x' for x) are treated the same as their base names
    trimmed = :string.trim(variable, :trailing, '\'')
    symbol = sym(trimmed)

    has_key?(scope, symbol) or is_bound?(symbol, parent)
  end

  def is_bound?(es, env) when is_list(es) or is_function(es, 2),
    do: Enum.all?(es, &is_bound?(&1, env))

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

  defp bind_codomain(scope, '=>', codomain),
    do: bind(scope, codomain, %Domain{name: codomain, ref: codomain})

  defp bind_codomain(scope, _, _), do: scope

  # Process some temporary bindings and check for boundness, without
  # those bindings being valid outside of this context.
  defp check_with_bindings(expr, bindings, env) do
    {binding_pairs, variable_references} = extract_binding_symbols(bindings)

    # Bind the extracted symbols.
    inner_scope = Enum.reduce(binding_pairs, %{}, &bind(&2, &1))
    env = [inner_scope | env]

    # Check the extract domains, as well as the expression itself.
    is_bound?(expr, env) and
      binding_pairs
      |> Stream.map(&elem(&1, 1))
      |> Stream.concat(variable_references)
      |> is_bound?(env)
  end

  def args_and_domains(bindings) do
    {binding_pairs, _} =
      bindings
      |> Enum.reverse()
      |> extract_binding_symbols()

    Enum.unzip(binding_pairs)
  end

  # Given a binding pattern, return the symbol being bound.
  def extract_binding_symbols(bindings),
    do: Enum.reduce(bindings, {[], []}, &extract_binding_symbols/2)

  defp extract_binding_symbols(
         {:binding, [x, domain]},
         {binding_pairs, symbol_references}
       ),
       do: {unbunch(x, domain) ++ binding_pairs, symbol_references}

  defp extract_binding_symbols({:guard, exprs}, {pairs, symbol_references}),
    do: {pairs, [exprs | symbol_references]}

  defp bind_binding({:binding, [x, d]}, s), do: bind(s, x, d)
  defp bind_binding(_, s), do: s

  defp unbunch({:cont, [:par, elements]}, domain), do: Enum.map(elements, &{&1, domain})
  defp unbunch(x, y), do: [{x, y}]

  defp has_key?(scope, variable),
    do: Map.has_key?(@starting_environment, variable) or Map.has_key?(scope, variable)
end
