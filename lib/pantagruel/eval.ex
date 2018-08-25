defmodule Pantagruel.Eval.Variable do
  defstruct name: "", domain: ""
end

defmodule Pantagruel.Eval.Scope do
  alias Pantagruel.Eval.{Variable, Scope}
  defstruct bindings: %{}, parent: nil

  def bind(scope, name, value) do
    to_put =
      case value do
        %{} -> value
        domain -> %Variable{name: name, domain: domain}
      end

    %Scope{
      scope
      | bindings: Map.put(scope.bindings, name, to_put)
    }
  end
end

defmodule Pantagruel.Eval.Lambda do
  alias Pantagruel.Eval.{Lambda, Scope}
  defstruct name: "", domain: [], codomain: nil, type: nil

  def bind(environment, name, domain, codomain, type \\ :function) do
    environment
    |> Scope.bind(name, %Lambda{
      name: name,
      domain: domain,
      codomain: codomain,
      type: type
    })
  end
end

defmodule Pantagruel.Eval.State do
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
    :in => %Variable{name: ":", domain: "⊤"}
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

  @doc """
  Boundness checking for container types.
  """
  defp is_bound?({container, contents}, environment, contents)
       when container in [:string, :bunch, :set, :list] do
    container_is_bound? = fn
      [], _, _ ->
        true

      contents, environment, should_recurse ->
        Enum.all?(contents, &is_bound?(&1, environment, should_recurse))
    end

    container_is_bound?.(contents, environment, contents)
  end

  @doc """
  Boundness checking for functions.
  """
  defp is_bound?({:lambda, lambda}, environment, should_recurse) do
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
        Pantagruel.Eval.bind_lambda_args(environment, lambda),
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

  defp is_bound?(variable, environment, should_recurse) do
    Map.has_key?(@starting_environment, variable) or Map.has_key?(environment.bindings, variable) or
      (should_recurse and is_bound?(variable, environment.parent, false))
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

defmodule Pantagruel.Eval do
  alias Pantagruel.Eval.{Lambda, State, Scope}

  @doc """
  Create bindings for all arguments introduced as arguments to a function.
  """
  def bind_lambda_args(scope, declaration) do
    Enum.zip(
      declaration[:decl_args] || [],
      declaration[:decl_doms] || []
    )
    |> Enum.reduce(scope, fn
      {var, dom}, env ->
        case env do
          %{^var => ^dom} -> scope
          _ -> Scope.bind(scope, var, dom)
        end
    end)
  end

  @doc """
  Bind all the variables introduced in a function declaration: function
  identifier, arguments, and domain.
  """
  defp bind_declaration_variables(scope, declaration) do
    yield_type = declaration[:yield_type] || :function

    scope
    |> Lambda.bind(
      declaration[:decl_ident],
      declaration[:decl_doms] || [],
      declaration[:yield_domain],
      yield_type
    )
    |> bind_lambda_args(declaration)
    # If this is a type constructor, bind the codomain of the function.
    |> (fn scope ->
          case yield_type do
            :constructor ->
              Scope.bind(
                scope,
                declaration[:yield_domain],
                declaration[:yield_domain]
              )

            _ ->
              scope
          end
        end).()
  end

  defp bind_subexpression_variables({:exists, [ident, _, domain, expr]}, scope) do
    bound = Scope.bind(scope, ident, domain)
    bind_subexpression_variables(expr, bound)
  end

  defp bind_subexpression_variables(_, scope), do: scope

  defp bind_expression_variables(scope, expression) do
    ((expression[:left] || []) ++ (expression[:right] || []))
    |> Enum.reduce(scope, &bind_subexpression_variables/2)
  end

  @doc """
  Bind all variables introduced in a function declaration and keep track
  of unbound ones.
  """
  defp eval_declaration({:decl, declaration}, {scope, global_unbound, head_unbound}) do
    # Add any newly introduced variables to our set
    # of unbound variables and filter out any that have been
    # bound in the environment.
    # If this is a yielding function, check the codomain for binding.
    case {declaration[:yield_type], declaration[:yield_domain]} do
      {:function, dom} when dom -> [dom]
      _ -> []
    end
    # If there is any precondition associated with the function, check
    # the symbols there for binding.
    |> Enum.concat(declaration[:expr][:right] || [])
    |> Enum.concat(declaration[:decl_doms] || [])
    |> State.include_and_filter_unbounds(
      {
        bind_declaration_variables(scope, declaration),
        global_unbound,
        head_unbound
      },
      :head
    )
  end

  @doc """
  Evaluate a section body expression. Track any unbound variables.
  """
  defp eval_body_expression({:expr, expression}, {scope, global_unbound, head_unbound}) do
    [expression[:left] || [], expression[:right]]
    |> Enum.reduce(
      {bind_expression_variables(scope, expression), global_unbound, head_unbound},
      &State.include_and_filter_unbounds(&1, &2, :body)
    )
  end

  defp new_state({nil, g_unbound, h_unbound}), do: {%Scope{}, g_unbound, h_unbound}
  defp new_state({scope, g_u, h_u}), do: {%Scope{parent: scope}, g_u, h_u}

  @doc """
  Evaluate a single section: evaluate the head, evaluate the body,
  then check for any unbound variables.
  """
  defp eval_section({:section, section}, state) do
    section[:head]
    |> Enum.reduce(new_state(state), &eval_declaration/2)
    |> (fn state ->
          Enum.reduce(section[:body] || [], state, &eval_body_expression/2)
        end).()
    |> State.check_unbound()
  end

  def eval(program) do
    program
    |> Enum.reduce({nil, MapSet.new(), MapSet.new()}, &eval_section/2)
    # Force a check for unbound values. Catches values
    # that are unbound at the end of the program (even
    # if the program is only one section long and thus
    # didn't get a chance to check the body yet)
    |> State.check_unbound(true)
  end
end
