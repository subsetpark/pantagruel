defmodule Pantagruel.Eval.Scope do
  defstruct bindings: %{}, parent: nil
end

defmodule Pantagruel.Eval.Variable do
  alias Pantagruel.Eval.{Variable, Scope}
  defstruct name: "", domain: ""

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
  alias Pantagruel.Eval.{Lambda, Variable}
  defstruct name: "", domain: [], codomain: nil, type: nil

  def bind(environment, name, domain, codomain, type \\ :function) do
    Variable.bind(environment, name, %Lambda{
      name: name,
      domain: domain,
      codomain: codomain,
      type: type
    })
  end
end

defmodule Pantagruel.Eval.State do
  alias Pantagruel.Eval.Variable

  @starting_environment %{
    "Real" => %Variable{name: "ℝ", domain: "ℝ"},
    "Int" => %Variable{name: "ℤ", domain: "ℤ"},
    "Nat" => %Variable{name: "ℕ", domain: "ℕ"},
    :equals => %Variable{name: "==", domain: "ℝ"},
    :gt => %Variable{name: ">", domain: "ℝ"},
    :lt => %Variable{name: "<", domain: "ℝ"},
    :gte => %Variable{name: ">=", domain: "ℝ"},
    :lte => %Variable{name: "<=", domain: "ℝ"},
    "+" => %Variable{name: "^", domain: "ℝ"},
    "-" => %Variable{name: "^", domain: "ℝ"},
    "^" => %Variable{name: "^", domain: "ℝ"}
  }
  defmodule UnboundVariablesError do
    defexception message: "Unbound variables remain", unbound: MapSet.new()
  end

  defp container_is_bound?([], _, _), do: true

  defp container_is_bound?(contents, environment, should_recurse) do
    Enum.all?(contents, &is_bound?(&1, environment, should_recurse))
  end

  defp is_bound?(variable, _, _)
       when is_integer(variable) or is_float(variable) do
    true
  end

  defp is_bound?([?` | _], _, _), do: true

  defp is_bound?({container, contents}, environment, contents)
       when container == :string or container == :bunch or container == :set or container == :list do
    container_is_bound?(contents, environment, contents)
  end

  defp is_bound?(_, nil, _), do: false

  defp is_bound?(variable, environment, should_recurse) do
    Map.has_key?(@starting_environment, variable) or Map.has_key?(environment.bindings, variable) or
      (should_recurse && is_bound?(variable, environment.parent, false))
  end

  defp is_bound?(v, s), do: is_bound?(v, s, true)

  @doc """
  Include new values into the data structures tracking unbound variables.
  """
  def include_and_filter_unbounds(variables, {scope, global_unbound, head_unbound}, scope_source) do
    union = fn unbound ->
      MapSet.union(unbound, MapSet.new(variables))
    end

    filter = fn unbound ->
      unbound
      |> Enum.filter(&(not is_bound?(&1, scope)))
      |> MapSet.new()
    end

    case scope_source do
      :head ->
        {scope, filter.(global_unbound), filter.(union.(head_unbound))}

      :body ->
        {scope, filter.(union.(global_unbound)), filter.(head_unbound)}
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
  alias Pantagruel.Eval.{Variable, Lambda, State, Scope}

  defp eval_head({:decl, declaration}, {scope, global_unbound, head_unbound}) do
    yield_type = declaration[:yield_type] || :function

    bind = fn
      {var, dom}, env ->
        case env do
          %{^var => ^dom} -> env
          _ -> Variable.bind(env, var, dom)
        end
    end

    # Variable binding

    # First bind this function as a lambda.
    scope =
      Lambda.bind(
        scope,
        declaration[:decl_ident],
        declaration[:decl_doms] || [],
        declaration[:yield_domain],
        yield_type
      )

    # First bind any functions introduced in this declaration.
    scope =
      Enum.zip(
        declaration[:decl_args] || [],
        declaration[:decl_doms] || []
      )
      |> Enum.reduce(scope, bind)

    # If this is a type constructor, bind the codomain of the function.
    scope =
      case yield_type do
        :constructor ->
          Variable.bind(scope, declaration[:yield_domain], declaration[:yield_domain])

        _ ->
          scope
      end

    # Unbound variable checks

    # Add any newly introduced variables to our set
    # of unbound variables and filter out any that have been
    # bound in the environment.
    # If this is a yielding function, check the codomain for binding.
    unbound_domain_check =
      case {yield_type, declaration[:yield_domain]} do
        {:function, dom} when dom -> [dom]
        _ -> []
      end

    # If there is any precondition associated with the function, check
    # the symbols there for binding.
    unbound_condition_check =
      case declaration[:expr][:right] do
        nil -> []
        right -> right
      end

    (unbound_domain_check ++ unbound_condition_check ++ (declaration[:decl_doms] || []))
    |> State.include_and_filter_unbounds({scope, global_unbound, head_unbound}, :head)
  end

  defp eval_body({:expr, expression}, state) do
    include_and_filter = fn variables, state ->
      State.include_and_filter_unbounds(variables, state, :body)
    end

    [expression[:left] || [], expression[:right]]
    |> Enum.reduce(state, include_and_filter)
  end

  defp eval_section({:section, section}, state) do
    state =
      case state do
        {nil, g_u, h_u} -> {%Scope{}, g_u, h_u}
        {scope, g_u, h_u} -> {%Scope{parent: scope}, g_u, h_u}
      end

    section[:head]
    |> Enum.reduce(state, &eval_head/2)
    |> (fn state -> Enum.reduce(section[:body] || [], state, &eval_body/2) end).()
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
