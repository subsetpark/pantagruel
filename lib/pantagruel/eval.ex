defmodule Pantagruel.Eval.Variable do
  alias Pantagruel.Eval.Variable

  defstruct name: "", domain: ""

  def bind(environment, name, domain) do
    Map.put(environment, name, %Variable{name: name, domain: domain})
  end
end

defmodule Pantagruel.Eval.Lambda do
  alias Pantagruel.Eval.Lambda
  defstruct name: "", domain: [], codomain: nil, type: nil

  def bind(environment, name, domain, codomain, type \\ :function) do
    Map.put(environment, name, %Lambda{name: name, domain: domain, codomain: codomain, type: type})
  end
end

defmodule Pantagruel.Eval.Constant do
  defstruct name: ""
end

defmodule Pantagruel.Eval.State do
  alias Pantagruel.Eval.Constant
  @starting_environment %{
    "Real" => %Constant{name: "ℝ"},
    "Nat" => %Constant{name: "ℕ"},
    :gt => %Constant{name: ">"}
  }
  def starting_environment, do: @starting_environment

  defmodule UnboundVariablesError do
    defexception message: "Unbound variables remain", unbound: MapSet.new()
  end

  defp is_bound?(variable, _)
       when is_integer(variable) or is_float(variable) do
    true
  end

  defp is_bound?(variable, environment) do
    Map.has_key?(@starting_environment, variable) or
      Map.has_key?(environment, variable)
  end

  def register_unbound(variables, {environment, unbound}) do
    {environment,
     unbound
     # Check the argument domains for binding.
     |> MapSet.union(MapSet.new(variables))
     # Filter out bound variables.
     |> Enum.filter(&(not is_bound?(&1, environment)))
     |> MapSet.new()}
  end
  def check_unbound(unbound) do
    # Check to see if all header variables have been bound.
    case MapSet.size(unbound) do
      0 -> :ok
      _ -> raise UnboundVariablesError, unbound: unbound
    end
  end

end

defmodule Pantagruel.Eval do
  alias Pantagruel.Eval.{Variable, Lambda, State}

  defp eval_head({:decl, declaration}, {environment, unbound}) do
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
    environment =
      Lambda.bind(
        environment,
        declaration[:decl_ident],
        declaration[:decl_doms] || [],
        declaration[:yield_domain],
        yield_type
      )

    # First bind any functions introduced in this declaration.
    environment =
      Enum.zip(
        declaration[:decl_args] || [],
        declaration[:decl_doms] || []
      )
      |> Enum.reduce(environment, bind)

    # If this is a type constructor, bind the codomain of the function.
    environment =
      case yield_type do
        :constructor ->
          Variable.bind(environment, declaration[:yield_domain], declaration[:yield_domain])

        _ ->
          environment
      end

    # Unbound variable checks

    # Add any newly introduced variables to our set
    # of unbound variables and filter out any that have been
    # bound in the environment.
    # If this is a yielding function, check the codomain for binding.
    unbound =
      case {yield_type, declaration[:yield_domain]} do
        {:function, dom} when dom -> MapSet.put(unbound, dom)
        _ -> unbound
      end

    # If there is any precondition associated with the function, check
    # the symbols there for binding.
    unbound =
      case declaration[:expr][:right] do
        nil -> unbound
        right -> MapSet.union(unbound, MapSet.new(right))
      end

    State.register_unbound(declaration[:decl_doms] || [], {environment, unbound})
  end

  defp eval_body({:expr, expression}, state) do
    [expression[:left] || [], expression[:right]]
    |> Enum.reduce(state, &State.register_unbound/2)
  end

  defp eval_section({:section, section}, state) do
    {_, unbound} =
      state =
      section[:head]
      |> Enum.reduce(state, &eval_head/2)

    :ok = State.check_unbound(unbound)

    {_, unbound} =
      state =
      (section[:body] || [])
      |> Enum.reduce(state, &eval_body/2)

    :ok = State.check_unbound(unbound)

    state
  end

  def eval(program) do
    program
    |> Enum.reduce({%{}, MapSet.new()}, &eval_section/2)
  end
end
