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

defmodule Pantagruel.UnboundVariablesError do
  defexception message: "Unbound variables remain", unbound: MapSet.new()
end

defmodule Pantagruel.Eval do
  alias Pantagruel.UnboundVariablesError
  alias Pantagruel.Eval.{Variable, Constant, Lambda}

  @starting_environment %{
    "Real" => %Constant{name: "ℝ"},
    "Nat" => %Constant{name: "ℕ"},
    :gt => %Constant{name: ">"}
  }

  defp is_bound?(variable, _)
       when is_integer(variable) or is_float(variable),
       do: true

  defp is_bound?(variable, environment),
    do: Map.has_key?(@starting_environment, variable) or Map.has_key?(environment, variable)

  defp eval_head([], environment, unbound), do: {environment, unbound}

  defp eval_head([{:decl, declaration} | program], environment, unbound) do
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

    unbound =
      unbound
      # Check the argument domains for binding.
      |> MapSet.union(MapSet.new(declaration[:decl_doms] || []))
      # Filter out bound variables.
      |> Enum.filter(&(not is_bound?(&1, environment)))
      |> MapSet.new()

    eval_head(program, environment, unbound)
  end

  defp eval_head([_ | program], environment, unbound) do
    eval_head(program, environment, unbound)
  end

  defp eval_body(body, environment, _unbound), do: environment

  defp eval_section({:section, section}, environment) do
    {environment, unbound} = eval_head(section[:head], environment, MapSet.new())
    
    # Check to see if all header variables have been bound.
    case MapSet.size(unbound) do
      0 -> :ok
      _ -> raise UnboundVariablesError, unbound: unbound
    end

    eval_body(section[:body], environment, MapSet.new())
  end

  def eval(program) do
    program
    |> Enum.reduce(%{}, &eval_section/2)
  end
end
