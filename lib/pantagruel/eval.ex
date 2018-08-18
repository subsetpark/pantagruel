defmodule Variable do
  defstruct name: "", domain: ""

  def bind(environment, name, domain) do
    Map.put(environment, name, %Variable{name: name, domain: domain})
  end
end

defmodule Function do
  defstruct name: "", codomain: nil
end

defmodule Constant do
  defstruct name: ""
end

defmodule UnboundVariablesError do
  defexception message: "Unbound variables remain", unbound: MapSet.new()
end

defmodule Pantagruel.Eval do
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

  defp eval([], environment, _), do: environment

  defp eval([{:decl, declaration} | program], environment, unbound) do
    yield_type = declaration[:yield_type]

    bind = fn
      {var, dom}, env ->
        case env do
          %{^var => ^dom} -> env
          _ -> Variable.bind(env, var, dom)
        end
    end

    # Variable binding

    # First bind any functions introduced in this declaration.
    environment =
      Enum.zip(
        declaration[:decl_args],
        declaration[:decl_doms]
      )
      |> Enum.reduce(environment, bind)

    # If this is a type constructor, bind the codomain of the function.
    environment =
      case yield_type do
        :produces ->
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
      case yield_type do
        :yields -> MapSet.put(unbound, declaration[:yield_domain])
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
      |> MapSet.union(MapSet.new(declaration[:decl_doms]))
      # Filter out bound variables.
      |> Enum.filter(&(not is_bound?(&1, environment)))
      |> MapSet.new()

    # Finally, check to see if all variables have been bound.
    # TODO: Ultimately this can't be done within declaration handling. It
    # can only be done after all declarations in a section have been
    # evaluated, because symbols can be bound in the declaration after
    # their introduction, as long as it's in the same section.
    case MapSet.size(unbound) do
      0 -> :ok
      _ -> raise UnboundVariablesError, unbound: unbound
    end

    eval(program, environment, unbound)
  end

  def eval(program) do
    eval(program, %{}, MapSet.new())
  end
end
