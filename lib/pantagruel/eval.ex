defmodule Pantagruel.Eval.Variable do
  defstruct name: "", domain: ""
end

defmodule Pantagruel.Eval.Scope do
  alias Pantagruel.Eval.{Variable, Scope}
  defstruct bindings: %{}, parent: nil

  @spec bind(%Scope{}, term(), term()) :: %Scope{}
  def bind(scope, {:bunch, elements}, value) do
    Enum.reduce(elements, scope, &bind(&2, hd(&1), value))
  end

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

  def bind(scope, name, domain, codomain, type \\ :function) do
    scope
    |> Scope.bind(name, %Lambda{
      name: name,
      domain: domain,
      codomain: codomain,
      type: type
    })
  end
end

defmodule Pantagruel.Eval do
  alias Pantagruel.Eval.{Lambda, Binding, Scope}

  defp pad_list(_, acc, l) when length(acc) == l, do: Enum.reverse(acc)

  defp pad_list([last], acc, l) do
    pad_list([last], [last | acc], l)
  end

  defp pad_list([item | rest], acc, l) do
    pad_list(rest, [item | acc], l)
  end

  @doc """
  Create bindings for all arguments introduced as arguments to a function.
  """
  @spec bind_lambda_args(%Scope{}, decl_args: [String.t()], decl_doms: [term()]) :: %Scope{}
  def bind_lambda_args(scope, declaration) do
    args = declaration[:decl_args] || []
    doms = declaration[:decl_doms] || []

    Enum.zip(
      args,
      case {length(doms), length(args)} do
        {l, l} ->
          doms

        {longer, l} when longer > l ->
          raise RuntimeError, "Too many function domains"

        # If there are more arguments than domains, we will use the last
        # domain specified for all the extra arguments.
        {shorter, l} when shorter < l ->
          pad_list(doms, [], l)
      end
    )
    |> Enum.reduce(scope, fn
      {var, dom}, env ->
        env
        |> Scope.bind(var, dom)
        # Automatically introduce successor variable.
        |> Scope.bind(var <> "'", dom)
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

  defp bind_subexpression_variables({:quantifier, [:exists, bindings, expr]}, scope) do
    bound =
      bindings
      |> Enum.reduce(scope, fn [ident, _, domain], scope2 ->
        Scope.bind(scope2, ident, domain)
      end)

    bind_subexpression_variables(expr, bound)
  end

  defp bind_subexpression_variables(_, scope), do: scope

  defp bind_expression_variables(scope, expression) do
    (expression[:left] || [] ++ expression[:right] || [])
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
    |> Binding.include_and_filter_unbounds(
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
      &Binding.include_and_filter_unbounds(&1, &2, :body)
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
    |> (fn s ->
          Enum.reduce(section[:body] || [], s, &eval_body_expression/2)
        end).()
    |> Binding.check_unbound()
  end

  @doc """
  Evaluate a Pantagruel AST for variable binding correctness.
  """
  @spec eval(Pantagruel.t()) :: %Pantagruel.Eval.Scope{}
  def eval(program) do
    program
    |> Enum.reduce({nil, MapSet.new(), MapSet.new()}, &eval_section/2)
    # Force a check for unbound values. Catches values
    # that are unbound at the end of the program (even
    # if the program is only one section long and thus
    # didn't get a chance to check the body yet)
    |> Binding.check_unbound(true)
    |> elem(0)
  end
end
