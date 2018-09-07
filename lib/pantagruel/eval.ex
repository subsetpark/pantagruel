defmodule Pantagruel.Eval.Variable do
  defstruct name: "", domain: ""
end

defmodule Pantagruel.Eval.Scope do
  alias Pantagruel.Eval.{Variable, Scope}

  def bind(state, {:bunch, elements}, value) do
    Enum.reduce(elements, state, &bind(&2, hd(&1), value))
  end

  def bind(scope, name, value) do
    to_put =
      case value do
        %{} -> value
        domain -> %Variable{name: name, domain: domain}
      end

    Map.put(scope, name, to_put)
  end
end

defmodule Pantagruel.Eval.Lambda do
  alias Pantagruel.Eval.{Lambda, Scope}
  defstruct name: "", domain: [], codomain: nil, type: nil

  def bind(state, name, domain, codomain, type \\ :function) do
    state
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

  # Bind all the variables introduced in a function declaration: function
  # identifier, arguments, and domain.
  defp bind_declaration_variables(state, decl) do
    yield_type = decl[:yield_type] || :function

    # If this is a type constructor, bind the codomain of the function.
    bind_codomain =
      case yield_type do
        :constructor -> &Scope.bind(&1, decl[:yield_domain], decl[:yield_domain])
        _ -> & &1
      end

    state
    |> Lambda.bind(decl[:decl_ident], decl[:decl_doms] || [], decl[:yield_domain], yield_type)
    |> bind_lambda_args(decl)
    |> bind_codomain.()
  end

  defp bind_subexpression_variables({:quantifier, [:exists, bindings, expr]}, state) do
    bound =
      bindings
      |> Enum.reduce(state, fn [ident, _, domain], state2 ->
        Scope.bind(state2, ident, domain)
      end)

    bind_subexpression_variables(expr, bound)
  end

  defp bind_subexpression_variables(_, state), do: state

  defp bind_expression_variables(state, expression) do
    (expression[:left] || [] ++ expression[:right] || [])
    |> Enum.reduce(state, &bind_subexpression_variables/2)
  end

  # Bind all variables introduced in a function declaration and keep track
  # of unbound ones.
  defp eval_declaration({:decl, declaration}, {[scope | scopes], header_unbounds, unbounds}) do
    # Add any newly introduced variables to our set
    # of unbound variables and filter out any that have been
    # bound in the environment.
    # If this is a yielding function, check the codomain for binding.
    scope = bind_declaration_variables(scope, declaration)

    header_unbounds =
      case {declaration[:yield_type], declaration[:yield_domain]} do
        {:function, dom} when dom -> [dom]
        _ -> []
      end
      # If there is any precondition associated with the function, check
      # the symbols there for binding.
      |> Enum.concat(declaration[:expr][:right] || [])
      |> Enum.concat(declaration[:decl_doms] || [])
      |> Binding.include_unbounds(header_unbounds)

    {[scope | scopes], header_unbounds, unbounds}
  end

  # Evaluate a section body expression. Track any unbound variables.
  defp eval_body_expression({:expr, expression}, {
         [scope | scopes],
         [unbounds, next_unbounds | rest]
       }) do
    scope = bind_expression_variables(scope, expression)

    next_unbounds =
      [expression[:left] || [], expression[:right]]
      |> Enum.reduce(
        next_unbounds,
        &Binding.include_unbounds/2
      )

    {[scope | scopes], [unbounds, next_unbounds | rest]}
  end

  defp new_state({scopes, header_unbounds, unbounds}) do
    {[%{} | scopes], header_unbounds, unbounds ++ [MapSet.new()]}
  end

  # Evaluate a single section: evaluate the head, evaluate the body,
  # then check for any unbound variables.
  defp eval_section({:section, section}, state) do
    {scopes, header_unbounds, unbounds} =
      Enum.reduce(section[:head], new_state(state), &eval_declaration/2)

    :ok = Binding.check_unbound(scopes, header_unbounds)

    {scopes, [unbounds | rest]} =
      Enum.reduce(section[:body] || [], {scopes, unbounds}, &eval_body_expression/2)

    :ok = Binding.check_unbound(scopes, unbounds)

    {scopes, MapSet.new(), rest}
  end

  @doc """
  Evaluate a Pantagruel AST for variable binding correctness.
  """
  def eval(program) do
    # Force a check for unbound values. All symbols must be bound by the
    # end of the program; thus the final section cannot introduce any
    # unbound symbols.
    final_check = fn {scopes, _, [unbound]} ->
      :ok = Binding.check_unbound(scopes, unbound)
      scopes
    end

    program
    |> Enum.reduce({[], MapSet.new(), [MapSet.new()]}, &eval_section/2)
    |> final_check.()
  end
end
