defmodule Pantagruel.Eval.Variable do
  defstruct name: "", domain: ""
end

defmodule Pantagruel.Eval.Scope do
  alias Pantagruel.Eval.{Variable, Scope}

  def bind(scope, {:bunch, elements}, value) do
    Enum.reduce(elements, scope, &bind(&2, hd(&1), value))
  end

  def bind(scope, name, value) do
    to_put =
      case value do
        %{} -> value
        domain -> %Variable{name: name, domain: domain}
      end

    Map.put(scope, name, to_put)
  end

  def puts(scope) do
    IO.puts "OK"
  end
end

defmodule Pantagruel.Eval.Lambda do
  alias Pantagruel.Eval.{Lambda, Scope}
  defstruct name: "", domain: [], codomain: nil, type: nil

  def bind(scope, decl, type \\ :function) do
    args = decl[:lambda_args] || []
    doms = decl[:lambda_doms] || []
    # If there are more arguments than domains, we will use the last
    # domain specified for all the extra arguments.
    doms =
      case {length(doms), length(args)} do
        {l, l} ->
          doms

        {longer, l} when longer > l ->
          raise RuntimeError, "Too many function domains"

        {shorter, l} when shorter < l ->
          pad_list(doms, [], l)
      end

    Enum.zip(args, doms)
    |> Enum.reduce(scope, fn {var, dom}, env ->
      env
      |> Scope.bind(var, dom)
      # Automatically introduce successor variable.
      |> Scope.bind(var <> "'", dom)
    end)
    |> Scope.bind(decl[:decl_ident], %Lambda{
      name: decl[:decl_ident],
      domain: decl[:lambda_doms] || [],
      codomain: decl[:lambda_codomain],
      type: type
    })
  end

  defp pad_list(_, acc, l) when length(acc) == l, do: Enum.reverse(acc)

  defp pad_list([last], acc, l) do
    pad_list([last], [last | acc], l)
  end

  defp pad_list([item | rest], acc, l) do
    pad_list(rest, [item | acc], l)
  end
end
