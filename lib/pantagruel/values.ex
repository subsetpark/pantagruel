defmodule Pantagruel.Eval.Variable do
  defstruct name: "", domain: ""
end

defmodule Pantagruel.Eval.Domain do
  alias Pantagruel.Eval.Domain
  alias Pantagruel.Env
  defstruct(name: "")

  def bind(scope, domain), do: Env.bind(scope, domain, %Domain{name: domain})
end

defmodule Pantagruel.Eval.Lambda do
  alias Pantagruel.Eval.{Lambda, Domain}
  alias Pantagruel.Env
  defstruct name: "", domain: [], codomain: nil, type: nil

  def bind(scope, decl) do
    args = decl[:lambda_args] || []
    doms = decl[:lambda_doms] || []

    # If there are more arguments than domains, we will use the last
    # domain specified for all the extra arguments.
    padded_doms =
      case {length(doms), length(args)} do
        {l, l} ->
          doms

        {longer, l} when longer > l ->
          raise RuntimeError, "Too many function domains"

        {shorter, l} when shorter < l ->
          pad_list(doms, [], l)
      end
    # If this is a type constructor, bind the codomain of the function.
    bind_codomain =
      case (decl[:yield_type] || []) do
        :constructor -> &Domain.bind(&1, decl[:lambda_codomain])
        _ -> & &1
      end
    Enum.zip(args, padded_doms)
    |> Enum.reduce(scope, fn {var, dom}, env ->
      env
      |> Env.bind(var, dom)
    end)
    |> Env.bind(decl[:decl_ident], from_declaration(decl, doms))
    |> bind_codomain.()
  end

  def from_declaration(decl, doms \\ nil) do
    %Lambda{
      name: decl[:decl_ident],
      domain:
        (doms || decl[:lambda_doms] || [])
        |> Env.lookup_binding_name(),
      codomain:
        decl[:lambda_codomain]
        |> Env.lookup_binding_name(),
      type: decl[:yield_type],
    }
  end

  defp pad_list(_, acc, l) when length(acc) == l, do: Enum.reverse(acc)
  defp pad_list([last], acc, l), do: pad_list([last], [last | acc], l)
  defp pad_list([item | rest], acc, l), do: pad_list(rest, [item | acc], l)
end
