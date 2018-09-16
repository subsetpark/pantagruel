defmodule Pantagruel.Eval.Variable do
  @moduledoc """
  A bound value in an evaluated Pantagruel program, with a name and the
  domain it is in.
  """
  defstruct name: "", domain: ""
end

defmodule Pantagruel.Eval.Domain do
  @moduledoc """
  A domain in an evaluated Pantagruel program, with a name and whatever
  domain it is an alias for (or itself, otherwise).
  """
  import Pantagruel.Guards
  alias Pantagruel.Eval.Domain
  alias Pantagruel.Env
  defstruct(name: "", ref: "")

  @doc """
  Introduce a new domain into scope.
  """
  def bind(scope, domain, ref), do: Env.bind(scope, domain, %Domain{name: domain, ref: ref})

  @doc """
  Generic domains can be introduced by prepending their name with an
  underscore. This distinction allows generics to be referred to as a
  part of a function definition without being defined first.
  """
  def is_generic?(domain), do: String.starts_with?(domain, "_")

  @doc """
  Flatten nested or composite domains to retrieve the basic domains they
  are composed of.
  """
  def flatten_domain({e, items}) when is_container(e), do: items
  def flatten_domain({:lambda, decl}), do: flatten_domain(decl[:lambda_doms])
  def flatten_domain(items) when is_list(items), do: items
  def flatten_domain(item), do: [item]
end

defmodule Pantagruel.Eval.Lambda do
  @moduledoc """
  A function in an evaluated Pantagruel program, either as introduced
  in a header declaration or referred to as an anonymous function inside
  of a section body.

  The `type` of a lambda is either `:function`, which is a normal
  function from 0 or more arguments with an optional return type, or
  `:constructor`, which defines the construction function for complex
  types such as List[Int] or of objects with named fields.

  The evaluation behavior of constructors is such that they also bind
  their codomain into scope; functions must have their codomains defined
  elsewhere.
  """
  alias Pantagruel.Eval.{Lambda, Domain}
  alias Pantagruel.Env
  defstruct name: "", domain: [], codomain: nil, type: nil

  @doc """
  Bind a lambda form into scope.
  """
  def bind(scope, decl) do
    args = decl[:lambda_args] || []
    doms = decl[:lambda_doms] || []
    # Introduce any generic domains into the scope.
    scope =
      doms
      |> Enum.flat_map(&Domain.flatten_domain/1)
      |> Enum.filter(&Domain.is_generic?/1)
      |> Enum.reduce(scope, &Domain.bind(&2, &1, &1))

    # If there are more arguments than domains, we will use the last
    # domain specified for all the extra arguments.
    padded_doms =
      case {length(doms), length(args)} do
        {longer, l} when longer > l ->
          raise RuntimeError, "Too many function domains"

        {_, l} ->
          pad_list(doms, [], l)
      end

    # If this is a type constructor, bind the codomain of the function.
    bind_codomain =
      case decl[:yield_type] || [] do
        :constructor -> &Domain.bind(&1, decl[:lambda_codomain], decl[:lambda_codomain])
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

  @doc """
  Given a function declaration, build a Lambda struct.
  """
  def from_declaration(decl, doms \\ nil) do
    %Lambda{
      name: decl[:decl_ident],
      domain:
        (doms || decl[:lambda_doms] || [])
        |> Env.lookup_binding_name(),
      codomain:
        decl[:lambda_codomain]
        |> Env.lookup_binding_name(),
      type: decl[:yield_type]
    }
  end

  defp pad_list(_, acc, l) when length(acc) == l, do: Enum.reverse(acc)
  defp pad_list([last], acc, l), do: pad_list([last], [last | acc], l)
  defp pad_list([item | rest], acc, l), do: pad_list(rest, [item | acc], l)
end
