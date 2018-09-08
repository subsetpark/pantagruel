defmodule Pantagruel.Eval.Variable do
  alias Pantagruel.Eval.Variable
  defstruct name: "", domain: ""
end

defimpl String.Chars, for: Pantagruel.Eval.Variable do
  def to_string(v) do
    "#{v.name} : #{v.domain}"
  end
end

defmodule Pantagruel.Eval.Scope do
  alias Pantagruel.Eval.{Variable, Scope}

  def starting_environment,
    do: %{
      "Bool" => %Variable{name: "𝔹", domain: "𝔹"},
      "Real" => %Variable{name: "ℝ", domain: "ℝ"},
      "Int" => %Variable{name: "ℤ", domain: "ℤ"},
      "Nat" => %Variable{name: "ℕ", domain: "ℕ"},
      "Nat0" => %Variable{name: "ℕ0", domain: "ℕ0"},
      "String" => %Variable{name: "𝕊", domain: "𝕊"},
      :equals => %Variable{name: "=", domain: "ℝ"},
      :notequals => %Variable{name: "≠", domain: "ℝ"},
      :gt => %Variable{name: ">", domain: "ℝ"},
      :lt => %Variable{name: "<", domain: "ℝ"},
      :gte => %Variable{name: "≥", domain: "ℝ"},
      :lte => %Variable{name: "≤", domain: "ℝ"},
      "+" => %Variable{name: "+", domain: "ℝ"},
      "-" => %Variable{name: "-", domain: "ℝ"},
      "*" => %Variable{name: "×", domain: "ℝ"},
      "^" => %Variable{name: "^", domain: "ℝ"},
      :in => %Variable{name: ":", domain: "⊤"},
      :from => %Variable{name: "∈", domain: "⊤"},
      :iff => %Variable{name: "⇔", domain: "𝔹"},
      :then => %Variable{name: "→", domain: "𝔹"},
      :exists => %Variable{name: "∃", domain: "⊤"},
      :forall => %Variable{name: "∀", domain: "⊤"}
    }

  def bind(scope, {:bunch, elements}, value) do
    Enum.reduce(elements, scope, &bind(&2, hd(&1), value))
  end

  def bind(scope, name, value) do
    to_put =
      case value do
        %{} -> value
        domain -> %Variable{name: name, domain: translate_domain(domain)}
      end

    Map.put(scope, name, to_put)
  end

  def translate_domain(expr) when is_list(expr) do
    Enum.map(expr, &translate_domain/1)
  end

  def translate_domain(domain) when is_binary(domain) or is_atom(domain) do
    case starting_environment() do
      # Look up domain name if predefined.
      %{^domain => variable} -> variable.name
      _ -> domain
    end
  end

  def translate_domain(expr), do: expr
end

defmodule Pantagruel.Eval.Domain do
  alias Pantagruel.Eval.{Scope, Domain}
  defstruct(name: "")

  def bind(scope, domain) do
    Scope.bind(scope, domain, %Domain{name: domain})
  end
end

defimpl String.Chars, for: Pantagruel.Eval.Domain do
  alias Pantagruel.Eval.Domain

  # TODO: This is just cribbed from Z. Can we do better?
  def to_string(%Domain{name: name}) do
    "[#{name}]"
  end
end

defmodule Pantagruel.Eval.Lambda do
  alias Pantagruel.Eval.{Lambda, Scope}
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

    Enum.zip(args, padded_doms)
    |> Enum.reduce(scope, fn {var, dom}, env ->
      env
      |> Scope.bind(var, dom)
    end)
    |> Scope.bind(decl[:decl_ident], from_declaration(decl, doms))
  end

  def from_declaration(decl, doms \\ nil) do
    doms = doms || decl[:lambda_doms] || []

    %Lambda{
      name: decl[:decl_ident],
      domain: doms |> Scope.translate_domain(),
      codomain: decl[:lambda_codomain] |> Scope.translate_domain(),
      type: decl[:yield_type]
    }
  end

  defp pad_list(_, acc, l) when length(acc) == l, do: Enum.reverse(acc)

  defp pad_list([last], acc, l) do
    pad_list([last], [last | acc], l)
  end

  defp pad_list([item | rest], acc, l) do
    pad_list(rest, [item | acc], l)
  end
end
