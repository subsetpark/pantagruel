defmodule Pantagruel do
  @type t :: [section]
  @type section :: [declaration | expression]
  @type declaration :: [
          decl_ident: value,
          lambda_args: [value],
          lambda_doms: [value],
          yield_type: :yields | :produces,
          yield_domain: value
        ]
  @type expression :: [left: subexpression, op: value, right: subexpression]
  @type value :: atom | String.t() | number | [value]
  @type subexpression :: [value]

  def eval(contents) do
    with {:ok, parsed, "", %{}, _, _} =
           contents
           |> Pantagruel.Scan.scan()
           |> Pantagruel.Parse.program() do
      Pantagruel.Eval.eval(parsed)
    end
  end

  def read!(filename) do
    File.read!(filename)
    |> eval
  end

  def main(filename) do
    try do
      scope = read!(filename)
      IO.inspect scope
      scope
      |> Pantagruel.Print.to_string()
      |> IO.puts
    rescue
      e in Pantagruel.Eval.Binding.UnboundVariablesError ->
        IO.puts("Unbound variables.")
        Enum.each(e.unbound, &IO.inspect/1)
    end
  end
end
