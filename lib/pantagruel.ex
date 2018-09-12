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

  def read!(filename) do
    File.read!(filename)
    |> Pantagruel.Scan.scan()
    |> Pantagruel.Parse.program()
  end

  def main(filename) do
    try do
      {:ok, parsed, "", %{}, _, _} = read!(filename)
      scope = Pantagruel.Eval.eval(parsed)

      Pantagruel.Print.print_program(parsed, scope)
      |> IO.puts()
    rescue
      e in Pantagruel.Env.UnboundVariablesError ->
        IO.puts("Unbound variables.")
        Enum.each(e.unbound, &IO.inspect/1)
    end
  end
end
