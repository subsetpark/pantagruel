defmodule Pantagruel.Shell do
  import Pantagruel.Macros

  def new_state(tree), do: {tree, [], []}

  def handle({tree, assumed, refuted}, "!" <> prop) do
    ast = get_ast(prop)

    tree = BoolAlg.assert(tree, ast)

    {tree, [ast | assumed], refuted}
  end

  def handle({tree, assumed, refuted}, "~" <> prop) do
    ast = get_ast(prop)

    tree = BoolAlg.refute(tree, ast)

    {tree, assumed, [ast | refuted]}
  end

  def handle(state, _), do: state

  def get_ast(prop) do
    prop
    |> make_minimal_program()
    |> Pantagruel.Scan.scan()
    |> :pant_lexer.string()
    |> Pantagruel.Parse.handle_lex()
    |> extract_minimal_program()
  end

  defp make_minimal_program(exp), do: "f.\n---\n#{exp}"

  defp extract_minimal_program(ast) do
    {:ok, {:program, [_, _, [chapter: [_, [exp(_, exp)]]]]}} = ast
    exp
  end
end
