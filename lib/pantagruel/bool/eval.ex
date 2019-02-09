defmodule Pantagruel.Bool.Eval do
  def test({:cont, [:par, [exp]]}, e2), do: test(exp, e2)
  def test(e2, {:cont, [:par, [exp]]}), do: test(e2, exp)
  def test({:bin_appl, [op, x, y]}, {:bin_appl, [op2, x2, y2]}) do
    env = make_so(op, x, y)
    pred = make_pred(op2, x2, y2)
    pred.(env)
  end

  def test(_, _) do
    false
  end

  def make_so(:=, x, y), do: %{x => y}
  def make_pred(:=, x, y), do: fn env -> Map.get(env, x) == y end
end
