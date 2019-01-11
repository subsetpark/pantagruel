defmodule Pantagruel.Bool.Slurp do
  import Kernel, except: [&&: 2, ||: 2]
  import BoolAlg

  def slurp({:program, [module, imports, chapters]}) do
    chapters = Enum.map(chapters, &slurp/1)
    {:program, [module, imports, chapters]}
  end

  def slurp({:chapter, chapter}), do: {:chapter, Enum.map(chapter, &slurp/1)}

  def slurp(section), do: [Enum.reduce(section, true, &slurp/2)]

  def slurp({:expr, [:or, term]}, expression), do: expression || term

  def slurp({:expr, [_, term]}, expression), do: expression && term

  def slurp(term, expression), do: expression && term
end

defmodule Pantagruel.Bool.Lift do
  import BoolAlg
  use Witchcraft
  def lift_term(nil), do: nil
  def lift_term(list) when is_list(list), do: Enum.map(list, &lift_term/1)
  def lift_term(value) when is_integer(value), do: value
  def lift_term(value) when is_atom(value), do: value
  def lift_term({:symbol, _} = s), do: s

  def lift_term({tag, _} = t) when tag in [:un_appl, :bin_appl] do
    alg = transform(t)
    lift(alg, &lift_term/1)
  end

  def lift_term({tag, arguments}) when is_list(arguments) do
    {tag, Enum.map(arguments, &lift_term/1)}
  end

  def lift_term({tag, argument}) do
    {tag, lift_term(argument)}
  end

  defp transform({_, [:or, x, y]}), do: disj(x, y)
  defp transform({_, [:and, x, y]}), do: conj(x, y)
  defp transform({_, [:"<->", x, y]}), do: iff(x, y)
  defp transform({_, [:->, x, y]}), do: impl(x, y)
  defp transform({_, [:"~", x]}), do: nott(x)
  defp transform(t), do: t
end
