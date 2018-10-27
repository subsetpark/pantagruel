defmodule Pantagruel.Load do
  alias Pantagruel.{Scan, Parse}

  defmodule ModuleShadowError do
    defexception message: "Attempted to redefine an existing module", mod_name: nil
  end

  def load(paths) do
    try do
      {:ok,
       paths
       |> Enum.reduce(%{}, &load_asts/2)}
    rescue
      e in ModuleShadowError ->
        {:error, {:module_shadow, e}}
    end
  end

  defp load_asts(path, asts) do
    Path.join([path, "**", "*.pant"])
    |> Path.wildcard()
    |> Enum.reduce(asts, &load_ast/2)
  end

  defp load_ast(path, asts) do
    path
    |> File.read!()
    |> Scan.scan()
    |> Parse.program()
    |> include(asts)
  end

  defp include({:ok, [{:module, mod_name: mod_name} | rest], "", %{}, _, _}, asts) do
    case asts do
      %{^mod_name => _} ->
        raise ModuleShadowError, mod_name: mod_name

      %{} ->
        Map.put(asts, mod_name, rest)
    end
  end
end
