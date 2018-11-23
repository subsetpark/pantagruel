defmodule Pantagruel.Load do
  @moduledoc """
  Provides the functionality for loading Pantagruel files, making them
  available for import.
  """
  alias Pantagruel.{Scan, Parse}

  defmodule ModuleShadowError do
    defexception message: "Attempted to redefine an existing module", mod_name: nil
  end

  @doc """
  Given a list of file paths, recurse through them, parsing all Pantagruel
  files which declare a module (with the `module` keyword) and return
  a map where every module AST is available for import by name.
  """
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

  # Load and parse a file by name.
  defp load_ast(path, asts) do
    path
    |> File.read!()
    |> Scan.scan()
    |> Parse.program()
    |> include(asts)
  end

  defp include({:ok, [{:module, mod_name} | rest]}, asts) do
    case asts do
      # No two modules can declare the same module name.
      %{^mod_name => _} ->
        raise ModuleShadowError, mod_name: mod_name

      %{} ->
        Map.put(asts, mod_name, rest)
    end
  end
end
