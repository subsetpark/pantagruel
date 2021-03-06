defmodule Pantagruel.Load do
  @moduledoc """
  Provides the functionality for loading Pantagruel files, making them
  available for import.
  """
  alias Pantagruel.Parse

  defmodule ModuleShadowError do
    defexception message: "Attempted to redefine an existing module", mod_name: nil
  end

  defmodule ModuleLoadError do
    defexception message: "Module could not be loaded.", error: nil
  end

  @doc """
  Given a list of file paths, recurse through them, parsing all Pantagruel
  files which declare a module (with the `module` keyword) and return
  a map where every module AST is available for import by name.
  """
  def load(paths) do
    try do
      {:ok, Enum.reduce(paths, %{}, &load_asts/2)}
    rescue
      e in ModuleShadowError ->
        {:error, {:module_shadow, e}}

      e in ModuleLoadError ->
        {:error, {:module_load, e}}
    end
  end

  defp load_asts(path, asts) do
    [path, "**", "*.pant"]
    |> Path.join()
    |> Path.wildcard()
    |> Enum.reduce(asts, &load_ast/2)
  end

  # Load and parse a file by name.
  defp load_ast(path, asts) do
    path
    |> File.read!()
    |> Pantagruel.Scan.scan()
    |> :pant_lexer.string()
    |> Parse.handle_lex()
    |> include(asts)
  end

  defp include({:ok, {:program, [mod_name, _, _]} = program}, asts) do
    case asts do
      # No two modules can declare the same module name.
      %{^mod_name => existing_ast} when existing_ast != program ->
        raise ModuleShadowError, mod_name: mod_name

      %{} ->
        Map.put(asts, mod_name, program)
    end
  end

  defp include({:ok, _}, asts), do: asts
  defp include(error, _), do: raise(ModuleLoadError, error: error)
end
