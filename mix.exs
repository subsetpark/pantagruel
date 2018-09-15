defmodule Pantagruel.MixProject do
  use Mix.Project

  def project do
    [
      app: :pantagruel,
      version: "0.0.1",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: escript_config(),
      dialyzer: [plt_add_apps: [:mix]],
      description: description(),
      package: package()
    ]
  end

  defp description do
    """
    A program specification language with a defined syntax, but ad-hoc
    semantics.
    """
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:nimble_parsec, "~> 0.2"},
      {:ex_doc, ">= 0.0.0", only: :dev}
    ]
  end

  defp escript_config do
    [main_module: Pantagruel]
  end

  defp package do
    [
      name: "pantagruel",
      licenses: ["BSD3"],
      links: %{"GitHub" => "https://github.com/subsetpark/pantagruel"}
    ]
  end
end
