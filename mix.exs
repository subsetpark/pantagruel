defmodule Pantagruel.MixProject do
  use Mix.Project

  def project do
    [
      app: :pantagruel,
      version: "0.2.0",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: escript_config(),
      description: description(),
      package: package(),

      # Docs
      name: "Pantagruel",
      source_url: "https://github.com/subsetpark/pantagruel",
      docs: [
        extras: ["README.md", "priv/a_full_specification.md", "priv/reference.md"],
        markdown_processor_options: [footnotes: true]
      ],
      dialyzer: [flags: ["-Wunmatched_returns", :error_handling, :underspecs]]
    ]
  end

  defp description do
    """
    A program specification language with a defined syntax, and ad-hoc
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
      {:dialyxir, "~> 1.0.0-rc.4", only: [:dev], runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev},
      {:witchcraft, ">= 1.0.1"},
      {:type_class, "~> 1.1"}
    ]
  end

  defp escript_config do
    [main_module: Pantagruel, name: :pant]
  end

  defp package do
    [
      name: "pantagruel",
      licenses: ["BSD3"],
      links: %{"GitHub" => "https://github.com/subsetpark/pantagruel"}
    ]
  end
end
