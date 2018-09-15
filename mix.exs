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
      dialyzer: [plt_add_apps: [:mix]]
    ]
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
      {:nimble_parsec, "~> 0.2"}
    ]
  end

  defp escript_config do
    [main_module: Pantagruel]
  end
end
