defmodule Mana.Mixfile do
  use Mix.Project

  def project do
    [ app: :mana,
      version: "0.0.1",
      elixir: "~> 0.10.2-dev",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ registered: [Mana],
      applications: [:socket, :dexts],
      mod: { Mana, [] } ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [ { :socket, github: "meh/elixir-socket" },
      { :dexts, github: "meh/dexts" },
      { :continuum, github: "meh/continuum" },
      { :exquisite, github: "meh/exquisite" } ]
  end
end
