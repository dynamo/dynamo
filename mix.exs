defmodule Dynamo.Mixfile do
  use Mix.Project

  def project do
    [ app: :dynamo,
      elixir: "~> 0.13.3 or ~> 0.14.0-dev",
      version: "0.1.0-dev",
      name: "Dynamo",
      source_url: "https://github.com/dynamo/dynamo",
      deps: deps(Mix.env),
      docs: &docs/0 ]
  end

  def deps(:prod) do
    [ { :mime,   github: "dynamo/mime" },
      { :cowboy, github: "extend/cowboy", optional: true } ]
  end

  def deps(:docs) do
    deps(:prod) ++
      [ { :ex_doc, github: "elixir-lang/ex_doc" } ]
  end

  def deps(_) do
    deps(:prod) ++
      [ { :hackney, github: "benoitc/hackney", tag: "0.12.1" } ]
  end

  def application do
    [ applications: [:crypto],
      env: [under_test: nil],
      mod: { Dynamo.App, [] } ]
  end

  defp docs do
    [ readme: true,
      main: "README",
      source_ref: System.cmd("git rev-parse --verify --quiet HEAD") ]
  end
end
