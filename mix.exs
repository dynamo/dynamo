defmodule Dynamo.Mixfile do
  use Mix.Project

  def project do
    [ app: :dynamo,
      elixir: "~> 0.10.2",
      version: "0.1.0-dev",
      name: "Dynamo",
      source_url: "https://github.com/elixir-lang/dynamo",
      deps: deps(Mix.env) ]
  end

  def deps(:prod) do
    [ { :mimetypes, github: "spawngrid/mimetypes", override: true } ]
  end

  def deps(:docs) do
    deps(:prod) ++
      [ { :ex_doc, github: "elixir-lang/ex_doc" } ]
  end

  def deps(_) do
    deps(:prod) ++
      [ { :hackney, github: "benoitc/hackney" },
        { :cowboy,  github: "extend/cowboy" } ]
  end

  def application do
    [ applications: [:crypto, :mimetypes],
      env: [under_test: nil],
      mod: { Dynamo.App, [] } ]
  end
end

defmodule Mix.Tasks.Release_docs do
  @shortdoc "Releases docs"

  def run(_) do
    Mix.Task.run "docs"

    File.cd! "../elixir-lang.github.com", fn -> System.cmd "git checkout master" end
    File.rm_rf "../elixir-lang.github.com/docs/dynamo"
    File.cp_r "docs/.", "../elixir-lang.github.com/docs/dynamo/"
    File.rm_rf "docs"
  end
end
