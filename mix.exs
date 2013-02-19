defmodule Dynamo.Mixfile do
  use Mix.Project

  def project do
    [ app: :dynamo,
      version: "0.1.0.dev",
      deps: deps,
      env:
        [test: [deps: test_deps]] ]
  end

  def deps do
    [ { :mimetypes, github: "spawngrid/mimetypes" },
      { :ranch,     github: "extend/ranch" },
      { :cowboy,    github: "extend/cowboy" } ]
  end

  def test_deps do
    deps ++
      [ { :edown,   github: "esl/edown" }, ## Hackney dependency
        { :hackney, github: "benoitc/hackney" } ]
  end

  def application do
    [ applications: [:crypto, :mimetypes],
      env: [under_test: nil],
      mod: { Dynamo.App, [] } ]
  end
end

defmodule Mix.Tasks.Docs do
  @shortdoc "Generates docs"

  def run(_) do
    Mix.Task.run "loadpaths"

	  Mix.shell.cmd %b[elixir -pa ebin ../exdoc/bin/exdoc "Dynamo" "#{Mix.project[:version]}" ] <>
	    %b[-m Dynamo -u "https://github.com/elixir-lang/dynamo/blob/master/%{path}#L%{line}"]
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
