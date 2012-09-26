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
    [applications: [:crypto, :mimetypes]]
  end
end