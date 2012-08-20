defmodule Dynamo.Mixfile do
  use Mix.Project

  def project do
    [ app: :dynamo,
      version: "0.1.0.dev",
      deps: deps ]
  end

  def deps do
    [ { :mimetypes, git: "https://github.com/spawngrid/mimetypes.git" },
      { :edown, git: "https://github.com/esl/edown.git" }, ## Hackney dependency
      { :hackney, git: "https://github.com/benoitc/hackney.git" },
      { :cowboy,  git: "https://github.com/josevalim/cowboy.git" } ]
  end

  def application do
    [applications: [:mimetypes]]
  end
end