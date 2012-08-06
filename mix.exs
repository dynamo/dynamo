defmodule Dynamo.Mixfile do
  use Mix.Project

  def project do
    [ app: :dynamo,
      version: "0.1.0.dev",
      deps: deps ]
  end

  def deps do
    [ { :ibrowse, git: "https://github.com/cmullaparthi/ibrowse.git" },
      { :cowboy,  git: "https://github.com/extend/cowboy.git" } ]
  end
end