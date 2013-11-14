defmodule SimpleEcto.Mixfile do
  use Mix.Project

  def project do
    [ app: :simple_ecto,
      version: "0.0.1",
      dynamos: [SimpleEcto.Dynamo],
      compilers: [:elixir, :dynamo, :app],
      env: [prod: [compile_path: "ebin"]],
      compile_path: "tmp/#{Mix.env}/simple_ecto/ebin",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:cowboy, :dynamo],
      mod: { SimpleEcto, [] } ]
  end

  defp deps do
    [ { :cowboy, github: "extend/cowboy" },
      { :dynamo, "0.1.0-dev", path: "../.." },
      { :postgrex, github: "ericmj/postgrex" },
      { :ecto, github: "elixir-lang/ecto"} ]
  end
end
