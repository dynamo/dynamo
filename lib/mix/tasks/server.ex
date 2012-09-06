defmodule Mix.Tasks.Server do
  use Mix.Task

  @shortdoc "Run a Dynamo app in a web server"

  @moduledoc """
  Runs a dynamo server.

  In order to know how the application behaves, this task
  needs to load the file dynamo app usually at `config/app.ex`.
  In production, this file is already compiled, so the loading
  step can be explicitly skipped by passing --app as an option.

  If the application is set to `:compile_on_demand`, Dynamo
  will compile everything on demand, otherwise, the application
  needs to be compiled explicitly.

  ## Command line options

  * `--app` - the name of the application to load
  * `-p`, `--port` - the port to listen to
  * `--acceptors` - the number of acceptors

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args, aliases: [p: :port])
    Mix.Task.run "dynamo.app"

    opts = Keyword.merge [port: 4000], opts
    Dynamo.app.run opts

    :timer.sleep(:infinity)
  end
end
