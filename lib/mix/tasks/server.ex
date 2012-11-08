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
    Mix.Task.run Mix.project[:prepare_task], args

    app = Enum.first Mix.project[:dynamos]
    endpoint = app.endpoint

    if endpoint && not Code.ensure_compiled?(endpoint) do
      if app.config[:dynamo][:compile_on_demand] do
        raise "could not find endpoint #{inspect endpoint}, please ensure it is available"
      else
        raise "could not find endpoint #{inspect endpoint}, please ensure it was compiled " <>
          "by running: MIX_ENV=#{Dynamo.env} mix compile"
      end
    end

    opts = Keyword.merge [port: 4000], opts
    app.run opts

    :timer.sleep(:infinity)
  end
end
