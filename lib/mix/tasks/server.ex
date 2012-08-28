defmodule Mix.Tasks.Server do
  use Mix.Task

  @shortdoc "Runs a Dynamo server"

  @moduledoc """
  Runs a dynamo server.

  If the application is set to `:compile_on_demand`, Dynamo
  will compile the library and generate the app file. Otherwise,
  the application needs to be compiled explicitly.

  ## Command line options

  * `:port` - the port to listen to
  * `:acceptors` - the number of acceptors

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args, aliases: [p: :port])

    project = Mix.project
    Dynamo.start

    Code.require_file project[:dynamo_app] || "config/app.ex"
    app = Dynamo.app

    if app.config[:dynamo][:compile_on_demand] do
      Mix.Task.run "compile"
    end

    app.start
    app.config[:dynamo][:handler].run app, opts
    :timer.sleep(:infinity)
  end
end
