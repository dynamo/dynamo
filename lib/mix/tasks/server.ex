defmodule Mix.Tasks.Server do
  use Mix.Task

  @shortdoc "Runs a Dynamo server"

  @moduledoc """
  Runs a dynamo server.

  ## Command line options

  """
  def run(_args) do
    project = Mix.project
    Dynamo.start

    Code.require_file project[:dynamo_app] || "config/app.ex"
    app = Dynamo.app

    Dynamo.start_app(app)
    app.config[:dynamo][:handler].run app
    :timer.sleep(:infinity)
  end
end
