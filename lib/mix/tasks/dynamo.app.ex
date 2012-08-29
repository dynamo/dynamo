defmodule Mix.Tasks.Dynamo.App do
  use Mix.Task

  @moduledoc """
  An internal task that loads and starts up the Dynamo app.
  It can be used as dependency by other tasks.
  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)
    Dynamo.start

    if app = opts[:app] do
      Module.concat([app]).start
    else
      Code.require_file Mix.project[:dynamo_app] || "config/app.ex"
      app = Dynamo.app || raise "Dynamo.app is not available"

      if app.config[:dynamo][:compile_on_demand] do
        Mix.Task.run "compile"
      else
        Mix.Task.run "loadpaths"
      end

      app.start
    end
  end
end
