defmodule Mix.Tasks.Dynamo.App do
  use Mix.Task

  @moduledoc """
  An task that loads and starts up the Dynamo app.
  It is usually used as dependency by other tasks.
  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)
    Dynamo.start

    app =
      cond do
        Dynamo.app -> Dynamo.app
        opts[:app] -> load_app_from_opts(opts[:app])
        true       -> load_app_from_file
      end

    app.start
    app
  end

  defp load_app_from_opts(app) do
    Module.concat([app])
  end

  defp load_app_from_file do
    Code.require_file Mix.project[:dynamo_app] || "config/app.ex"
    app = Dynamo.app || raise "Dynamo.app is not available"

    if app.config[:dynamo][:compile_on_demand] do
      Mix.Task.run "compile"
    else
      Mix.Task.run "deps.check"
    end

    app
  end
end
