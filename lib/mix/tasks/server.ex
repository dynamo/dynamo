defmodule Mix.Tasks.Server do
  use Mix.Task

  @shortdoc "Run all Dynamos in a web server"

  @moduledoc """
  Runs all registered Dynamos in their servers.
  """
  def run(args) do
    Mix.Task.run Mix.project[:prepare_task], args

    dynamos = Mix.project[:dynamos]
    Dynamo.Loader.enable

    Enum.each dynamos, fn(dynamo) ->
      validate_dynamo(dynamo)
      dynamo.run
    end

    unless Code.ensure_loaded?(IEx) && IEx.started? do
      :timer.sleep(:infinity)
    end
  end

  defp validate_dynamo(dynamo) do
    config   = dynamo.config[:dynamo]
    endpoint = config[:endpoint]

    if endpoint && not Code.ensure_compiled?(endpoint) do
      if config[:compile_on_demand] do
        raise "could not find endpoint #{inspect endpoint}, please ensure it is available"
      else
        raise "could not find endpoint #{inspect endpoint}, please ensure it was compiled " <>
          "by running: MIX_ENV=#{Dynamo.env} mix compile"
      end
    end
  end
end
