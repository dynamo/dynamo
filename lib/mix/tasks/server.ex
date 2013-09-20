defmodule Mix.Tasks.Server do
  use Mix.Task

  @shortdoc "Run all Dynamos in a web server"
  @recursive true

  @moduledoc """
  Runs all registered Dynamos in their servers.

  ## Command line options

    * `-h`, `--host` - bind to the given ip
    * `-p`, `--port` - the port to listen to

  """
  def run(args) do
    opts = OptionParser.parse(args, aliases: [h: :host, p: :port]) |> elem(0)
    Mix.Task.run "app.start", args

    dynamos = Mix.project[:dynamos]

    if length(dynamos) != 1 && (opts[:host] || opts[:port]) do
      raise "cannot pass host/port when serving more than one dynamo"
    end

    if opts[:port] do
      opts = Keyword.update!(opts, :port, &binary_to_integer(&1))
    end

    Dynamo.Loader.enable

    Enum.each dynamos, fn(dynamo) ->
      validate_dynamo(dynamo)
      dynamo.run(opts)
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
