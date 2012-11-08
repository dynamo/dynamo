defmodule Mix.Tasks.Dynamo.Start do
  use Mix.Task

  @moduledoc """
  A task that loads and starts up all registered
  Dynamo apps. It is usually used as dependency
  by other tasks.
  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args)
    dynamos = Mix.project[:dynamos]

    unless dynamos do
      raise Mix.Error, message: %b(configuration :dynamos not set for #{inspect Mix.Project.get!})
    end

    Dynamo.start Mix.env
    Mix.Task.run "compile", args

    unless opts[:no_start] do
      Enum.each dynamos, fn(dynamo) ->
        if Code.ensure_loaded?(dynamo) do
          dynamo.start
        else
          raise Mix.Error, message: %b(could not load Dynamo #{inspect dynamo}) 
        end
      end
    end
  end
end
