defmodule Mix.Tasks.Dynamo.Filters do
  use Mix.Task

  @shortdoc "Print all dynamos filters"
  @recursive true

  @moduledoc """
  Prints all dynamos filters
  """
  def run(args) do
    Mix.Task.run "app.start", args
    shell = Mix.shell

    Enum.each (Mix.project[:dynamos] || []), fn(dynamo) ->
      shell.info "# #{inspect dynamo}"

      Enum.each dynamo.__filters__, fn(filter) ->
        shell.info "filter #{inspect filter}"
      end

    endpoint = dynamo.config[:dynamo][:endpoint] || dynamo
      shell.info "#{inspect endpoint}.service/1"
      shell.info ""
    end
  end
end
