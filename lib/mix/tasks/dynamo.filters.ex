defmodule Mix.Tasks.Dynamo.Filters do
  use Mix.Task

  @shortdoc "Print all dynamos filters"

  @moduledoc """
  Prints all dynamos filters
  """
  def run(args) do
    Mix.Task.run Mix.project[:prepare_task], args
    shell = Mix.shell

    Enum.each Mix.project[:dynamos], fn(dynamo) ->
      shell.info "# #{inspect dynamo}"

      Enum.each dynamo.filters, fn(filter) ->
        shell.info "filter #{inspect filter}"
      end

      shell.info "#{inspect dynamo.endpoint || dynamo}.service/1"
      shell.info ""
    end
  end
end
