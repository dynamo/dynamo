defmodule Dynamo.Reloader.Filter do
  @moduledoc """
  A simple module that handles compilation on demand
  and automatic code loading.

  ## Examples

      defmodule MyApp do
        use Dynamo.App
        filter Dynamo.Reloader.Filter.new(true, false)
      end

  """

  @doc false
  def new(compile_on_demand, reload_modules) do
    { __MODULE__, compile_on_demand, reload_modules }
  end

  @doc false
  def prepare(conn, { __MODULE__, compile_on_demand, reload_modules }) do
    if compile_on_demand do
      Dynamo.Reloader.enable!
    end

    if reload_modules do
      if Dynamo.Reloader.conditional_purge == :purged do
        IO.puts "[Dynamo] Changes detected, reloading modules..."
      end
    end

    conn
  end
end