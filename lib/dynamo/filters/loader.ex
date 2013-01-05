defmodule Dynamo.Filters.Loader do
  @moduledoc """
  A simple module that handles compilation on demand
  and automatic code loading. This filter is automatically
  by Dynamos based on your configuration options.
  """

  @doc false
  def new(compile_on_demand, reload_modules) do
    { __MODULE__, compile_on_demand, reload_modules }
  end

  @doc false
  def prepare(conn, { __MODULE__, compile_on_demand, reload_modules }) do
    if compile_on_demand do
      Dynamo.Loader.enable
    end

    if reload_modules do
      if Dynamo.Loader.conditional_purge == :purged do
        IO.puts "[Dynamo] Changes detected, reloading modules..."
      end
    end

    conn
  end
end