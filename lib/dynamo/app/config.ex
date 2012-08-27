defmodule Dynamo.App.Config do
  @moduledoc """
  Holds the configuration DSL available in Dynamo.App.
  """

  @doc """
  Defines a configuration for the given key.
  """
  defmacro config(key, opts) do
    quote do
      key    = unquote(key)
      config = @config
      merged = Keyword.merge(config[key] || [], unquote(opts))
      @config Keyword.put(config, key, merged)
    end
  end

  @doc """
  Defines the default endpoint to dispatch to.
  """
  defmacro endpoint(endpoint) do
    quote do
      def service(conn) do
        unquote(endpoint).service(conn)
      end
    end
  end

  @doc false
  defmacro __using__(_) do
    quote do
      @config []
      @before_compile unquote(__MODULE__)
      import Dynamo.App.Config, only: [endpoint: 1, config: 2]
    end
  end

  @doc false
  defmacro before_compile(_mod) do
    quote do
      def config do
        @config
      end
    end
  end
end