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
      @endpoint unquote(endpoint)

      def service(conn) do
        @endpoint.service(conn)
      end
    end
  end

  @doc """
  Defines default functionality available in views.
  """
  defmacro views(contents) do
    quote do
      def views do
        unquote(Macro.escape(contents))
      end
    end
  end

  @doc """
  Defines an initializer that will be invoked when
  the application starts.
  """
  defmacro initializer(name, [do: block]) do
    quote do
      name = :"__initializer_#{unquote(name)}"
      @initializers { name, unquote(__CALLER__.line), [] }
      defp name, [], [], do: unquote(Macro.escape block)
    end
  end

  @doc false
  defmacro __using__(_) do
    quote do
      import Dynamo.App.Config

      @config []
      @before_compile unquote(__MODULE__)

      Module.register_attribute __MODULE__, :initializers, accumulate: true

      def views do
        quote do
          use Dynamo.View.Helpers
        end
      end

      defoverridable [views: 0]
    end
  end

  @doc false
  defmacro __before_compile__(mod) do
    initializers = Module.get_attribute(mod, :initializers)

    quote location: :keep do
      def start do
        unquote(Enum.reverse initializers)
        __MODULE__
      end

      def config do
        @config
      end

      def endpoint do
        @endpoint
      end
    end
  end
end