defmodule Dynamo.App.Base do
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

      @doc """
      Receives a connection and dispatches it to #{unquote(endpoint)}
      """
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
      import Dynamo.App.Base

      @config []
      @before_compile unquote(__MODULE__)

      Module.register_attribute __MODULE__, :initializers, accumulate: true

      @doc """
      Returns the code to be injected in each view
      to expose default functionality.
      """
      def views do
        quote do
          use Dynamo.Helpers
        end
      end

      @doc """
      Runs the app in the configured web server.
      """
      def run(options // []) do
        Dynamo.Cowboy.run __MODULE__, options
      end

      defoverridable [views: 0, run: 1, run: 0]
    end
  end

  @doc false
  defmacro __before_compile__(mod) do
    initializers = Module.get_attribute(mod, :initializers)

    quote location: :keep do
      @doc """
      Starts the application by running all registered
      initialziers. Check `Dynamo.App` for more information.
      """
      def start do
        unquote(Enum.reverse initializers)
        __MODULE__
      end

      @doc """
      Returns the configuration for this application.
      """
      def config do
        @config
      end

      @doc """
      Returns the registered endpoint.
      """
      def endpoint do
        @endpoint
      end
    end
  end
end