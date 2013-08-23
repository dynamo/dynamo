defmodule Dynamo.Base do
  @moduledoc """
  Holds the base configuration available in a Dynamo.
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
  Defines default functionality available in templates.
  """
  defmacro templates(do: contents) do
    quote do
      def templates_prelude do
        unquote(Macro.escape(contents))
      end
    end
  end

  @doc """
  Defines an initializer that will be invoked when
  the dynamo starts.
  """
  defmacro initializer(name, do: block) do
    block = Macro.escape(block)
    quote bind_quoted: binding do
      name = :"initializer_#{name}"
      @initializers { name, [line: __ENV__.line], [] }
      defp unquote(name)(), do: unquote(block)
    end
  end

  @doc false
  defmacro __using__(_) do
    quote do
      import Dynamo.Base
      @before_compile unquote(__MODULE__)

      # Base attributes
      @config []
      Module.register_attribute __MODULE__, :initializers, accumulate: true

      @doc """
      Returns the code to be injected in each template
      to expose default functionality.
      """
      def templates_prelude do
        quote do
          use Dynamo.Helpers
        end
      end

      defoverridable [templates_prelude: 0]
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    initializers = Module.get_attribute(env.module, :initializers)

    quote location: :keep do
      @doc """
      Returns the configuration for this dynamo.
      """
      def config do
        @config
      end

      defp run_initializers do
        unquote(Enum.reverse initializers)
      end
    end
  end
end