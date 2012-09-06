defmodule Dynamo.App.Runner do
  @moduledoc """
  Basic functionality which allows us to run and
  customize a Dynamo app. For instance, you can
  define your own run variation that pass custom
  options to the handler:

      def run(options) do
        options = Keyword.merge(options, custom_cowboy_options)
        Dynamo.Cowboy.run __MODULE__, options
      end

  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @doc """
      Runs the app in a web server.
      """
      def run(options // []) do
        Dynamo.Cowboy.run __MODULE__, options
      end

      defoverridable [run: 1]
    end
  end
end