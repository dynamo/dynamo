defmodule Dynamo.Websocket.Behaviour do
  @moduledoc """
  Default implementation for `Dynamo.Websocket`.
  It always hibernates when possible.
  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour Dynamo.Websocket

      def init(conn) do
        { :ok, conn, :hibernate }
      end

      def handle_msg(_message, conn) do
        { :ok, conn, :hibernate }
      end

      def handle_info(_term, conn) do
        { :ok, conn, :hibernate }
      end

      def terminate(_reason, _conn) do
        :ok
      end

      defoverridable [ init: 1, handle_msg: 2,
                       handle_info: 2, terminate: 2 ]
    end
  end
end