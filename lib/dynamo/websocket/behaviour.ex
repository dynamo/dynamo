defmodule Dynamo.Websocket.Behaviour do
  @moduledoc """
  Default implementation for `Dynamo.Websocket`.
  It always hibernates when possible.
  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behaviour Dynamo.Websocket

      def websocket_init(conn) do
        { :ok, conn, :hibernate }
      end

      def websocket_handle(_message, conn) do
        { :ok, conn, :hibernate }
      end

      def websocket_info(_term, conn) do
        { :ok, conn, :hibernate }
      end

      def websocket_terminate(_reason, _conn) do
        :ok
      end
    end
  end
end