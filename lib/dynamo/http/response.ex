defmodule Dynamo.HTTP.Response do
  @moduledoc false

  @doc """
  Generates functions related to the response. It expects a connection
  record macro with fields:

  * status with nil as default;
  * resp_body with nil as default;
  * state with `:unset` as default;

  """
  defmacro __using__(_) do
    quote location: :keep do
      def status(connection(status: status)) do
        status
      end

      def status(status, conn) when is_integer(status) do
        connection(conn, status: status, state: :set)
      end

      def resp_body(connection(resp_body: resp_body)) do
        resp_body
      end

      def resp_body(body, conn) do
        connection(conn, resp_body: body, state: :set)
      end

      def resp(status, body, conn) when is_integer(status) do
        connection(conn,
          status: status,
          resp_body: body,
          state: :set
        )
      end

      def send(connection(status: status, resp_body: body) = conn) do
        send(status, body, conn)
      end

      def state(connection(state: state)) do
        state
      end

      def resp_headers(connection(resp_headers: resp_headers)) do
        resp_headers
      end

      def set_resp_header(key, value, connection(resp_headers: resp_headers) = conn) do
        connection(conn, resp_headers: Dict.put(resp_headers, key, to_binary(value)))
      end

      def delete_resp_header(key, connection(resp_headers: resp_headers) = conn) do
        connection(conn, resp_headers: Dict.delete(resp_headers, key))
      end
    end
  end
end