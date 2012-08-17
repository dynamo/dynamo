defmodule Dynamo.Connection.Response do
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
      @doc """
      Returns the response status if one was set.
      """
      def status(connection(status: status)) do
        status
      end

      @doc """
      Returns the response body if one was set.
      """
      def resp_body(connection(resp_body: resp_body)) do
        resp_body
      end

      @doc """
      Sets a response to the given status and body. The
      response will only be sent when `send` is called.

      After calling this function, the state changes to `:set`,
      both `status` and `resp_body` are set.
      """
      def resp(status, body, conn) when is_integer(status) do
        connection(conn,
          status: status,
          resp_body: body,
          state: :set
        )
      end

      @doc """
      A shortcut to `conn.send(conn.status, conn.resp_body)`.
      """
      def send(connection(status: status, resp_body: body) = conn) do
        send(status, body, conn)
      end

      @doc """
      Returns the response state. It can be:

      * `:unset` - the response was not configured yet
      * `:set` - the response was configured via `conn.resp`
      * `:chunked` - the response is being sent in chunks
      * `:sent` - the response was sent

      """
      def state(connection(state: state)) do
        state
      end

      @doc """
      Returns the response headers as `Binary.Dict`.
      """
      def resp_headers(connection(resp_headers: resp_headers)) do
        resp_headers
      end

      @doc """
      Sets a response header, overriding any previous value.
      Both `key` and `value` are converted to binary.
      """
      def set_resp_header(key, value, connection(resp_headers: resp_headers) = conn) do
        connection(conn, resp_headers: Dict.put(resp_headers, key, to_binary(value)))
      end

      @doc """
      Deletes a response header.
      """
      def delete_resp_header(key, connection(resp_headers: resp_headers) = conn) do
        connection(conn, resp_headers: Dict.delete(resp_headers, key))
      end
    end
  end
end