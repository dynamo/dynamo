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
      def before_send(fun, connection(before_send: before_send) = conn) when is_function(fun) do
        connection(conn, before_send: [fun|before_send])
      end

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

      def resp_content_type(connection(resp_content_type: resp_content_type)) do
        resp_content_type
      end

      def resp_content_type(resp_content_type, conn) when is_binary(resp_content_type) do
        connection(conn, resp_content_type: resp_content_type)
      end

      def resp_charset(connection(resp_charset: resp_charset)) do
        resp_charset
      end

      def resp_charset(resp_charset, conn) when is_binary(resp_charset) do
        connection(conn, resp_charset: resp_charset)
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

      defp default_before_send do
        [
          fn(conn) ->
            if content_type = conn.resp_content_type do
              if charset = conn.resp_charset do
                content_type = content_type <> "; charset=" <> charset
              end
              conn.set_resp_header("content-type", content_type)
            else
              conn
            end
          end
        ]
      end

      defp run_before_send(connection(before_send: before_send) = conn) do
        Enum.reduce Enum.reverse(before_send), conn, fn(fun, c) -> fun.(c) end
      end
    end
  end
end