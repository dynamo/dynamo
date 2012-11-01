defmodule Dynamo.HTTP.Behaviour do
  @moduledoc """
  Common behaviour to be used between Dynamo connection
  implementations. It expects a Record.defmacro connection
  to be defined with the following keys - defaults:

  * assigns - an empty list
  * before_send - a call to `Dynamo.HTTP.default_before_send`
  * method - the current request method
  * original_method - `nil`
  * state - `:unset`
  * status - `nil`
  * params - `nil`
  * path_info_segments - the current path segments
  * req_headers - `nil`
  * resp_body - an empty binary
  * resp_charset - `"utf-8"`
  * resp_content_type - `nil`
  * script_name_segments - an empty list

  """

  @doc false
  defmacro __using__(_) do

    quote location: :keep do
      @behaviour Dynamo.HTTP

      ## Assigns

      def assigns(connection(assigns: assigns)) do
        assigns
      end

      def assign(key, value, connection(assigns: assigns) = conn) do
        connection(conn, assigns: Keyword.put(assigns, key, value))
      end

      def app(connection(app: app)) do
        app
      end

      ## Request

      def params(connection(params: nil)) do
        raise Dynamo.HTTP.UnfetchedError, aspect: :params
      end

      def params(connection(params: params)) do
        params
      end

      def method(connection(method: method)) do
        method
      end

      def original_method(connection(original_method: method)) do
        method
      end

      def method(method, connection(method: original_method) = conn) when is_binary(method) do
        connection(conn,
          method: method,
          original_method: original_method)
      end

      def req_headers(connection(req_headers: nil)) do
        raise Dynamo.HTTP.UnfetchedError, aspect: :req_headers
      end

      def req_headers(connection(req_headers: req_headers)) do
        req_headers
      end

      def req_body(connection(req_body: nil)) do
        raise Dynamo.HTTP.UnfetchedError, aspect: :req_body
      end

      def req_body(connection(req_body: req_body)) do
        req_body
      end

      ## Cookies

      def cookies(connection(cookies: nil)) do
        raise Dynamo.HTTP.UnfetchedError, aspect: :cookies
      end

      def cookies(connection(cookies: cookies)) do
        cookies
      end

      def resp_cookies(connection(resp_cookies: resp_cookies)) do
        resp_cookies
      end

      def set_cookie(key, value, opts // [],
          connection(cookies: cookies, resp_cookies: resp_cookies) = conn) do
        key   = to_binary(key)
        value = to_binary(value)

        if cookies do
          cookies = Dict.put(cookies, key, value)
        end

        resp_cookies = List.keydelete(resp_cookies, key, 0)
        connection(conn, cookies: cookies, resp_cookies: [{ key, value, opts }|resp_cookies])
      end

      def delete_cookie(key, opts // [],
          connection(cookies: cookies, resp_cookies: resp_cookies) = conn) do
        key  = to_binary(key)
        unix = { { 1970, 1, 1 }, { 12, 0, 0 } }
        opts = Keyword.merge(opts, max_age: 0, universal_time: unix)

        if cookies do
          cookies = Dict.delete(cookies, key)
        end

        resp_cookies = List.keydelete(resp_cookies, key, 0)
        connection(conn, cookies: cookies, resp_cookies: [{ key, "", opts }|resp_cookies])
      end

      ## Paths

      def path_info_segments(connection(path_info_segments: segments)) do
        segments
      end

      def path_info(connection(path_info_segments: segments)) do
        to_path segments
      end

      def script_name_segments(connection(script_name_segments: segments)) do
        segments
      end

      def script_name(connection(script_name_segments: segments)) do
        to_path segments
      end

      def forward_to(segments, _target,
          connection(path_info_segments: path, script_name_segments: script) = conn) do
        { prefix, ^segments } = Enum.split path, length(path) - length(segments)

        connection(conn,
          path_info_segments: segments,
          script_name_segments: script ++ prefix
        )
      end

      defp to_path(segments) do
        "/" <> Enum.join(segments, "/")
      end

      ## Response

      def status(connection(status: status)) do
        status
      end

      def status(status, connection(state: state) = conn) when
          is_integer(status) and state in [:unset, :set] do
        connection(conn, status: status, state: :set)
      end

      def resp_body(connection(resp_body: resp_body)) do
        resp_body
      end

      def resp_body(body, connection(status: status, state: state) = conn) when state in [:unset, :set] do
        connection(conn,
          status: status || 200,
          resp_body: body,
          state: :set)
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

      # Callbacks

      def before_send(fun, connection(before_send: before_send) = conn) when is_function(fun) do
        connection(conn, before_send: [fun|before_send])
      end

      defp run_before_send(connection(before_send: before_send) = conn) do
        Enum.reduce Enum.reverse(before_send), conn, fn(fun, c) -> fun.(c) end
      end
    end

  end
end