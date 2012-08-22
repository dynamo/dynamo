defmodule Dynamo.HTTP.Cookies do
  @moduledoc false

  @doc """
  Generates functions related to cookies. It expects a connection
  record macro with fields:

  * cookies with a Binary.Dict.new as default;
  * resp_cookies with an empty list as default;

  """
  defmacro __using__(_) do
    quote location: :keep do
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

        resp_cookies = List.keydelete(resp_cookies, key, 1)
        connection(conn, cookies: cookies, resp_cookies: [{ key, value, opts }|resp_cookies])
      end

      def delete_cookie(key, opts // [],
          connection(cookies: cookies, resp_cookies: resp_cookies) = conn) do
        key  = to_binary(key)
        unix = { { 1970, 1, 1 }, { 12, 0, 0 } }
        opts = Keyword.merge(opts, max_age: 0, local_time: unix)

        if cookies do
          cookies = Dict.delete(cookies, key)
        end

        resp_cookies = List.keydelete(resp_cookies, key, 1)
        connection(conn, cookies: cookies, resp_cookies: [{ key, "", opts }|resp_cookies])
      end
    end
  end
end
