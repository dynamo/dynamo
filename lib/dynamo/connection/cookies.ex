defmodule Dynamo.Connection.Cookies do
  @moduledoc false

  @doc """
  Generates functions related to cookies. It expects a connection
  record macro with fields:

  * cookies with a Binary.Dict.new as default;
  * resp_cookies with an empty list as default;

  """
  defmacro __using__(_) do
    quote location: :keep do
      @doc """
      Returns a Binary.Dict with cookies. Cookies need to be explicitly
      fetched with `conn.fetch(:cookies)` before using this function.
      """
      def cookies(connection(cookies: nil)) do
        raise Dynamo.Connection.UnfetchedError, aspect: :cookies
      end

      def cookies(connection(cookies: cookies)) do
        cookies
      end

      @doc """
      Returns the response cookies as a list of three element tuples
      containing the key, value and given options.
      """
      def resp_cookies(connection(resp_cookies: resp_cookies)) do
        resp_cookies
      end

      @doc """
      Sets a cookie with given key and value and the given options.

      ## Options

      * `max_age` - The cookie max-age in seconds. In order to support
        older IE versions, setting `max_age` also sets the Expires, which
        the developer may customize by passing `local_time`;

      * `secure` - Marks the cookie as secure;

      * `domain` - The domain to which the cookie applies;

      * `path` - The path to which the cookie applies;

      * `http_only` - If the cookie is sent only via http. Default to true;

      """
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

      @doc """
      Deletes a cookie. The same options given when setting the cookie
      must be given on delete to ensure the browser will pick them up.
      """
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
