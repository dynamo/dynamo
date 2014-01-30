defmodule Dynamo.HTTP.Cookies do
  @doc """
  Conveniences for working with cookies.
  To use them, just import this module.
  """

  @doc """
  Returns all cookies. In case you want to retrieve
  just a value, it is recommended to use `get_cookie/2`.
  """
  def get_cookies(conn) do
    Enum.reduce conn.resp_cookies, conn.req_cookies, fn({ key, value, _opts }, acc) ->
      Binary.Dict.put(acc, key, value)
    end
  end

  @doc """
  Returns the cookie value regardless if it was given sent
  on the request or set as part of the response.
  """
  def get_cookie(conn, key) do
    key = to_string(key)
    case List.keyfind(conn.resp_cookies, key, 0) do
      { ^key, value, _ } -> value
      nil -> conn.req_cookies[key]
    end
  end

  @doc """
  Puts a cookie with given key and value and the given options
  to the connection.

  ## Options

  * `max_age` - The cookie max-age in seconds. In order to support
    older IE versions, setting `max_age` also sets the Expires header;

  * `secure` - Marks the cookie as secure;

  * `domain` - The domain to which the cookie applies;

  * `path` - The path to which the cookie applies;

  * `http_only` - If the cookie is sent only via http. Default to true;

  """
  def put_cookie(conn, key, value, opts \\ []) do
    conn.put_resp_cookie(to_string(key), to_string(value), opts)
  end

  @doc """
  Deletes a cookie. The same options given when setting the cookie
  must be given on delete to ensure the browser will pick them up.
  """
  def delete_cookie(conn, key, opts \\ []) do
    unix = { { 1970, 1, 1 }, { 12, 0, 0 } }
    opts = [max_age: 0, universal_time: unix] ++ opts
    conn.put_resp_cookie(to_string(key), nil, opts)
  end
end