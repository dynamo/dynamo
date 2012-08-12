defmodule Dynamo.Cowboy.Connection do
  require :cowboy_http_req, as: R

  ## Utilities
  ## TODO: Generate those automatically

  defmacrop _request(conn) do
    quote do
      elem(unquote(conn), 2)
    end
  end

  defmacrop _request(conn, value) do
    quote do
      setelem(unquote(conn), 2, unquote(value))
    end
  end

  defmacrop _path_info_segments(conn) do
    quote do
      elem(unquote(conn), 3)
    end
  end

  defmacrop _path_info_segments(conn, value) do
    quote do
      setelem(unquote(conn), 3, unquote(value))
    end
  end

  defmacrop _script_info_segments(conn) do
    quote do
      elem(unquote(conn), 4)
    end
  end

  defmacrop _script_info_segments(conn, value) do
    quote do
      setelem(unquote(conn), 4, unquote(value))
    end
  end

  defmacrop _params(conn) do
    quote do
      elem(unquote(conn), 5)
    end
  end

  defmacrop _params(conn, value) do
    quote do
      setelem(unquote(conn), 5, unquote(value))
    end
  end

  defmacrop _cookies(conn) do
    quote do
      elem(unquote(conn), 6)
    end
  end

  defmacrop _cookies(conn, value) do
    quote do
      setelem(unquote(conn), 6, unquote(value))
    end
  end

  defmacrop _res_cookies(conn) do
    quote do
      elem(unquote(conn), 7)
    end
  end

  defmacrop _res_cookies(conn, value) do
    quote do
      setelem(unquote(conn), 7, unquote(value))
    end
  end

  ## Request API

  @doc """
  Returns the query string as a binary.
  """
  def query_string(conn) do
    { query_string, _ } = R.raw_qs _request(conn)
    query_string
  end

  @doc """
  Returns a Binary.Dict with params retrieved from the query
  string or from post body. The parameters need to be explicitly
  fetched with `conn.fetch(:params)` before using this function.
  """
  def params(conn) do
    _params(conn) || raise Dynamo.Connection.UnfetchedError, aspect: :params
  end

  @doc """
  Return the path as a list of binaries split on "/".
  If the request was forwarded request, `path_info_segments` returns
  only the segments related to the current forwarded endpoint.
  """
  def path_info_segments(conn) do
    _path_info_segments(conn)
  end

  @doc """
  Returns the request path relative to the forwarding endpoint
  as a binary.
  """
  def path_info(conn) do
    to_path _path_info_segments(conn)
  end

  @doc """
  Returns the full path segments, as received by the web server.
  """
  def path_segments(conn) do
    { segments, _ } = R.path _request(conn)
    segments
  end

  @doc """
  Returns the full path as a binary, as received by the web server.
  """
  def path(conn) do
    { binary, _ } = R.raw_path _request(conn)
    binary
  end

  @doc """
  As in CGI environment, returns the current forwarded endpoint as segments.
  """
  def script_info_segments(conn) do
    _script_info_segments(conn)
  end

  @doc """
  As in CGI environment, returns the current forwarded endpoint as binary.
  """
  def script_info(conn) do
    to_path _script_info_segments(conn)
  end

  @doc """
  Returns the HTTP method as an atom.

  ## Examples

      request.method #=> :GET

  """
  def method(conn) do
    { verb, _ } = R.method _request(conn)
    verb
  end

  @doc """
  Returns the HTTP version.
  """
  def version(conn) do
    { version, _ } = R.version _request(conn)
    version
  end

  ## Response API

  @doc """
  Replies to the client with the given status, headers and body
  """
  def reply(status, headers, body, conn) do
    req = Enum.reduce _res_cookies(conn), _request(conn), write_cookie(&1, &2)
    { :ok, req } = R.reply(status, headers, body, req)
    _res_cookies(_request(conn, req), [])
  end

  ## Cookies

  @doc """
  Returns the cookies sent in the request as a `Binary.Dict`.
  Cookies need to be explicitly fetched with `conn.fetch(:cookies)`
  before using this function.
  """
  def req_cookies(conn) do
    { cookies, _ } = R.cookies _request(conn)
    Binary.Dict.new(cookies)
  end

  @doc """
  Returns a Binary.Dict with cookies. Cookies need to be explicitly
  fetched with `conn.fetch(:cookies)` before using this function.
  """
  def cookies(conn) do
    _cookies(conn) || raise Dynamo.Connection.UnfetchedError, aspect: :cookies
  end

  @doc """
  Returns the response cookies as a list of three element tuples
  containing the key, value and given options.
  """
  def res_cookies(conn) do
    _res_cookies(conn)
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
  def set_cookie(key, value, opts // [], conn) do
    key   = to_binary(key)
    value = to_binary(value)

    if cookies = _cookies(conn) do
      conn = _cookies(conn, Dict.put(cookies, key, value))
    end

    _res_cookies(conn, [{ key, value, opts }|_res_cookies(conn)])
  end

  ## Misc

  @doc """
  Returns the underlying cowboy request. This is used
  internally by Dynamo but may also be used by other
  developers (with caution).
  """
  def cowboy_request(conn) do
    _request(conn)
  end

  ## Internal

  @doc """
  Builds a new Dynamo.Cowboy.Request based on
  the original Cowboy request object.
  """
  def new(req) do
    { segments, req } = R.path(req)
    { __MODULE__, req, segments, [], nil, nil, [] }
  end

  @doc """
  Responsible for fetching and caching aspects of the response.
  The "fetchable" aspects are: params, cookies and session.
  """
  def fetch(:params, conn) do
    { query_string, req } = R.raw_qs _request(conn)
    params = Dynamo.Connection.QueryParser.parse(query_string)
    { params, req } = Dynamo.Cowboy.BodyParser.parse(params, req)
    _params(_request(conn, req), params)
  end

  def fetch(:cookies, conn) do
    { cookies, req } = R.cookies _request(conn)
    _cookies(_request(conn, req), Binary.Dict.new(cookies))
  end

  # Mounts the request by setting the new path information to the given
  # *segments*. Both script_info/1 and path_segments/1 are updated.
  # The segments given must be a suffix of the current path segments.
  @doc false
  def forward_to(segments, _target, conn) do
    current = _path_info_segments(conn)
    { prefix, ^segments } = Enum.split current, length(current) - length(segments)
    conn = _path_info_segments(conn, segments)
    _script_info_segments(conn, _script_info_segments(conn) ++ prefix)
  end

  ## Helpers

  defp write_cookie({ key, value, opts }, req) do
    opts = Keyword.update(opts, :http_only, true, fn(x) -> x end)
    { :ok, req } = R.set_resp_cookie(key, value, opts, req)
    req
  end

  defp to_path(segments) do
    "/" <> Enum.join(segments, "/")
  end
end