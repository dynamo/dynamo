defmodule Dynamo.Cowboy.Connection do
  require :cowboy_http_req, as: R

  # TODO: Plan a mechanism that makes this easier
  @request              2
  @path_info_segments   3
  @script_info_segments 4
  @params               5
  @cookies              6
  @res_cookies          7

  defmacrop _(req) do
    quote do
      elem(unquote(req), 2)
    end
  end

  defmacrop _(req, value) do
    quote do
      setelem(unquote(req), 2, unquote(value))
    end
  end

  ## Request API

  @doc """
  Returns the query string as a binary.
  """
  def query_string(conn) do
    { query_string, _ } = R.raw_qs _(conn)
    query_string
  end

  @doc """
  Returns a Binary.Dict with params retrieved from the query
  string or from post body. The parameters need to be explicitly
  fetched with `conn.fetch(:params)` before using this function.
  """
  def params(conn) do
    elem(conn, @params) || raise Dynamo.Connection.UnfetchedError, aspect: :params
  end

  @doc """
  Return the path as a list of binaries split on "/".
  If the request was forwarded request, `path_info_segments` returns
  only the segments related to the current forwarded endpoint.
  """
  def path_info_segments(conn) do
    elem(conn, @path_info_segments)
  end

  @doc """
  Returns the request path relative to the forwarding endpoint
  as a binary.
  """
  def path_info(conn) do
    to_path path_info_segments(conn)
  end

  @doc """
  Returns the full path segments, as received by the web server.
  """
  def path_segments(conn) do
    { segments, _ } = R.path _(conn)
    segments
  end

  @doc """
  Returns the full path as a binary, as received by the web server.
  """
  def path(conn) do
    { binary, _ } = R.raw_path _(conn)
    binary
  end

  @doc """
  As in CGI environment, returns the current forwarded endpoint as segments.
  """
  def script_info_segments(conn) do
    elem(conn, @script_info_segments)
  end

  @doc """
  As in CGI environment, returns the current forwarded endpoint as binary.
  """
  def script_info(conn) do
    to_path script_info_segments(conn)
  end

  @doc """
  Returns the HTTP method as an atom.

  ## Examples

      request.method #=> :GET

  """
  def method(conn) do
    { verb, _ } = R.method _(conn)
    verb
  end

  @doc """
  Returns the HTTP version.
  """
  def version(conn) do
    { version, _ } = R.version _(conn)
    version
  end

  ## Response API

  @doc """
  Replies to the client with the given status, headers and body
  """
  def reply(status, headers, body, conn) do
    req = Enum.reduce elem(conn, @res_cookies), _(conn), write_cookie(&1, &2)
    { :ok, req } = R.reply(status, headers, body, req)
    conn /> _(req) /> setelem(@res_cookies, [])
  end

  ## Cookies

  @doc """
  Returns the cookies sent in the request as a `Binary.Dict`.
  Cookies need to be explicitly fetched with `conn.fetch(:cookies)`
  before using this function.
  """
  def req_cookies(conn) do
    { cookies, _ } = R.cookies _(conn)
    Binary.Dict.new(cookies)
  end

  @doc """
  Returns a Binary.Dict with cookies. Cookies need to be explicitly
  fetched with `conn.fetch(:cookies)` before using this function.
  """
  def cookies(conn) do
    elem(conn, @cookies) || raise Dynamo.Connection.UnfetchedError, aspect: :cookies
  end

  @doc """
  Returns the response cookies as a list of three element tuples
  containing the key, value and given options.
  """
  def res_cookies(conn) do
    elem(conn, @res_cookies)
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

    if cookies = elem(conn, @cookies) do
      conn = setelem(conn, @cookies, Dict.put(cookies, key, value))
    end

    res_cookies = [{ key, value, opts }|elem(conn, @res_cookies)]
    setelem(conn, @res_cookies, res_cookies)
  end

  ## Misc

  @doc """
  Returns the underlying cowboy request. This is used
  internally by Dynamo but may also be used by other
  developers (with caution).
  """
  def cowboy_request(conn) do
    _(conn)
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
    { query_string, req } = R.raw_qs _(conn)
    params = Dynamo.Connection.QueryParser.parse(query_string)
    { params, req } = Dynamo.Cowboy.BodyParser.parse(params, req)
    conn /> setelem(@request, req) /> setelem(@params, params)
  end

  def fetch(:cookies, conn) do
    { cookies, req } = R.cookies _(conn)
    conn /> setelem(@request, req) /> setelem(@cookies, Binary.Dict.new(cookies))
  end

  # Mounts the request by setting the new path information to the given
  # *segments*. Both script_info/1 and path_segments/1 are updated.
  # The segments given must be a suffix of the current path segments.
  @doc false
  def forward_to(segments, _target, req) do
    current = path_info_segments(req)
    { prefix, ^segments } = Enum.split current, length(current) - length(segments)
    req = setelem(req, @path_info_segments, segments)
    req = setelem(req, @script_info_segments, script_info_segments(req) ++ prefix)
    req
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