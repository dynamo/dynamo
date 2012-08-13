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

  defmacrop _req_headers(conn) do
    quote do
      elem(unquote(conn), 5)
    end
  end

  defmacrop _req_headers(conn, value) do
    quote do
      setelem(unquote(conn), 5, unquote(value))
    end
  end

  defmacrop _params(conn) do
    quote do
      elem(unquote(conn), 6)
    end
  end

  defmacrop _params(conn, value) do
    quote do
      setelem(unquote(conn), 6, unquote(value))
    end
  end

  defmacrop _cookies(conn) do
    quote do
      elem(unquote(conn), 7)
    end
  end

  defmacrop _cookies(conn, value) do
    quote do
      setelem(unquote(conn), 7, unquote(value))
    end
  end

  defmacrop _resp_headers(conn) do
    quote do
      elem(unquote(conn), 8)
    end
  end

  defmacrop _resp_headers(conn, value) do
    quote do
      setelem(unquote(conn), 8, unquote(value))
    end
  end

  defmacrop _resp_cookies(conn) do
    quote do
      elem(unquote(conn), 9)
    end
  end

  defmacrop _resp_cookies(conn, value) do
    quote do
      setelem(unquote(conn), 9, unquote(value))
    end
  end

  defmacrop _assigns(conn) do
    quote do
      elem(unquote(conn), 10)
    end
  end

  defmacrop _assigns(conn, value) do
    quote do
      setelem(unquote(conn), 10, unquote(value))
    end
  end

  defmacrop _status(conn) do
    quote do
      elem(unquote(conn), 11)
    end
  end

  defmacrop _status(conn, value) do
    quote do
      setelem(unquote(conn), 11, unquote(value))
    end
  end

  defmacrop _resp_body(conn) do
    quote do
      elem(unquote(conn), 12)
    end
  end

  defmacrop _resp_body(conn, value) do
    quote do
      setelem(unquote(conn), 12, unquote(value))
    end
  end

  defmacrop _state(conn) do
    quote do
      elem(unquote(conn), 13)
    end
  end

  defmacrop _state(conn, value) do
    quote do
      setelem(unquote(conn), 13, unquote(value))
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
  Returns the params retrieved from the query string and the request
  body as a `Binary.Dict`. The parameters need to be explicitly
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
  Returns the response status if one was set.
  """
  def status(conn) do
    _status(conn)
  end

  @doc """
  Returns the response body if one was set.
  """
  def resp_body(conn) do
    _resp_body(conn)
  end

  @doc """
  Sets a response to the given status and body. The
  response will only be sent when `send` is called.

  After calling this function, the state changes to `:configured`,
  both `status` and `resp_body` are set.
  """
  def resp(status, body, conn) when is_integer(status) do
    _state(_status(_resp_body(conn, body), status), :configured)
  end

  @doc """
  A shortcut to `conn.send(conn.status, conn.body)`.
  """
  def send(conn) do
    send(_status(conn), _resp_body(conn), conn)
  end

  @doc """
  Sends to the client the given status and body.
  An updated connection is returned with `:sent` state,
  the given status and response body set to nil.
  """
  def send(status, body, conn) when is_integer(status) do
    req = Enum.reduce _resp_cookies(conn), _request(conn), write_cookie(&1, &2)
    { :ok, req } = R.reply(status, Dict.to_list(_resp_headers(conn)), body, req)
    _state(_status(_resp_body(_request(conn, req), nil), status), :sent)
  end

  @doc """
  Returns true if a reply was already sent back to the client.
  """
  def state(conn) do
    _state(conn)
  end

  ## Headers

  @doc """
  Returns the request headers as `Binary.Dict`. Note that duplicated
  entries are removed. The headers need to be explicitly fetched with
  `conn.fetch(:headers)` before using this function.
  """
  def req_headers(conn) do
    _req_headers(conn) || raise Dynamo.Connection.UnfetchedError, aspect: :req_headers
  end

  @doc """
  Returns the response headers as `Binary.Dict`.
  """
  def resp_headers(conn) do
    _resp_headers(conn)
  end

  @doc """
  Sets the response header, overriding any previous value.
  Both `key` and `value` are converted to binary.
  """
  def set_resp_header(key, value, conn) do
    _resp_headers(conn, Dict.put(_resp_headers(conn), key, to_binary(value)))
  end

  @doc """
  Deletes the response header.
  """
  def delete_resp_header(key, conn) do
    _resp_headers(conn, Dict.delete(_resp_headers(conn), key))
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
  def resp_cookies(conn) do
    _resp_cookies(conn)
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

    resp_cookies = List.keydelete(_resp_cookies(conn), key, 1)
    _resp_cookies(conn, [{ key, value, opts }|resp_cookies])
  end

  @doc """
  Deletes a cookie. The same options given when setting the cookie
  must be given on delete to ensure the browser will pick them up.
  """
  def delete_cookie(key, opts // [], conn) do
    key  = to_binary(key)
    unix = { { 1970, 1, 1 }, { 12, 0, 0 } }
    opts = Keyword.merge(opts, max_age: 0, local_time: unix)

    if cookies = _cookies(conn) do
      conn = _cookies(conn, Dict.delete(cookies, key))
    end

    resp_cookies = List.keydelete(_resp_cookies(conn), key, 1)
    _resp_cookies(conn, [{ key, "", opts }|resp_cookies])
  end

  ## Assigns

  @doc """
  Returns a keywords list with assigns set so far.
  """
  def assigns(conn) do
    _assigns(conn)
  end

  @doc """
  Sets a new assign with the given key and value.
  """
  def assign(key, value, conn) do
    _assigns(conn, Keyword.put(_assigns(conn), key, value))
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

  @doc """
  Builds a new Dynamo.Cowboy.Request based on
  the original Cowboy request object.
  """
  def new(req) do
    { segments, req } = R.path(req)
    { __MODULE__, req, segments, [], nil, nil, nil, Binary.Dict.new, [], [], nil, nil, :blank }
  end

  @doc """
  Responsible for fetching and caching aspects of the response.
  The "fetchable" aspects are: headers, params, cookies and session.
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

  def fetch(:headers, conn) do
    { headers, req } = R.headers _request(conn)
    _req_headers(_request(conn, req), Binary.Dict.new(headers))
  end

  @doc """
  Mounts the request by setting the new path information to the given
  *segments*. Both script_info/1 and path_segments/1 are updated.
  The segments given must be a suffix of the current path segments.
  """
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