defmodule Dynamo.Cowboy.Connection do
  require :cowboy_http_req, as: R

  Record.defmacros __ENV__, :connection,
    [ :req, :path_info_segments, :script_name_segments, :req_headers,
      :params, :cookies, :resp_headers, :resp_cookies, :assigns, :status,
      :resp_body, :state ]

  ## Request API

  @doc """
  Returns the query string as a binary.
  """
  def query_string(connection(req: req)) do
    { query_string, _ } = R.raw_qs req
    query_string
  end

  @doc """
  Returns the params retrieved from the query string and the request
  body as a `Binary.Dict`. The parameters need to be explicitly
  fetched with `conn.fetch(:params)` before using this function.
  """
  def params(connection(params: nil)) do
    raise Dynamo.Connection.UnfetchedError, aspect: :params
  end

  def params(connection(params: params)) do
    params
  end

  @doc """
  Return the path as a list of binaries split on "/".
  If the request was forwarded request, `path_info_segments` returns
  only the segments related to the current forwarded endpoint.
  """
  def path_info_segments(connection(path_info_segments: segments)) do
    segments
  end

  @doc """
  Returns the request path relative to the forwarding endpoint
  as a binary.
  """
  def path_info(connection(path_info_segments: segments)) do
    to_path segments
  end

  @doc """
  Returns the full path segments, as received by the web server.
  """
  def path_segments(connection(req: req)) do
    { segments, _ } = R.path req
    segments
  end

  @doc """
  Returns the full path as a binary, as received by the web server.
  """
  def path(connection(req: req)) do
    { binary, _ } = R.raw_path req
    binary
  end

  @doc """
  As in CGI environment, returns the current forwarded endpoint as segments.
  """
  def script_name_segments(connection(script_name_segments: segments)) do
    segments
  end

  @doc """
  As in CGI environment, returns the current forwarded endpoint as binary.
  """
  def script_name(connection(script_name_segments: segments)) do
    to_path segments
  end

  @doc """
  Returns the HTTP method as an atom.

  ## Examples

      request.method #=> :GET

  """
  def method(connection(req: req)) do
    { verb, _ } = R.method req
    verb
  end

  @doc """
  Returns the HTTP version.
  """
  def version(connection(req: req)) do
    { version, _ } = R.version req
    version
  end

  ## Response API

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
  Sends to the client the given status and body.
  An updated connection is returned with `:sent` state,
  the given status and response body set to nil.
  """
  def send(status, body,
      connection(req: req, resp_headers: headers, resp_cookies: cookies) = conn) when is_integer(status) do
    req = Enum.reduce cookies, req, write_cookie(&1, &2)
    { :ok, req } = R.reply(status, Dict.to_list(headers), body, req)

    connection(conn,
      req: req,
      resp_body: nil,
      status: status,
      state: :sent
    )
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

  ## Headers

  @doc """
  Returns the request headers as `Binary.Dict`. Note that duplicated
  entries are removed. The headers need to be explicitly fetched with
  `conn.fetch(:headers)` before using this function.
  """
  def req_headers(connection(req_headers: nil)) do
    raise Dynamo.Connection.UnfetchedError, aspect: :req_headers
  end

  def req_headers(connection(req_headers: req_headers)) do
    req_headers
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

  ## Cookies

  @doc """
  Returns the cookies sent in the request as a `Binary.Dict`.
  Cookies need to be explicitly fetched with `conn.fetch(:cookies)`
  before using this function.
  """
  def req_cookies(connection(req: req)) do
    { cookies, _ } = R.cookies req
    Binary.Dict.new(cookies)
  end

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

  ## Assigns

  @doc """
  Returns a keywords list with assigns set so far.
  """
  def assigns(connection(assigns: assigns)) do
    assigns
  end

  @doc """
  Sets a new assign with the given key and value.
  """
  def assign(key, value, connection(assigns: assigns) = conn) do
    connection(conn, assigns: Keyword.put(assigns, key, value))
  end

  ## Misc

  @doc """
  Returns the underlying cowboy request. This is used
  internally by Dynamo but may also be used by other
  developers (with caution).
  """
  def cowboy_request(connection(req: req)) do
    req
  end

  @doc """
  Builds a new Dynamo.Cowboy.Request based on
  the original Cowboy request object.
  """
  def new(req) do
    { segments, req } = R.path(req)

    connection(
      req: req,
      path_info_segments: segments,
      script_name_segments: [],
      resp_headers: Binary.Dict.new,
      resp_cookies: [],
      assigns: [],
      state: :unset
    )
  end

  @doc """
  Responsible for fetching and caching aspects of the response.
  The "fetchable" aspects are: headers, params, cookies and session.
  """
  def fetch(:params, connection(req: req) = conn) do
    { query_string, req } = R.raw_qs req
    params = Dynamo.Connection.QueryParser.parse(query_string)
    { params, req } = Dynamo.Cowboy.BodyParser.parse(params, req)
    connection(conn, req: req, params: params)
  end

  def fetch(:cookies, connection(req: req) = conn) do
    { cookies, req } = R.cookies req
    connection(conn, req: req, cookies: Binary.Dict.new(cookies))
  end

  def fetch(:headers, connection(req: req) = conn) do
    { headers, req } = R.headers req
    connection(conn, req: req, req_headers: Binary.Dict.new(headers))
  end

  @doc """
  Mounts the request by setting the new path information to the given
  *segments*. Both script_name/1 and path_segments/1 are updated.
  The segments given must be a suffix of the current path segments.
  """
  def forward_to([], _target, conn) do
    conn
  end

  def forward_to(segments, _target,
      connection(path_info_segments: path, script_name_segments: script) = conn) do
    { prefix, ^segments } = Enum.split path, length(path) - length(segments)

    connection(conn,
      path_info_segments: segments,
      script_name_segments: script ++ prefix
    )
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