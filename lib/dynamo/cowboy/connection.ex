defmodule Dynamo.Cowboy.Connection do
  require :cowboy_http_req, as: R

  @behaviour Dynamo.Connection

  Record.defmacros __ENV__, :connection,
    [ :req, :path_info_segments, :script_name_segments, :req_headers,
      :params, :cookies, :resp_headers, :resp_cookies, :assigns, :status,
      # :method, :res_charset, :res_type, :session, :req_body,
      :resp_body, :state ]

  use Dynamo.Connection.Paths
  use Dynamo.Connection.Cookies
  use Dynamo.Connection.Request
  use Dynamo.Connection.Response
  use Dynamo.Connection.Assigns

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

  ## Request API

  @doc """
  Returns the query string as a binary.
  """
  def query_string(connection(req: req)) do
    { query_string, _ } = R.raw_qs req
    query_string
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

  ## Helpers

  defp write_cookie({ key, value, opts }, req) do
    opts = Keyword.update(opts, :http_only, true, fn(x) -> x end)
    { :ok, req } = R.set_resp_cookie(key, value, opts, req)
    req
  end
end