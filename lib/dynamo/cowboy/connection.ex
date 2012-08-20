defmodule Dynamo.Cowboy.Connection do
  @moduledoc """
  A wrapper around Cowboy request structure.

  Check `Dynamo.Connection` for documentation on
  the majority of the functions.
  """

  @behaviour Dynamo.Connection
  require :cowboy_http_req, as: R

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

  @doc false
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
  Returns the underlying cowboy request. This is used
  internally by Dynamo but may also be used by other
  developers (with caution).
  """
  def cowboy_request(connection(req: req)) do
    req
  end

  ## Request API

  def query_string(connection(req: req)) do
    { query_string, _ } = R.raw_qs req
    query_string
  end

  def path_segments(connection(req: req)) do
    { segments, _ } = R.path req
    segments
  end

  def path(connection(req: req)) do
    { binary, _ } = R.raw_path req
    binary
  end

  def method(connection(req: req)) do
    { verb, _ } = R.method req
    verb
  end

  def version(connection(req: req)) do
    { version, _ } = R.version req
    version
  end

  ## Response API

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

  def req_cookies(connection(req: req)) do
    { cookies, _ } = R.cookies req
    Binary.Dict.new(cookies)
  end

  ## Misc

  def fetch(:params, connection(req: req, params: nil) = conn) do
    { query_string, req } = R.raw_qs req
    params = Dynamo.Connection.QueryParser.parse(query_string)
    { params, req } = Dynamo.Cowboy.BodyParser.parse(params, req)
    connection(conn, req: req, params: params)
  end

  def fetch(:cookies, connection(req: req, cookies: nil) = conn) do
    { cookies, req } = R.cookies req
    connection(conn, req: req, cookies: Binary.Dict.new(cookies))
  end

  def fetch(:headers, connection(req: req, req_headers: nil) = conn) do
    { headers, req } = R.headers req
    connection(conn, req: req, req_headers: Binary.Dict.new(headers))
  end

  # The given aspect was already loaded.
  def fetch(aspect, conn) when aspect in [:params, :cookies, :headers] do
    conn
  end

  ## Helpers

  defp write_cookie({ key, value, opts }, req) do
    opts = Keyword.update(opts, :http_only, true, fn(x) -> x end)
    { :ok, req } = R.set_resp_cookie(key, value, opts, req)
    req
  end
end