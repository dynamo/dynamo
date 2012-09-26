defmodule Dynamo.Cowboy.HTTP do
  @moduledoc """
  A wrapper around Cowboy request structure.

  Check `Dynamo.HTTP` for documentation on
  the majority of the functions.
  """

  @behaviour Dynamo.HTTP
  require :cowboy_req, as: R

  Record.defmacros __ENV__, :connection,
    [ :req, :path_info_segments, :script_name_segments, :req_headers,
      :params, :cookies, :resp_headers, :resp_cookies, :assigns, :status,
      :method, :original_method, # :res_charset, :res_type, :session, :req_body,
      :resp_body, :state ]

  use Dynamo.HTTP.Paths
  use Dynamo.HTTP.Cookies
  use Dynamo.HTTP.Request
  use Dynamo.HTTP.Response
  use Dynamo.HTTP.Assigns

  @doc false
  def new(req) do
    { verb, req }     = R.method(req)

    { binary, _ } = R.path req
    { segments, _, _} = :cowboy_dispatcher.split_path(binary, 
                         fn(bin) -> :cowboy_http.urldecode(bin, :crash) end)

    connection(
      req: req,
      original_method: verb,
      method: verb,
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
    { query_string, _ } = R.qs req
    query_string
  end

  def path_segments(connection(req: req)) do
    { binary, _ } = R.path req
    { segments, _, _} = :cowboy_dispatcher.split_path(binary, 
                         fn(bin) -> :cowboy_http.urldecode(bin, :crash) end)
    segments
  end

  def path(connection(req: req)) do
    { binary, _ } = R.path req
    binary
  end

  def version(connection(req: req)) do
    { version, _ } = R.version req
    version
  end

  ## Response API

  def send(_status, body,
      connection(original_method: "HEAD")) when body != "" do
    raise Dynamo.HTTP.InvalidSendOnHeadError
  end

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

  def sendfile(path, connection(req: req) = conn) do
    File.Stat[type: :regular, size: size] = File.stat!(path)
    { :ok, :ranch_tcp, socket } = R.transport(req)
    send(200, { size, fn -> :file.sendfile(path, socket) end }, conn)
  end

  ## Cookies

  def req_cookies(connection(req: req)) do
    { cookies, _ } = R.cookies req
    Binary.Dict.new(cookies)
  end

  ## Misc

  def fetch(:params, connection(req: req, params: nil) = conn) do
    { query_string, req } = R.qs req
    params = Dynamo.HTTP.QueryParser.parse(query_string)
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
    R.set_resp_cookie(key, value, opts, req)
  end
end