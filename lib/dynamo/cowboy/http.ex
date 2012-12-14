defmodule Dynamo.Cowboy.HTTP do
  @moduledoc false

  use Dynamo.HTTP.Behaviour, [:req]
  require :cowboy_req, as: R

  @doc false
  def new(app, req) do
    { verb, req } = R.method req
    { path, _ }   = R.path req

    segments = split_path(path)

    connection(
      app: app,
      req: req,
      method: verb,
      path_info_segments: segments,
      before_send: Dynamo.HTTP.default_before_send,
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

  def original_method(connection(req: req)) do
    { method, _ } = R.method req
    method
  end

  def query_string(connection(req: req)) do
    { query_string, _ } = R.qs req
    query_string
  end

  def path_segments(connection(req: req)) do
    { path, _ } = R.path req
    split_path path
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

  def send(status, body, connection(state: state) = conn) when is_integer(status)
      and state in [:unset, :set] and (is_binary(body) or is_tuple(body)) do
    conn = run_before_send(conn)
    connection(resp_headers: headers, resp_cookies: cookies, req: req) = conn
    { :ok, req } = R.reply(status, get_resp_headers(headers, cookies), body, req)

    connection(conn,
      req: req,
      resp_body: nil,
      status: status,
      state: :sent)
  end

  def send_chunked(status, connection(state: state) = conn) when is_integer(status)
      and state in [:unset, :set] do
    conn = run_before_send(conn)
    connection(resp_headers: headers, resp_cookies: cookies, req: req) = conn
    { :ok, req } = R.chunked_reply(status, get_resp_headers(headers, cookies), req)

    connection(conn,
      req: req,
      resp_body: nil,
      status: status,
      state: :chunked)
  end

  def chunk(body, connection(state: state, req: req) = conn) when state == :chunked do
    R.chunk(body, req)
    conn
  end

  def sendfile(path, connection(req: req) = conn) do
    File.Stat[type: :regular, size: size] = File.stat!(path)
    { :ok, :ranch_tcp, socket } = R.transport(req)
    send(200, { size, fn -> :file.sendfile(path, socket) end }, conn)
  end

  ## Misc

  def fetch(list, conn) when is_list(list) do
    Enum.reduce list, conn, fn(item, acc) -> acc.fetch(item) end
  end

  def fetch(:body, connection(req: req, req_body: nil) = conn) do
    { :ok, body, req } = R.body req
    connection(conn, req: req, req_body: body)
  end

  def fetch(:params, connection(req: req, params: nil) = conn) do
    { query_string, req } = R.qs req
    params = Dynamo.HTTP.QueryParser.parse(query_string)
    { params, req } = Dynamo.Cowboy.BodyParser.parse(params, req)
    connection(conn, req: req, params: params)
  end

  def fetch(:cookies, connection(req: req, req_cookies: nil) = conn) do
    { cookies, req } = R.cookies req
    connection(conn, req: req, req_cookies: Binary.Dict.new(cookies))
  end

  def fetch(:headers, connection(req: req, req_headers: nil) = conn) do
    { headers, req } = R.headers req
    connection(conn, req: req, req_headers: Binary.Dict.new(headers))
  end

  # The given aspect was already loaded.
  def fetch(aspect, conn) when aspect in [:params, :cookies, :headers, :body] do
    conn
  end

  def fetch(aspect, connection(fetchable: fetchable) = conn) when is_atom(aspect) do
    case Keyword.get(fetchable, aspect) do
      nil -> raise Dynamo.HTTP.UnknownAspectError, aspect: aspect
      fun -> fun.(conn)
    end
  end

  ## Helpers

  defp split_path(path) do
    case :binary.split(path, "/", [:global, :trim]) do
      [""|segments] -> segments
      segments -> segments
    end
  end

  defp get_resp_headers(headers, resp_cookies) do
    Enum.reduce resp_cookies, Binary.Dict.to_list(headers), fn({ key, value, opts }, acc) ->
      [{ "set-cookie", Dynamo.HTTP.Utils.cookie_header(key, value, opts) }|acc]
    end
  end
end

defimpl Binary.Inspect, for: Dynamo.Cowboy.HTTP do
  def inspect(conn, _) do
    "Dynamo.HTTP[#{conn.method} #{conn.path} (cowboy)]"
  end
end