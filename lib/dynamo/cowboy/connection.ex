defmodule Dynamo.Cowboy.Connection do
  @moduledoc false

  use Dynamo.Connection.Behaviour, [:req, :scheme]
  require :cowboy_req, as: R

  @doc """
  Returns the underlying cowboy request. This is used
  internally by Dynamo but may also be used by other
  developers (with caution).
  """
  def cowboy_request(connection(req: req)) do
    req
  end

  @doc """
  Sets the underlying cowboy request.
  """
  def cowboy_request(req, conn) do
    connection(conn, req: req)
  end

  @doc false
  def new(main, req, scheme) do
    { verb, req } = R.method req
    { path, _ }   = R.path req

    segments = split_path(path)

    connection(
      main: main,
      before_send: Dynamo.Connection.default_before_send,
      method: verb,
      path_info_segments: segments,
      req: req,
      scheme: scheme
    )
  end

  ## Request API

  @doc false
  def original_method(connection(req: req)) do
    { method, _ } = R.method req
    method
  end

  @doc false
  def query_string(connection(req: req)) do
    { query_string, _ } = R.qs req
    query_string
  end

  @doc false
  def path_segments(connection(req: req)) do
    { path, _ } = R.path req
    split_path path
  end

  @doc false
  def path(connection(req: req)) do
    { binary, _ } = R.path req
    binary
  end

  @doc false
  def version(connection(req: req)) do
    { version, _ } = R.version req
    version
  end

  @doc false
  def host(connection(req: req)) do
    { host, _ } = R.host req
    host
  end

  @doc false
  def port(connection(req: req)) do
    { port, _ } = R.port req
    port
  end

  @doc false
  def host_url(connection(req: req)) do
    { host_url, _ } = R.host_url req
    host_url
  end

  @doc false
  def scheme(connection(scheme: scheme)) do
    scheme
  end

  ## Response API

  def already_sent?(_conn) do
    receive do
      { :cowboy_req, :resp_sent } = flag ->
        self <- flag
        true
    after
      0 ->
        false
    end
  end

  @doc false
  def send(status, body, connection(state: state) = conn) when is_integer(status)
      and state in [:unset, :set] and is_binary(body) do
    conn = run_before_send(connection(conn, status: status, resp_body: body, state: :set))
    connection(req: req, status: status, resp_body: body,
               resp_headers: headers, resp_cookies: cookies) = conn

    merged_resp_headers = Dynamo.Connection.Utils.merge_resp_headers(headers, cookies)
    { :ok, req } = R.reply(status, merged_resp_headers, body, req)

    connection(conn,
      req: req,
      resp_body: nil,
      state: :sent
    )
  end

  @doc false
  def send_chunked(status, connection(state: state) = conn)
      when is_integer(status) and state in [:unset, :set] do
    conn = run_before_send(connection(conn, status: status, state: :chunked))
    connection(status: status, req: req,
               resp_headers: headers, resp_cookies: cookies) = conn
    merged_resp_headers = Dynamo.Connection.Utils.merge_resp_headers(headers, cookies)

    { :ok, req } = R.chunked_reply(status, merged_resp_headers, req)

    connection(conn,
      req: req,
      resp_body: nil)
  end

  @doc false
  def chunk(body, connection(state: state, req: req) = conn) when state == :chunked do
    case R.chunk(body, req) do
      :ok   -> { :ok, conn }
      other -> other
    end
  end

  @doc false
  def sendfile(status, path, conn) do
    File.Stat[type: :regular, size: size] = File.stat!(path)
    body_fun = fn (socket, _transport) ->
                    {:ok, sent} = :file.sendfile(path, socket)
                    {:sent, sent}
               end

    conn = run_before_send(connection(conn, status: status, state: :sendfile))
    connection(req: req, status: status,
               resp_headers: headers, resp_cookies: cookies) = conn

    merged_resp_headers = Dynamo.Connection.Utils.merge_resp_headers(headers, cookies)
    req = R.set_resp_body_fun(size, body_fun, req)
    { :ok, req } = R.reply(status, merged_resp_headers, req)

    connection(conn,
      req: req,
      resp_body: nil,
      state: :sent
    )
  end

  ## Misc

  @doc false
  def fetch(list, conn) when is_list(list) do
    Enum.reduce list, conn, fn(item, acc) -> acc.fetch(item) end
  end

  def fetch(:body, connection(req: req, req_body: nil) = conn) do
    { :ok, body, req } = R.body req
    connection(conn, req: req, req_body: body)
  end

  def fetch(:params, connection(req: req, params: nil, route_params: route_params) = conn) do
    { query_string, req } = R.qs req
    params = Dynamo.Connection.QueryParser.parse(query_string)
    { params, req } = Dynamo.Cowboy.BodyParser.parse(params, req)
    connection(conn, req: req, params: merge_route_params(params, route_params))
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
      nil -> raise Dynamo.Connection.UnknownFetchError, aspect: aspect
      fun -> fun.(conn)
    end
  end

  ## Helpers

  defp merge_route_params(params, []), do: params
  defp merge_route_params(params, route_params), do: Dict.merge(params, route_params)

  defp split_path(path) do
    case :binary.split(path, "/", [:global, :trim]) do
      [""|segments] -> segments
      segments -> segments
    end
  end

end

defimpl Binary.Inspect, for: Dynamo.Cowboy.Connection do
  def inspect(conn, _) do
    "#Dynamo.Connection<#{conn.method} #{conn.path} (cowboy)>"
  end
end