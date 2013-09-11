defmodule Dynamo.Connection.Test do
  @moduledoc """
  Implementation of the `Dynamo.Connection` behaviour used for testing.
  Check `Dynamo.Connection` for documentation of all available callbacks.

  Note that, besides the functions required by `Dynamo.Connection`,
  this connection also implements a couple extra functions, like
  `sent_body/1`, to retrieve the sent body, and `fetched/1`, to
  retrieve fetched aspects, which are useful for testing.

  Although a new connection can be created via:

      Dynamo.Connection.Test.new(verb, path, body, peer)

  In practice, a developer should simply use `Dynamo.HTTP.Case`,
  which provides some wrappers around testing:

      conn(verb, path, body)

  Check `Dynamo.HTTP.Case` for more information on testing.
  """

  use Dynamo.Connection.Behaviour,
    [ :query_string, :raw_req_headers, :raw_req_body, :raw_req_cookies, :fetched,
      :path, :path_segments, :sent_body, :original_method, :scheme, :port, :peer ]

  @doc """
  Initializes a connection to be used in tests.
  """
  def new(method, path, body // "") do
    connection(
      main: Dynamo.under_test,
      raw_req_cookies: Binary.Dict.new(),
      raw_req_headers: Binary.Dict.new([{ "host", "127.0.0.1" }]),
      scheme: :http,
      port: 80
    ).recycle.req(method, path, body)
  end

  ## Request API

  @doc false
  def version(_conn) do
    { 1, 1 }
  end

  @doc false
  def original_method(connection(original_method: method)) do
    method
  end

  @doc false
  def query_string(connection(query_string: query_string)) do
    query_string
  end

  @doc false
  def path_segments(connection(path_segments: path_segments)) do
    path_segments
  end

  @doc false
  def path(connection(path: path)) do
    path
  end

  @doc false
  def scheme(connection(scheme: scheme)) do
    scheme
  end

  @doc false
  def port(connection(port: port)) do
    port
  end

  @doc false
  def peer(connection(peer: peer)) do
    peer
  end

  def peer(peer, connection = conn) do
    connection(conn, peer: peer)
  end

  @doc false
  def host(connection(raw_req_headers: headers)) do
    hd(:binary.split(headers["host"], ":"))
  end

  @doc false
  def host_url(connection(scheme: scheme, raw_req_headers: headers)) do
    "#{scheme}://#{headers["host"]}"
  end

  ## Response API

  @send_flag { :dynamo_req, :resp_sent }

  def already_sent?(_conn) do
    receive do
      @send_flag ->
        self <- @send_flag
        true
    after
      0 ->
        false
    end
  end

  @doc false
  def send(status, body, connection(state: state) = conn) when is_integer(status)
      and state in [:unset, :set] and is_binary(body) do
    self() <- @send_flag

    conn = run_before_send(connection(conn, state: :set, status: status, resp_body: body))
    connection(resp_body: body) = conn

    connection(conn,
      state: :sent,
      sent_body: check_sent_body(conn, body),
      resp_body: nil
    )
  end

  @doc false
  def send_chunked(status, connection(state: state) = conn) when is_integer(status)
      and state in [:unset, :set] do
    self() <- @send_flag
    conn = run_before_send(connection(conn, state: :chunked, status: status))

    connection(conn,
      sent_body: "",
      resp_body: nil
    )
  end

  @doc false
  def chunk(body, connection(state: state, sent_body: sent) = conn) when state == :chunked do
    { :ok, connection(conn, sent_body: check_sent_body(conn, sent <> body)) }
  end

  defp check_sent_body(connection(original_method: "HEAD"), _body), do: ""
  defp check_sent_body(_conn, body),                                do: body

  @doc false
  def sendfile(status, path, conn) when is_integer(status) and is_binary(path) do
    self() <- @send_flag
    conn = run_before_send(connection(conn, state: :sendfile, status: status))

    connection(conn,
      state: :sent,
      sent_body: check_sent_body(conn, File.read!(path)),
      resp_body: nil
    )
  end

  @doc false
  def sent_body(connection(sent_body: sent_body)) do
    sent_body
  end

  ## Misc

  @doc false
  def fetch(list, conn) when is_list(list) do
    Enum.reduce list, conn, fn(item, acc) -> acc.fetch(item) end
  end

  def fetch(:headers, connection(raw_req_headers: raw_req_headers, req_headers: nil, fetched: fetched) = conn) do
    connection(conn,
      fetched: [:headers|fetched],
      req_headers: raw_req_headers)
  end

  def fetch(:params, connection(query_string: query_string, params: nil, route_params: route_params, fetched: fetched) = conn) do
    params = Dynamo.Connection.QueryParser.parse(query_string)
    params = Dict.merge(params, route_params)
    connection(conn, params: params, fetched: [:params|fetched])
  end

  def fetch(:cookies, connection(raw_req_cookies: raw_req_cookies, req_cookies: nil, fetched: fetched) = conn) do
    connection(conn, req_cookies: raw_req_cookies, fetched: [:cookies|fetched])
  end

  def fetch(:body, connection(raw_req_body: req_body, req_body: nil, fetched: fetched) = conn) do
    connection(conn, req_body: req_body, fetched: [:body|fetched])
  end

  def fetch(aspect, conn) when aspect in [:params, :cookies, :body, :headers] do
    conn
  end

  def fetch(aspect, connection(fetchable: fetchable, fetched: fetched) = conn) when is_atom(aspect) do
    case Keyword.get(fetchable, aspect) do
      nil -> raise Dynamo.Connection.UnknownFetchError, aspect: aspect
      fun -> connection(fun.(conn), fetched: [aspect|fetched])
    end
  end

  ## Test only API

  @doc """
  Sets the main module under test.
  """
  def main(main, conn) do
    connection(conn, main: main)
  end

  @doc """
  Prepares the connection to do a new request on the
  given `path` with the given `method` and `body`.

  This can be considered the counter-part of recycle
  (which is used to clean up the response).
  """
  def req(method, path, body // "", conn) do
    uri      = URI.parse(path)
    segments = Dynamo.Router.Utils.split(uri.path)
    method   = Dynamo.Router.Utils.normalize_verb(method)

    # Clear up any existing send flag
    flush_send

    conn = connection(conn,
      method: method,
      original_method: method,
      params: nil,
      path: uri.path,
      path_info_segments: segments,
      path_segments: segments,
      query_string: uri.query || "",
      raw_req_body: body,
      req_body: nil,
      route_params: [],
      script_name_segments: [])

    if uri.authority do
      conn = conn.put_req_header "host", uri.authority
    end

    if uri.scheme do
      conn = connection(conn, scheme: uri.scheme)
    end

    if uri.port do
      conn = connection(conn, port: uri.port)
    end

    conn
  end

  defp flush_send do
    receive do
      @send_flag -> flush_send
    after
      0 -> :ok
    end
  end

  @doc """
  Recycles the connection to it can be used in a subsequent requests.

  Reclycing the connection resets all assigns, privates and other
  response information but keeps mostly of request information intact.

  Particularly, cookies sent in the response are moved to the request,
  so they can be used in upcoming requests.
  """
  def recycle(connection(resp_cookies: resp_cookies) = conn) do
    conn = connection(conn,
      assigns: [],
      before_send: Dynamo.Connection.default_before_send,
      fetchable: [],
      fetched: [],
      private: [dynamo_handle_exceptions: false],
      req_cookies: nil,
      req_headers: nil,
      resp_body: "",
      resp_charset: "utf-8",
      resp_cookies: [],
      resp_content_type: nil,
      resp_headers: Binary.Dict.new,
      sent_body: nil,
      state: :unset,
      status: nil
    )

    Enum.reduce resp_cookies, conn, fn({ key, value, _opts }, acc) ->
      acc.put_req_cookie(key, value)
    end
  end

  @doc """
  Returns all fetched aspects during a request.
  """
  def fetched(connection(fetched: fetched)) do
    fetched
  end

  @doc """
  Sets the cookies to be read by the request.
  """
  def put_req_cookie(key, value, connection(raw_req_cookies: cookies) = conn) do
    connection(conn, raw_req_cookies: Dict.put(cookies, key, value))
  end

  @doc """
  Sets a request header, overriding any previous value.
  Both `key` and `value` are converted to binary.
  """
  def put_req_header(key, value, connection(raw_req_headers: raw_req_headers) = conn) do
    connection(conn, raw_req_headers: Dict.put(raw_req_headers, String.downcase(key), to_string(value)))
  end

  @doc """
  Deletes a request header.
  """
  def delete_req_header(key, connection(raw_req_headers: raw_req_headers) = conn) do
    connection(conn, raw_req_headers: Dict.delete(raw_req_headers, String.downcase(key)))
  end
end

defimpl Inspect, for: Dynamo.Connection.Test do
  def inspect(conn, _) do
    "#Dynamo.Connection.Test<#{conn.method} #{conn.path}>"
  end
end
