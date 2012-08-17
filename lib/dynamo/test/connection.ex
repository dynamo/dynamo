defmodule Dynamo.Test.Connection do
  @behaviour Dynamo.Connection

  Record.defmacros __ENV__, :connection,
    [ :method, :path_segments, :path_info_segments, :script_name_segments,
      :query_string, :raw_req_headers, :req_headers, :req_body, :params,
      :resp_headers, :raw_cookies, :cookies, :resp_cookies, :assigns,
      :status, :resp_body, :state ]

  use Dynamo.Connection.Paths
  use Dynamo.Connection.Cookies
  use Dynamo.Connection.Request
  use Dynamo.Connection.Response

  def new() do
    connection(
      path_info_segments: [],
      script_name_segments: [],
      raw_req_headers: Binary.Dict.new([ { "Host", "127.0.0.1" } ]),
      resp_headers: Binary.Dict.new,
      resp_cookies: [],
      assigns: [],
      state: :unset
    )
  end

  ## Request API

  def version(_conn) do
    { 1, 1 }
  end

  def method(connection(method: method)) do
    method
  end

  def query_string(connection(query_string: query_string)) do
    query_string
  end

  def path_segments(connection(path_segments: path_segments)) do
    path_segments
  end

  def path(connection(path_segments: path_segments)) do
    to_path path_segments
  end

  def req_cookies(connection(raw_cookies: raw_cookies)) do
    raw_cookies
  end

  ## Response API

  def send(status, body, conn) do
    connection(conn,
      state: :sent,
      status: status,
      resp_body: body
    )
  end

  ## Misc

  def fetch(:headers, connection(raw_req_headers: raw_req_headers) = conn) do
    connection(conn,
      req_headers: raw_req_headers,
      raw_req_headers: Binary.Dict.new)
  end

  def fetch(:params, connection(query_string: query_string) = conn) do
    params = Dynamo.Connection.QueryParser.parse(query_string)
    connection(conn, params: params)
  end

  def fetch(:cookies, connection(raw_cookies: raw_cookies) = conn) do
    connection(conn, cookies: raw_cookies)
  end

  ## Test only API

  def req(method, path, conn) do
    uri = URI.parse(path)
    segments = Dynamo.Router.Utils.split(uri.path)

    conn = connection(conn,
      query_string: uri.query || "",
      path_segments: segments,
      path_info_segments: segments,
      script_name_segments: [],
      params: nil,
      req_headers: nil,
      method: method)

    if uri.authority do
      conn.set_req_header "Host", uri.authority
    else
      conn
    end
  end

  @doc """
  Sets the cookies to be read by the request.
  """
  def req_cookies(cookies, conn) do
    connection(conn, raw_cookies: Binary.Dict.new(cookies))
  end

  @doc """
  Sets a request header, overriding any previous value.
  Both `key` and `value` are converted to binary.
  """
  def set_req_header(key, value, connection(raw_req_headers: raw_req_headers) = conn) do
    connection(conn, raw_req_headers: Dict.put(raw_req_headers, key, to_binary(value)))
  end

  @doc """
  Deletes a request header.
  """
  def delete_req_header(key, connection(raw_req_headers: raw_req_headers) = conn) do
    connection(conn, raw_req_headers: Dict.delete(raw_req_headers, key))
  end
end