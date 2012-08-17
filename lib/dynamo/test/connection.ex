defmodule Dynamo.Test.Connection do
  Record.defmacros __ENV__, :connection,
    [ :method, :path_segments, :path_info_segments, :script_name_segments,
      :req_headers, :req_body, :params, :cookies, :resp_headers, :resp_cookies,
      :assigns, :status, :resp_body, :state ]

  use Dynamo.Connection.Paths
  use Dynamo.Connection.Cookies
  use Dynamo.Connection.Request
  use Dynamo.Connection.Response

  def new() do
    connection(
      path_info_segments: [],
      script_name_segments: [],
      req_headers: Binary.Dict.new,
      resp_headers: Binary.Dict.new,
      resp_cookies: [],
      assigns: [],
      state: :unset
    )
  end

  ## Request API

  def path_segments(connection(path_segments: path_segments)) do
    path_segments
  end

  ## Response API

  def send(status, body, conn) do
    connection(conn,
      state: :sent,
      status: status,
      resp_body: body
    )
  end

  ## Test only API

  def req(method, path, conn) do
    segments = Dynamo.Router.Utils.split(path)
    connection(conn,
      path_segments: segments,
      path_info_segments: segments,
      script_name_segments: [],
      method: method)
  end
end