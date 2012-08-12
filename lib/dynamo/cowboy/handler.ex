defmodule Dynamo.Cowboy.Handler do
  @moduledoc """
  This is the default Cowboy handler that is able
  to respond to http and websockets requests.
  """

  require :cowboy_http_req, as: R
  @behaviour :cowboy_http_handler

  def init({ :tcp, :http }, req, app) do
    { :ok, req, app }
  end

  def handle(req, app) do
    conn = app.service(Dynamo.Cowboy.Connection.new(req))

    if is_record(conn, Dynamo.Cowboy.Connection) do
      { :ok, conn.cowboy_request, app }
    else
      raise "Expected service to return a Dynamo.Cowboy.Response, got #{inspect conn}"
    end
  end

  def terminate(_req, _app) do
    :ok
  end
end