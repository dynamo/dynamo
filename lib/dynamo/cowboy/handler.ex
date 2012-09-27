defmodule Dynamo.Cowboy.Handler do
  @moduledoc """
  This is the default Cowboy handler that is able
  to respond to http and websockets requests.
  """

  require :cowboy_req, as: R
  @behaviour :cowboy_http_handler

  def init({ :tcp, :http }, req, app) do
    { :ok, req, app }
  end

  def handle(req, app) do
    conn = app.service(Dynamo.Cowboy.HTTP.new(req))

    if is_record(conn, Dynamo.Cowboy.HTTP) do
      if conn.state == :set do
        conn = conn.send
      end
      { :ok, conn.cowboy_request, app }
    else
      raise "Expected service to return a Dynamo.Cowboy.HTTP, got #{inspect conn}"
    end
  end

  def terminate(_req, _app) do
    :ok
  end
end