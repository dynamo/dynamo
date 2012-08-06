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
    res = app.service(Dynamo.Cowboy.Request.new(req), Dynamo.Cowboy.Response.new(req))

    if is_record(res, Dynamo.Cowboy.Response) do
      { :ok, res.cowboy_request, app }
    else
      raise "Expected service to return a Dynamo.Cowboy.Response, got #{inspect res}"
    end
  end

  def terminate(_req, _app) do
    :ok
  end
end