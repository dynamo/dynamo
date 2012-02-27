defmodule Dynamo::Cowboy::Handler do
  @moduledoc """
  This is the default Cowboy handler that is able
  to respond to http and websockets requests.
  """

  @behaviour :cowboy_http_handler

  def init({ :tcp, :http }, req, app) do
    { :ok, req, app }
  end

  def handle(req, app) do
    _ = app.service(Dynamo::Cowboy::Request.new(req), {})
    { :ok, req, app }
  end

  def terminate(_req, _app) do
    :ok
  end
end