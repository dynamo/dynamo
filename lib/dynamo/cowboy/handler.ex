defmodule Dynamo.Cowboy.Handler do
  @moduledoc false

  @behaviour :cowboy_http_handler
  @behaviour :cowboy_websocket_handler

  require :cowboy_req, as: R

  # HTTP

  defp scheme(:tcp), do: :http
  defp scheme(:ssl), do: :https

  def init({ transport, :http }, req, main) when transport in [:tcp, :ssl] do
    conn = main.service(Dynamo.Cowboy.Connection.new(main, req, scheme(transport)))

    if is_record(conn, Dynamo.Cowboy.Connection) do
      { :ok, conn.cowboy_request, nil }
    else
      raise "Expected service to return a Dynamo.Cowboy.Connection, got #{inspect conn}"
    end
  end

  def handle(req, nil) do
    { :ok, req, nil }
  end

  def terminate(_reason, _req, nil) do
    :ok
  end

  # Websockets

  def websocket_init(any, req, conn) do
    { mod, req } = req.meta(:websocket_handler)
    mod.websocket_init(any, req, conn)
  end

  def websocket_handle(msg, req, state) do
    { mod, req } = req.meta(:websocket_handler)
    mod.websocket_handle(msg, req, state)
  end

  def websocket_info(msg, req, state) do
    { mod, req } = req.meta(:websocket_handler)
    mod.websocket_info(msg, req, state)
  end

  def websocket_terminate(reason, req, state) do
    { mod, req } = req.meta(:websocket_handler)
    mod.websocket_terminate(reason, req, state)
  end
end
