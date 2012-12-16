defmodule Dynamo.Cowboy.Handler do
  @moduledoc false

  @behaviour :cowboy_http_handler
  @behaviour :cowboy_websocket_handler

  require :cowboy_req, as: R

  # HTTP

  def init({ :tcp, :http }, req, app) do
    conn = app.service(Dynamo.Cowboy.HTTP.new(app, req))

    if is_record(conn, Dynamo.Cowboy.HTTP) do
      case conn.state do
        { :handler, :websocket, mod } ->
          { :upgrade, :protocol, :cowboy_websocket, conn.cowboy_request, { conn, mod } }
        :set ->
          { :ok, conn.send.cowboy_request, nil }
        other ->
          { :ok, conn.cowboy_request, nil }
      end
    else
      raise "Expected service to return a Dynamo.Cowboy.HTTP, got #{inspect conn}"
    end
  end

  def handle(req, nil) do
    { :ok, req, nil }
  end

  def terminate(_req, nil) do
    :ok
  end

  # Websockets

  def websocket_init(_any, req, { conn, mod }) do
    case mod.init(conn.cowboy_request(req)) do
      { :ok, conn } ->
        { :ok, conn.cowboy_request, { conn, mod } }
      { :ok, conn, timeout_or_hibernate } ->
        { :ok, conn.cowboy_request, { conn, mod }, timeout_or_hibernate }
      { :ok, conn, timeout, hibernate } ->
        { :ok, conn.cowboy_request, { conn, mod }, timeout, hibernate }
      { :shutdown, conn } ->
        { :shutdown, conn.cowboy_request }
    end
  end

  def websocket_handle(msg, req, state) do
    websocket_call(:handle_msg, msg, req, state)
  end

  def websocket_info(msg, req, state) do
    websocket_call(:handle_info, msg, req, state)
  end

  def websocket_terminate(reason, req, { conn, mod }) do
    mod.terminate(reason, conn.cowboy_request(req))
  end

  defp websocket_call(fun, msg, req, { conn, mod }) do
    case apply(mod, fun, [msg, conn.cowboy_request(req)]) do
      { :ok, conn } ->
        { :ok, conn.cowboy_request, { conn, mod } }
      { :ok, conn, hibernate } ->
        { :ok, conn.cowboy_request, { conn, mod }, hibernate }
      { :reply, payload, conn } ->
        { :reply, payload, conn.cowboy_request, { conn, mod } }
      { :reply, payload, conn, hibernate } ->
        { :reply, payload, conn.cowboy_request, { conn, mod }, hibernate }
      { :shutdown, conn } ->
        { :shutdown, conn.cowboy_request, { conn, mod } }
    end
  end
end
