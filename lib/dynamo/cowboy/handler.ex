defmodule Dynamo.Cowboy.Handler do
  @moduledoc false
  require Record
  
  @behaviour :cowboy_http_handler

  require :cowboy_req

  # HTTP

  defp scheme(:tcp), do: :http
  defp scheme(:ssl), do: :https

  def init({ transport, :http }, req, main) when transport in [:tcp, :ssl] do
    conn = main.service(Dynamo.Cowboy.Connection.new(main, req, scheme(transport)))

    if Record.record?(conn, Dynamo.Cowboy.Connection) do
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
end
