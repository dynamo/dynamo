defmodule Dynamo.Filters.Exceptions do
  @moduledoc """
  A filter that is responsible to catch, log and handle exceptions.
  """

  def new(handler) do
    { __MODULE__, handler }
  end

  def service(conn, fun, { __MODULE__, handler }) do
    try do
      fun.(conn)
    rescue
      e ->
        handle(conn, handler, :exception, e, System.stacktrace)
    catch
      { :halt!, conn } ->
        conn
      kind, e ->
        handle(conn, handler, kind, e, System.stacktrace)
    end
  end

  defp handle(conn, handler, kind, value, stacktrace) do
    status = Dynamo.Exception.status(value)
    handler.service conn.assign(:exception, { status, kind, value, stacktrace })
  end
end

