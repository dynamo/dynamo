defmodule Dynamo.Filters.Exceptions do
  @moduledoc """
  A filter that is responsible to catch, log and handle exceptions.
  """

  import Exception, only: [format_entry: 2]
  @key :dynamo_handle_exceptions

  def new(handler) do
    { __MODULE__, handler }
  end

  def service(conn, fun, { __MODULE__, handler }) do
    try do
      fun.(conn)
    rescue
      e ->
        handle(conn, handler, :error, e, System.stacktrace)
    catch
      { :halt!, conn } ->
        conn
      kind, e ->
        handle(conn, handler, kind, e, System.stacktrace)
    end
  end

  defp handle(conn, handler, kind, value, stacktrace) do
    if conn.private[@key] == false do
      if kind == :exception, do: kind = :error
      :erlang.raise(kind, value, stacktrace)
    end

    :error_logger.error_msg(
      logger_conn(conn) <>
      logger_reason(kind, value) <>
      logger_stacktrace(stacktrace, conn.app.root))

    if conn.already_sent? do
      conn
    else
      status = Dynamo.Exception.status(value)
      handler.service conn.assign(:exception, { status, kind, value, stacktrace })
    end
  end

  defp logger_conn(conn) do
    "      Conn: #{conn.method} #{conn.path}\n"
  end

  defp logger_reason(:error, value) do
    "    Reason: (#{inspect value.__record__(:name)}) #{value.message}\n"
  end

  defp logger_reason(kind, value) do
    "Reason: (#{kind}) #{inspect(value)}\n"
  end

  defp logger_stacktrace(stacktrace, root) do
    Enum.reduce stacktrace, "Stacktrace:\n", fn(trace, acc) ->
      acc <> "  " <> format_entry(trace, root) <> "\n"
    end
  end
end

