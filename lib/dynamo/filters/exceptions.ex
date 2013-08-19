defmodule Dynamo.Filters.Exceptions do
  @moduledoc """
  A filter that is responsible to catch, log and handle exceptions.
  """

  import Exception, only: [format_stacktrace_entry: 2]
  @key :dynamo_handle_exceptions

  def new(handler) do
    { __MODULE__, handler }
  end

  def service(conn, fun, { __MODULE__, handler }) do
    try do
      conn = fun.(conn)

      case conn.state do
        :unset -> raise Dynamo.Connection.NotSentError
        :set   -> conn.send
        _      -> conn
      end
    rescue
      e ->
        handle(conn, handler, :error, e, System.stacktrace)
    catch
      { :halt!, conn } ->
        case conn.state do
          :unset -> handle(conn, handler, :error, Dynamo.Connection.NotSentError[], System.stacktrace)
          :set   -> conn.send
          _      -> conn
        end
      kind, e ->
        handle(conn, handler, kind, e, System.stacktrace)
    end
  end

  defp handle(conn, handler, kind, value, stacktrace) do
    if conn.private[@key] == false do
      :erlang.raise(kind, value, stacktrace)
    end

    message = logger_conn(conn) <> logger_reason(kind, value) <> logger_stacktrace(stacktrace, conn.main.root)
    message = String.to_char_list!(message)
    :error_logger.error_msg(message)

    if conn.already_sent? do
      conn
    else
      { status, conn } = status(kind, value, conn)
      handler.service conn.assign(:exception, { status, kind, value, stacktrace })
    end
  end

  defp status(:error, value, conn) do
    case Dynamo.Exception.status(value) do
      { status, conn } when is_integer(status) and is_tuple(conn) -> { status, conn }
      status when is_integer(status) -> { status, conn }
    end
  end

  defp status(_, _, conn) do
    { 500, conn }
  end

  defp logger_conn(conn) do
    "      Conn: #{conn.method} #{conn.path}\n"
  end

  defp logger_reason(:error, value) do
    "    Reason: (#{inspect value.__record__(:name)}) #{value.message}\n"
  end

  defp logger_reason(kind, value) do
    "    Reason: (#{kind}) #{inspect(value)}\n"
  end

  defp logger_stacktrace(stacktrace, root) do
    Enum.reduce stacktrace, "Stacktrace:\n", fn(trace, acc) ->
      acc <> "  " <> format_stacktrace_entry(trace, root) <> "\n"
    end
  end
end

