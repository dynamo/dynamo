defmodule Dynamo.Filters.ExceptionsTest do
  defexception UnauthorizedError, message: "Unauthorized" do
    defimpl Dynamo.Exception do
      def status(_exception) do
        401
      end
    end
  end

  defmodule ExceptionsApp do
    use Dynamo.Router

    filter Dynamo.Filters.Exceptions.new(Dynamo.Filters.Exceptions.Public)

    def root do
      __ENV__.file
    end

    # Halt

    get "/halt" do
      halt! conn.resp(200, "HALT")
    end

    @finalize :finalizer
    get "/no_halt" do
      conn.resp(200, "NO HALT")
    end

    get "/unset_halt" do
      halt! conn
    end

    # Exception

    get "/unset" do
      conn
    end

    get "/set" do
      conn.resp(200, "SET")
    end

    get "/error" do
      _ = conn
      raise "Oops"
    end

    get "/already_sent" do
      conn.send(200, "SENT")
      raise "Oops"
    end

    get "/unauthorized" do
      _ = conn
      raise UnauthorizedError
    end

    defp finalizer(conn) do
      conn.resp(200, "FINALIZE")
    end
  end

  use ExUnit.Case
  use Dynamo.HTTP.Case

  @endpoint ExceptionsApp

  setup_all do
    :error_logger.tty(false)
    :ok
  end

  teardown_all do
    :error_logger.tty(true)
    :ok
  end

  # Halt

  test "sends the connection if it was only set" do
    conn = get("/no_halt")
    assert conn.status == 200
    assert conn.sent_body == "FINALIZE"
  end

  test "halts the request" do
    conn = get("/halt")
    assert conn.status == 200
    assert conn.sent_body == "HALT"
  end

  test "raises an exception when halting unset connection" do
    assert_raise Dynamo.Connection.NotSentError, fn ->
      get("/unset_halt")
    end
  end

  # Exceptions

  test "sends the response on set connections" do
    assert get("/set").sent_body == "SET"
  end

  test "raises an exception on unset connection" do
    assert_raise Dynamo.Connection.NotSentError, fn ->
      get("/unset")
    end
  end

  test "invokes handler on exceptions" do
    conn = get(exceptions_conn, "/error")
    assert conn.status == 500
    assert conn.sent_body == ""
  end

  test "requests status code on exceptions" do
    conn = get(exceptions_conn, "/unauthorized")
    assert conn.status == 401
    assert conn.sent_body == ""
  end

  defp exceptions_conn do
    conn(:get, "/").put_private(:dynamo_handle_exceptions, true).main(ExceptionsApp)
  end
end
