Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Filters.ExceptionsTest do
  defexception Unauthorized, message: "Unauthorized"

  defimpl Dynamo.Exception, for: Unauthorized do
    def status(_exception) do
      401
    end
  end

  defmodule ExceptionsApp do
    use Dynamo
    use Dynamo.Router

    get "/halt" do
      halt! conn.resp(200, "HALT")
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
      raise Unauthorized
    end

    get "/no_halt" do
      conn.resp(200, "NO HALT")
    end

    finalize do
      conn.resp(200, "FINALIZE")
    end
  end

  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  @endpoint ExceptionsApp

  test "halts and catches the request" do
    conn = get("/halt")
    assert conn.status == 200
    assert conn.sent_body == "HALT"
  end

  test "no-op when halt is not invoked" do
    conn = get("/no_halt")
    assert conn.status == 200
    assert conn.sent_body == "FINALIZE"
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
    conn(:get, "/").put_private(:dynamo_handle_exceptions, true).app(ExceptionsApp)
  end
end
