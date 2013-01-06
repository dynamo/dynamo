Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Filters.ExceptionsTest do
  defmodule ExceptionsApp do
    use Dynamo.Router

    filter Dynamo.Filters.Exceptions.new Dynamo.Filters.Exceptions.Public

    get "/halt" do
      halt! conn.resp(200, "HALT")
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
end
