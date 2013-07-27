defmodule Dynamo.Filters.HeadTest do
  defmodule HeadApp do
    use Dynamo.Router

    filter Dynamo.Filters.Head

    get "/hello" do
      conn.send(200, "WILL BE IGNORED")
    end

    post "/hello" do
      conn.status(201).resp_body(conn.method)
    end
  end

  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  @endpoint HeadApp

  test "converts head to get and ignores the body" do
    conn = process @endpoint, :HEAD, "/hello"
    assert conn.status == 200
    assert conn.sent_body == ""
  end

  test "does not touch non head requests" do
    conn = process @endpoint, :POST, "/hello"
    assert conn.status == 201
    assert conn.resp_body == "POST"
  end
end
