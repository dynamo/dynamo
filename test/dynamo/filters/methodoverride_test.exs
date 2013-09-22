defmodule Dynamo.Filters.MethodOverrideTest do
  defmodule MethodOverrideApp do
    use Dynamo.Router

    filter Dynamo.Filters.MethodOverride

    delete "/hello" do
      conn.send(200, "DELETE")
    end

    put "/hello" do
      conn.send(200, "PUT")
    end

    patch "/hello" do
      conn.send(200, "PATCH")
    end
  end

  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  @endpoint MethodOverrideApp

  test "converts POST to DELETE when _method=DELETE param is specified" do
    conn = process @endpoint, :POST, "/hello?_method=DELETE"
    assert conn.status == 200
    assert conn.sent_body == "DELETE"
  end

  test "converts POST to PUT when _method=PUT param is specified" do
    conn = process @endpoint, :POST, "/hello?_method=PUT"
    assert conn.status == 200
    assert conn.sent_body == "PUT"
  end

  test "converts POST to PATCH when _method=PATCH param is specified" do
    conn = process @endpoint, :POST, "/hello?_method=PATCH"
    assert conn.status == 200
    assert conn.sent_body == "PATCH"
  end
end
