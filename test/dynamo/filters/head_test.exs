Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Filters.HeadTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  defmodule HeadApp do
    use Dynamo.Router

    filter Dynamo.Filters.Head

    get "/hello" do
      conn.resp(200, "WILL BE IGNORED")
    end

    post "/hello" do
      conn.resp(201, conn.method)
    end
  end

  @app HeadApp

  test "converts head to get and ignores the body" do
    conn = process @app, :HEAD, "/hello"
    assert conn.status == 200
    assert conn.resp_body == ""
  end

  test "does not touch non head requests" do
    conn = process @app, :POST, "/hello"
    assert conn.status == 201
    assert conn.resp_body == "POST"
  end
end
