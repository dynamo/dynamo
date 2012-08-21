Code.require_file "../../test_helper.exs", __FILE__

defmodule Dynamo.StaticTest do
  use ExUnit.Case, async: true
  import Dynamo.Router.TestHelpers

  defmodule StaticApp do
    use Dynamo.Router
    use Dynamo.App

    forward "/public", to: Dynamo.Static.new(root: File.expand_path("../..", __FILE__))

    def not_found(conn) do
      conn.send(404, "File not served")
    end
  end

  @app StaticApp

  test "serves the file" do
    conn = get("/public/fixtures/file.txt")
    assert conn.status == 200
    assert conn.resp_body == "HELLO"
    assert conn.resp_headers["Content-Type"] == "text/plain"
  end

  test "returns 404 for non existing files" do
    conn = get("/public/fixtures/unknown.txt")
    assert conn.status    == 404
    assert conn.resp_body == "File not served"
  end

  test "returns 404 for directories" do
    conn = get("/public/fixtures")
    assert conn.status    == 404
    assert conn.resp_body == "File not served"
  end

  test "returns 404 for unsecure paths" do
    conn = get("/public/fixtures/../fixtures/file.txt")
    assert conn.status    == 404
    assert conn.resp_body == "File not served"
  end
end
