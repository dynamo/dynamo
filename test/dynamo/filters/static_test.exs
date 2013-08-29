defmodule Dynamo.Filters.StaticTest do
  defmodule StaticApp do
    use Dynamo
    use Dynamo.Router

    config :dynamo, root: Path.expand("../../..", __FILE__)

    filter Dynamo.Filters.Static.new("/public", "")

    get "/public/fixtures/fallback" do
      conn.send(200, "Fallback")
    end

    match _ do
      conn.send(404, "File not served")
    end
  end

  use ExUnit.Case
  use Dynamo.HTTP.Case

  setup_all do
    Dynamo.under_test(StaticApp)
    :ok
  end

  teardown_all do
    Dynamo.under_test(nil)
    :ok
  end

  @endpoint StaticApp

  test "serves the file" do
    conn = get("/public/fixtures/static/file.txt")
    assert conn.status == 200
    assert conn.sent_body == "HELLO"
    assert conn.resp_headers["content-type"]  == "text/plain"
    assert conn.resp_headers["cache-control"] == "public, max-age=31536000"
  end

  test "serves the file with a urlencoded filename" do
    conn = get("/public/fixtures/static/file+with%20spaces.txt")
    assert conn.status == 200
    assert conn.sent_body == "HELLO"
    assert conn.resp_headers["content-type"]  == "text/plain"
    assert conn.resp_headers["cache-control"] == "public, max-age=31536000"
  end

  test "hits the fallback" do
    conn = get("/public/fixtures/fallback")
    assert conn.status == 200
    assert conn.sent_body == "Fallback"
  end

  test "returns 404 for non existing files" do
    conn = get("/public/fixtures/unknown.txt")
    assert conn.status    == 404
    assert conn.sent_body == "File not served"
  end

  test "returns 404 for directories" do
    conn = get("/public/fixtures")
    assert conn.status    == 404
    assert conn.sent_body == "File not served"
  end

  test "returns 400 for unsecure paths" do
    conn = get("/public/fixtures/../fixtures/static/file.txt")
    assert conn.status    == 400
    assert conn.sent_body == ""

    conn = get("/public/c:\\foo.txt")
    assert conn.status    == 400
    assert conn.sent_body == ""
  end
end
