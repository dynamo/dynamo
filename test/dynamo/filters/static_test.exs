Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Filters.StaticTest do
  defmodule StaticApp do
    use Dynamo
    use Dynamo.Router

    filter Dynamo.Filters.Static.new("/public", Path.expand("../../..", __FILE__))

    get "/public/fixtures/fallback" do
      conn.send(200, "Fallback")
    end

    match _ do
      conn.send(404, "File not served")
    end
  end

  use ExUnit.Case
  use Dynamo.HTTP.Case

  def setup_all do
    Dynamo.under_test(StaticApp)
  end

  def teardown_all(_) do
    Dynamo.under_test(nil)
  end

  @endpoint StaticApp

  test "serves the file" do
    conn = get("/public/fixtures/static/file.txt")
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

  test "returns 404 for unsecure paths" do
    conn = get("/public/fixtures/../fixtures/static/file.txt")
    assert conn.status    == 404
    assert conn.sent_body == "File not served"
  end
end
