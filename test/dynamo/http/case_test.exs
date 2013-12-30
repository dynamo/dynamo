defmodule Dynamo.HTTP.CaseTest do
  defmodule SessionApp do
    use Dynamo
    use Dynamo.Router

    config :dynamo,
      session_store: Session.CookieStore,
      session_options:
        [ key: "_my_app_session", secret: String.duplicate("1234567890", 8)]

    prepare do
      conn.fetch(:session)
    end

    get "/get_session" do
      conn.send(200, get_session(conn, :hello))
    end

    post "/test_post" do
      conn = conn.fetch :body
      conn.send 200, conn.req_body
    end
  end

  use ExUnit.Case
  use Dynamo.HTTP.Case

  setup_all do
    Dynamo.under_test(SessionApp)
    :ok
  end

  teardown_all do
    Dynamo.under_test(nil)
    :ok
  end

  @endpoint SessionApp

  test "gets the session put in manually" do
    conn = put_session_cookie conn(:get, "/"), hello: "world"
    conn = get(conn, "/get_session")
    assert conn.sent_body == "world"
  end

  test "sees the request body from a post" do
    conn = post("/test_post", "test body")
    assert conn.sent_body == "test body"
  end

  test "sees the request body from a post with HashDict" do
    conn = post("/test_post", [{"foo", "bar"}, {"bar", "foo"}])
    assert conn.sent_body == "foo=bar&bar=foo"
  end
end
