defmodule Dynamo.Filters.SessionTest do
  defmodule SessionApp do
    use Dynamo.Router

    filter Dynamo.Filters.Session.new(
      Dynamo.Filters.Session.CookieStore,
      [key: "_my_app_session", secret: String.duplicate("1234567890", 8)])

    prepare do
      conn.fetch(:session)
    end

    get "/put_session" do
      put_session(conn, :hello, "world").send(200, "OK")
    end

    get "/get_session" do
      conn.send(200, get_session(conn, :hello))
    end
  end

  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  @endpoint SessionApp

  test "gets/puts the session" do
    conn = get("/put_session")
    assert { "_my_app_session", _, _ } = List.keyfind(conn.resp_cookies, "_my_app_session", 0)

    conn = get(conn, "/get_session")
    assert conn.sent_body == "world"
  end
end
