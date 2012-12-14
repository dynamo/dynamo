Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Filters.SessionTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  import Dynamo.Router.Session

  @key "_session"

  test "allow session to be fetched" do
    conn = session.prepare conn(:GET, "/hello")
    conn = conn.fetch(:session)
    assert get_session(conn, :foo) == nil
  end

  test "fails when session is not fetched" do
    assert_raise Dynamo.HTTP.UnfetchedError, fn ->
      conn = session.prepare conn(:GET, "/hello")
      get_session(conn, :foo)
    end
  end

  test "sets the cookie before sending it if session was writen to" do
    conn = session.prepare(conn(:GET, "/hello"))
    conn = conn.fetch(:session)
    conn = put_session(conn, :foo, :bar).send(200, "OK")
    assert { @key, _, _ } = List.keyfind(conn.resp_cookies, @key, 0)
  end

  test "persists session in between requests" do
    conn = session.prepare(conn(:GET, "/hello"))
    conn = conn.fetch(:session)
    conn = put_session(conn, :foo, :bar)

    conn = send_and_recycle(conn, @key)
    conn = session.prepare(conn).fetch(:session)
    assert get_session(conn, :foo) == :bar
  end

  defp send_and_recycle(conn, key) do
    { ^key, value, _ } = List.keyfind(conn.send(200, "OK").resp_cookies, key, 0)
    conn(conn.method, conn.path_info).put_req_cookie(key, value)
  end

  defp session do
    Dynamo.Filters.Session.new(Dynamo.Filters.Session.CookieStore, key: @key)
  end
end
