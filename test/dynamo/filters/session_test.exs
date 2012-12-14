Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Filters.SessionTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  import Dynamo.HTTP.Session

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

  test "can mark the cookie as secure on the filter" do
    conn = session(secure: true).prepare(conn(:GET, "/hello"))
    conn = conn.fetch(:session)
    conn = put_session(conn, :foo, :bar).send(200, "OK")
    assert { @key, _, opts } = List.keyfind(conn.resp_cookies, @key, 0)
    assert opts[:secure]
  end

  test "can mark the cookie as secure during a request" do
    conn = session.prepare(conn(:GET, "/hello"))
    conn = configure_session(conn.fetch(:session), :secure, true)
    conn = put_session(conn, :foo, :bar).send(200, "OK")
    assert { @key, _, opts } = List.keyfind(conn.resp_cookies, @key, 0)
    assert opts[:secure]
  end

  test "does not set a cookie if session was not writen to" do
    conn = session.prepare(conn(:GET, "/hello"))
    conn = conn.fetch(:session).send(200, "OK")
    refute List.keyfind(conn.resp_cookies, @key, 0)
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

  defp session(opts // []) do
    Dynamo.Filters.Session.new(Dynamo.Filters.Session.CookieStore,
      [key: @key, secret: String.duplicate("1234567890", 8)] ++ opts)
  end
end
