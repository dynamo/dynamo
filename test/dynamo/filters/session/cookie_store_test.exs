defmodule Dynamo.Filters.Session.CookieStoreTest do
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
    assert_raise Dynamo.Connection.UnfetchedError, fn ->
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

  test "fails when cookie exceeds 4kb" do
    conn = session.prepare(conn(:GET, "/hello"))
    conn = put_session(conn.fetch(:session), :key, String.duplicate("0123456789", 480))
    assert_raise Dynamo.Filters.Session.CookieOverflowError, fn ->
      conn.send(200, "OK")
    end
  end

  test "persists session in between requests" do
    conn = session.prepare(conn(:GET, "/hello"))
    conn = conn.fetch(:session)
    conn = put_session(conn, :foo, :bar).send(200, "OK")
    assert { @key, id, _ } = List.keyfind(conn.resp_cookies, @key, 0)

    conn = conn(:GET, "/hello").put_req_cookie(@key, id)
    conn = session.prepare(conn).fetch(:session)
    assert get_session(conn, :foo) == :bar
  end

  test "does not resend cookie if it did not change" do
    conn = session.prepare(conn(:GET, "/hello"))
    conn = conn.fetch(:session)
    conn = put_session(conn, :foo, :bar).send(200, "OK")
    assert { @key, id, _ } = List.keyfind(conn.resp_cookies, @key, 0)

    conn = conn(:GET, "/hello").put_req_cookie(@key, id)
    conn = session.prepare(conn).fetch(:session).send(200, "OK")
    refute List.keyfind(conn.resp_cookies, @key, 0)
  end

  test "resends cookie if it changed" do
    conn = session.prepare(conn(:GET, "/hello"))
    conn = conn.fetch(:session)
    conn = put_session(conn, :foo, :bar).send(200, "OK")
    assert { @key, id, _ } = List.keyfind(conn.resp_cookies, @key, 0)

    conn = conn(:GET, "/hello").put_req_cookie(@key, id)
    conn = session.prepare(conn).fetch(:session)
    conn = put_session(conn, :foo, :baz).send(200, "OK")
    assert { @key, other_id, _ } = List.keyfind(conn.resp_cookies, @key, 0)
    assert other_id != id
  end

  defp session(opts \\ []) do
    Dynamo.Filters.Session.new(Dynamo.Filters.Session.CookieStore,
      [key: @key, secret: String.duplicate("1234567890", 8)] ++ opts)
  end
end
