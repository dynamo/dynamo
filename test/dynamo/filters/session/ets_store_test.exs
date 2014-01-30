defmodule Dynamo.Filters.Session.ETSStoreTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  import Dynamo.HTTP.Session

  setup do
    :ets.new(:dynamo_session_store, [:set, :public, :named_table])
    :ok
  end

  teardown do
    :ets.delete(:dynamo_session_store)
    :ok
  end

  @key "_session"

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

  test "does not resend cookie if it did not change even if session was writen to" do
    conn = session.prepare(conn(:GET, "/hello"))
    conn = conn.fetch(:session)
    conn = put_session(conn, :foo, :bar).send(200, "OK")
    assert { @key, id, _ } = List.keyfind(conn.resp_cookies, @key, 0)

    conn = conn(:GET, "/hello").put_req_cookie(@key, id)
    conn = session.prepare(conn).fetch(:session)
    conn = put_session(conn, :foo, :baz).send(200, "OK")
    refute List.keyfind(conn.resp_cookies, @key, 0)
  end

  test "sends a another cookie when renew is requested" do
    conn = session.prepare(conn(:GET, "/hello"))
    conn = conn.fetch(:session)
    conn = put_session(conn, :foo, :bar).send(200, "OK")
    assert { @key, id, _ } = List.keyfind(conn.resp_cookies, @key, 0)

    conn = conn(:GET, "/hello").put_req_cookie(@key, id)
    conn = session.prepare(conn).fetch(:session)
    conn = configure_session(conn, :renew, true).send(200, "OK")
    assert { @key, other_id, _ } = List.keyfind(conn.resp_cookies, @key, 0)
    assert id != other_id
  end

  defp session(opts \\ []) do
    Dynamo.Filters.Session.new(Dynamo.Filters.Session.ETSStore,
      [key: @key, table: :dynamo_session_store] ++ opts)
  end
end
