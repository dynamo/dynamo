defmodule Dynamo.HTTP.CookiesTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case
  import Dynamo.HTTP.Cookies

  test :get_cookie do
    conn = conn(:GET, "/").put_req_cookie(:foo, "bar")
             .put_req_cookie(:baz, "bat").fetch(:cookies)
    assert get_cookie(conn, :foo) == "bar"
    assert get_cookie(conn, "baz") == "bat"
    conn
  end

  test :get_cookies do
    conn = conn(:GET, "/").put_req_cookie(:foo, "bar")
             .put_req_cookie(:baz, "bat").fetch(:cookies)
    assert get_cookies(conn) == Binary.Dict.new([baz: "bat", foo: "bar"])

    conn = put_cookie(conn, :baz, "new")
    assert get_cookies(conn) == Binary.Dict.new([baz: "new", foo: "bar"])

    conn
  end

  test :put_cookies do
    conn = conn(:GET, "/")
    assert conn.resp_cookies == []

    conn = put_cookie(conn, :foo, :bar, path: "/hello")
    assert conn.resp_cookies == [{ "foo", "bar", path: "/hello" }]
  end

  test :delete_cookie do
    conn = conn(:GET, "/").put_req_cookie(:foo, "bar")
             .put_req_cookie(:baz, "bat").fetch(:cookies)
    assert get_cookie(conn, "foo") == "bar"
    assert get_cookie(conn, "baz") == "bat"

    conn = put_cookie(conn, :foo, :new)
    assert get_cookie(conn, "foo") == "new"
    assert get_cookie(conn, "baz") == "bat"

    conn = delete_cookie(conn, :foo)
    assert get_cookie(conn, :foo) == nil
  end
end
