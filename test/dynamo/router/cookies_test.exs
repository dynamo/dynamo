Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Router.CookiesTest do
  use ExUnit.Case, async: true
  import Dynamo.Router.Cookies

  test :get_cookie do
    conn = conn(:GET, "/").req_cookies(foo: "bar", baz: "bat").fetch(:cookies)
    assert get_cookie(conn, :foo) == "bar"
    assert get_cookie(conn, "baz") == "bat"
    conn
  end

  test :set_cookies do
    conn = conn(:GET, "/")
    assert conn.resp_cookies == []

    conn = put_cookie(conn, :foo, :bar, path: "/hello")
    assert conn.resp_cookies == [{ "foo", "bar", path: "/hello" }]
  end

  test :delete_cookie do
    conn = conn(:GET, "/").req_cookies(foo: "bar", baz: "bat").fetch(:cookies)
    assert get_cookie(conn, "foo") == "bar"
    assert get_cookie(conn, "baz") == "bat"

    conn = put_cookie(conn, :foo, :new)
    assert get_cookie(conn, "foo") == "new"
    assert get_cookie(conn, "baz") == "bat"

    conn = delete_cookie(conn, :foo)
    assert get_cookie(conn, :foo) == nil
  end

  defp conn(verb, path, body // "") do
    Dynamo.HTTP.Test.new.req(verb, path, body)
  end
end
