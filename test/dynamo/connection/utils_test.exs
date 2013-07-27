defmodule Dynamo.Connection.UtilsTest do
  use ExUnit.Case, async: true

  alias Dynamo.Connection.Utils, as: Utils

  test "generate a header value" do
    header = Utils.cookie_header("foo", "bar")
    assert header == "foo=bar; path=/; HttpOnly"
  end

  test "allows a :path option" do
    header = Utils.cookie_header("foo", "bar", path: "/baz")
    assert header == "foo=bar; path=/baz; HttpOnly"
  end

  test "allows a :domain option" do
    header = Utils.cookie_header("foo", "bar", domain: "google.com")
    assert header == "foo=bar; path=/; domain=google.com; HttpOnly"
  end

  test "supports a :secure option" do
    header = Utils.cookie_header("foo", "bar", secure: true)
    assert header == "foo=bar; path=/; secure; HttpOnly"
  end

  test "supports a :http_only option, which defaults to true" do
    header = Utils.cookie_header("foo", "bar", http_only: false)
    assert header == "foo=bar; path=/"
  end

  test "supports max_age" do
    start  = { { 2012, 9, 29 }, { 15, 32, 10 } }
    header = Utils.cookie_header("foo", "bar", max_age: 60, universal_time: start)
    assert header == "foo=bar; path=/; expires=Sat, 29 Sep 2012 15:33:10 GMT; max-age=60; HttpOnly"
  end
end