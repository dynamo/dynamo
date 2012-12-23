Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Connection.UtilsTest do
  use ExUnit.Case, async: true

  alias Dynamo.Connection.Utils, as: Utils

  test "generate a header value" do
    header = Utils.cookie_header("foo", "bar")
    assert header == "foo=bar; HttpOnly"
  end

  test "allows a :path option" do
    header = Utils.cookie_header("foo", "bar", path: "/baz")
    assert header == "foo=bar; path=/baz; HttpOnly"
  end

  test "allows a :domain option" do
    header = Utils.cookie_header("foo", "bar", domain: "google.com")
    assert header == "foo=bar; domain=google.com; HttpOnly"
  end

  test "supports a :secure option" do
    header = Utils.cookie_header("foo", "bar", secure: true)
    assert header == "foo=bar; secure; HttpOnly"
  end

  test "supports a :http_only option, which defaults to true" do
    header = Utils.cookie_header("foo", "bar", http_only: false)
    assert header == "foo=bar"
  end

  test "escapes the key" do
    header = Utils.cookie_header("foo=bar", "baz")
    assert header == "foo%3Dbar=baz; HttpOnly"
  end

  test "escapes the value" do
    header = Utils.cookie_header("foo", "baz=bat")
    assert header == "foo=baz%3Dbat; HttpOnly"
  end

  test "supports max_age" do
    start  = { { 2012, 9, 29 }, { 15, 32, 10 } }
    header = Utils.cookie_header("foo", "bar", max_age: 60, universal_time: start)
    assert header == "foo=bar; expires=Sat, 29 Sep 2012 15:33:10 GMT; max-age=60; HttpOnly"
  end
end