Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Request.QueryParserTest do
  use ExUnit.Case

  import Dynamo.Request.QueryParser

  test "parse simple queries" do
    params = parse [{ "foo", "bar" }, { "baz", "bat" }]
    assert params["foo"] == "bar"
    assert params["baz"] == "bat"
  end

  test "parse one-level nested query" do
    params = parse [{ "users[name]", "hello" }]
    assert params["users"]["name"] == "hello"

    params = parse [{ "users[name]", "hello" }, { "users[age]", "17" }]
    assert params["users"]["name"] == "hello"
    assert params["users"]["age"]  == "17"
  end
end