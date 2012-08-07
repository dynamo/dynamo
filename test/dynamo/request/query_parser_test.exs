Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Request.QueryParserTest do
  use ExUnit.Case

  import Dynamo.Request.QueryParser

  test "parse simple queries" do
    params = [{ "foo", "bar" }, { "baz", "bat" }]
    assert parse(params) == params
  end

  test "parse one-level nested query" do
    params = [{ "users[name]", "hello" }]
    assert parse(params) == [ { "users", [{ "name", "hello" }] } ]

    params = [{ "users[name]", "hello" }, { "users[age]", "17" }]
    assert parse(params) == [ { "users", [{ "age", "17" }, { "name", "hello" }] } ]
  end
end