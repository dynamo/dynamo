Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Request.QueryParserTest do
  use ExUnit.Case

  test "parse queries" do
    params = parse "foo=bar&baz=bat"
    assert params["foo"] == "bar"
    assert params["baz"] == "bat"

    params = parse "users[name]=hello&users[age]=17"
    assert params["users"]["name"] == "hello"
    assert params["users"]["age"]  == "17"
  end

  test "reduce simple queries" do
    params = reduce [{ "foo", "bar" }, { "baz", "bat" }]
    assert params["foo"] == "bar"
    assert params["baz"] == "bat"
  end

  test "reduce one-level nested query" do
    params = reduce [{ "users[name]", "hello" }]
    assert params["users"]["name"] == "hello"

    params = reduce [{ "users[name]", "hello" }, { "users[age]", "17" }]
    assert params["users"]["name"] == "hello"
    assert params["users"]["age"]  == "17"
  end

  test "reduce query override" do
    params = reduce [{ "foo", "bar" }, { "foo", "baz" }]
    assert params["foo"] == "baz"

    params = reduce [{ "users[name]", "bar" }, { "users[name]", "baz" }]
    assert params["users"]["name"] == "baz"
  end

  test "reduce many-levels nested query" do
    params = reduce [{ "users[name]", "hello" }]
    assert params["users"]["name"] == "hello"

    params = reduce [{ "users[name]", "hello" }, { "users[age]", "17" }, { "users[address][street]", "Mourato" }]
    assert params["users"]["name"]              == "hello"
    assert params["users"]["age"]               == "17"
    assert params["users"]["address"]["street"] == "Mourato"
  end

  test "reduce list query" do
    params = reduce [{ "foo[]", "bar" }, { "foo[]", "baz" }]
    assert params["foo"] == ["bar", "baz"]
  end

  defp parse(binary) do
    Dynamo.Request.QueryParser.parse(binary)
  end

  defp reduce(pairs) do
    Enum.reduce Enum.reverse(pairs), Binary.Dict.new, fn({ k, v }, acc) ->
      Dynamo.Request.QueryParser.reduce(k, v, acc)
    end
  end
end