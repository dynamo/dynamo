defmodule Dynamo.Connection.QueryParserTest do
  use ExUnit.Case, async: true

  test "parse queries" do
    params = parse "foo=bar&baz=bat"
    assert params["foo"] == "bar"
    assert params["baz"] == "bat"

    params = parse "users[name]=hello&users[age]=17"
    assert params["users"]["name"] == "hello"
    assert params["users"]["age"]  == "17"

    params = parse("my+weird+field=q1%212%22%27w%245%267%2Fz8%29%3F")
    assert params["my weird field"] == "q1!2\"'w$5&7/z8)?"

    assert parse("=")[""] == ""
    assert parse("key=")["key"] == ""
    assert parse("=value")[""] == "value"

    assert parse("foo[]")["foo"]  == []
    assert parse("foo[]=")["foo"] == [""]
    assert parse("foo[]=bar&foo[]=baz")["foo"] == ["bar", "baz"]
    assert parse("foo[]=bar&foo[]=baz")["foo"] == ["bar", "baz"]

    params = parse("foo[]=bar&foo[]=baz&bat[]=1&bat[]=2")
    assert params["foo"] == ["bar", "baz"]
    assert params["bat"] == ["1", "2"]

    assert parse("x[y][z]=1")["x"]["y"]["z"] == "1"
    assert parse("x[y][z][]=1")["x"]["y"]["z"] == ["1"]
    assert parse("x[y][z]=1&x[y][z]=2")["x"]["y"]["z"] == "2"
    assert parse("x[y][z][]=1&x[y][z][]=2")["x"]["y"]["z"] == ["1", "2"]

    assert (parse("x[y][][z]=1")["x"]["y"] |> Enum.first)["z"] == "1"
    assert (parse("x[y][][z][]=1")["x"]["y"] |> Enum.first)["z"] |> Enum.first == "1"
  end

  test "failure on bad queries" do
    assert_raise Dynamo.Connection.QueryParser.ParseError, fn ->
      parse "x[y]=1&x[]=1"
    end

    assert_raise Dynamo.Connection.QueryParser.ParseError, fn ->
      IO.inspect parse "x[y]=1&x[y][][w]=2"
    end

    assert_raise Dynamo.Connection.QueryParser.ParseError, fn ->
      IO.inspect parse "x[y]=1&x=1"
    end

    assert_raise FunctionClauseError, fn ->
      IO.inspect parse "x=%2x"
    end
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
    Dynamo.Connection.QueryParser.parse(binary)
  end

  defp reduce(pairs) do
    Enum.reduce Enum.reverse(pairs), Binary.Dict.new, &Dynamo.Connection.QueryParser.reduce(&1, &2)
  end
end
