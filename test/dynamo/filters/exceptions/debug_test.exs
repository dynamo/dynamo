defmodule Dynamo.Filters.Exceptions.DebugTest do
  defmodule ExceptionsApp do
    use Dynamo
    use Dynamo.Router

    config :dynamo,
      source_paths: ["filters/exceptions"],
      exceptions_editor: "editor://__FILE__:__LINE__",
      root: Path.expand("../..", __DIR__) # test/dynamo
  end

  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  @endpoint Dynamo.Filters.Exceptions.Debug

  test "prints exception name and title" do
    conn = @endpoint.service(conn)
    assert conn.status == 500
    assert conn.sent_body =~ ~r"ArgumentError"
    assert conn.sent_body =~ ~r"bad argument"
  end

  test "shows shortcut file path, module and function" do
    conn = @endpoint.service(conn)
    assert conn.sent_body =~ ~r"\bfilters/exceptions/debug_test.exs\b"
    assert conn.sent_body =~ ~r"\bDynamo.Filters.Exceptions.DebugTest\b"
    assert conn.sent_body =~ ~r"\bstacktrace/0\b"
  end

  test "handles __MODULE__ stacktraces" do
    conn = @endpoint.service(conn)
    assert conn.sent_body =~ ~r"Dynamo.Filters.Exceptions.DebugTest \(module\)"
  end

  test "show editor links" do
    conn = @endpoint.service(conn)
    assert conn.sent_body =~ ~r"editor://#{URI.encode __ENV__.file}:#{16}"
  end

  test "shows snippets if they are part of the source_paths" do
    conn = @endpoint.service(conn)
    assert conn.sent_body =~ ~r"@endpoint Dynamo.Filters.Exceptions.Debug"
  end

  test "does not show snippets if they are not part of the source_paths" do
    conn = @endpoint.service(conn)
    assert conn.sent_body =~ ~r"No code snippets for code outside the Dynamo"
  end

  defp conn do
    conn(:get, "/oops").main(ExceptionsApp).put_assign(:exception,
      { 500, :error, ArgumentError[message: "bad argument"], stacktrace })
  end

  defp stacktrace do
    [ { Dynamo.Filters.Exceptions.DebugTest, :stacktrace, 0, [file: __ENV__.file, lilne: __ENV__.line] },
      { Dynamo.Filters.Exceptions.DebugTest, :stacktrace, 0, [file: __ENV__.file, line: 1000] },
      { Dynamo.Filters.Exceptions.DebugTest, :__MODULE__, 0, [file: __ENV__.file, line: 16] },
      { Dynamo, :__MODULE__, 0, [file: "lib/dynamo.ex", line: 1] } ]
  end
end
