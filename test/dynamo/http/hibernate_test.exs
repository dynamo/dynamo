Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.HTTP.HibernateTest do
  use ExUnit.Case, async: true

  import Dynamo.HTTP.Hibernate

  test "hibernates and invokes wake up callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    pid = spawn_link fn ->
      hibernate conn, on_wake_up(&1, &2)
    end

    assert await_wake_up(pid)
  end

  test "hibernates with timeout and invokes wake up callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    pid = spawn_link fn ->
      hibernate conn, 10_000, on_wake_up(&1, &2), on_timeout(&1)
    end

    assert await_wake_up(pid)
  end

  test "hibernates with timeout and invokes time out callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    spawn_link fn ->
      hibernate conn, 10, on_wake_up(&1, &2), on_timeout(&1)
    end

    assert_timeout
  end

  test "awaits and invokes wake up callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    pid = spawn_link fn ->
      await conn, on_wake_up(&1, &2)
    end

    assert await_wake_up(pid)
  end

  test "awaits with timeout and invokes wake up callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    pid = spawn_link fn ->
      await conn, 10_000, on_wake_up(&1, &2), on_timeout(&1)
    end

    assert await_wake_up(pid)
  end

  test "awaits with timeout and invokes time out callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    spawn_link fn ->
      await conn, 10, on_wake_up(&1, &2), on_timeout(&1)
    end

    assert_timeout
  end

  defp conn(verb, path, body // "") do
    Dynamo.HTTP.Test.new.req(verb, path, body)
  end

  defp await_wake_up(pid) do
    pid <- "hello"

    receive do
      :awaken -> true
    after
      10 -> await_wake_up(pid)
    end
  end

  defp assert_timeout do
    receive do
      :timedout -> true
    after
      1_000 -> raise "Expected to time out"
    end
  end

  defp on_wake_up(conn, "hello") do
    conn.assigns[:parent] <- :awaken
  end

  defp on_timeout(conn) do
    conn.assigns[:parent] <- :timedout
  end
end