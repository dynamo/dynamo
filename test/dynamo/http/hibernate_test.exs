defmodule Dynamo.HTTP.HibernateTest do
  use ExUnit.Case, async: true

  use Dynamo.HTTP.Case
  import Dynamo.HTTP.Hibernate

  setup do: flush

  def flush do
    receive do
      _ -> flush
    after
      0 -> :ok
    end
  end

  test "hibernates and invokes wake up callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    pid = spawn_link fn ->
      hibernate conn, &on_wake_up(&1, &2)
    end

    assert await_wake_up(pid)
  end

  test "hibernates with timeout and invokes wake up callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    pid = spawn_link fn ->
      hibernate conn, 10_000, &on_wake_up(&1, &2), &on_timeout(&1)
    end

    assert await_wake_up(pid)
  end

  test "hibernates with infinite timeout and invokes wake up callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    pid = spawn_link fn ->
      hibernate conn, :infinity, &on_wake_up(&1, &2), &on_timeout(&1)
    end

    assert await_wake_up(pid)
  end

  test "hibernates with timeout and invokes time out callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    spawn_link fn ->
      hibernate conn, 10, &on_wake_up(&1, &2), &on_timeout(&1)
    end

    assert_timeout
  end

  test "hibernates with timeout and invokes time out callback even with received message" do
    conn = conn(:GET, "/").assign(:parent, self())

    pid = spawn_link fn ->
      hibernate conn, 500, fn(msg, conn) ->
        on_wake_up(msg, conn)
        hibernate conn, :keep, &on_wake_up(&1, &2), &on_timeout(&1)
      end, &on_timeout(&1)
    end

    assert await_wake_up(pid)
    assert_timeout
  end

  test "awaits and invokes wake up callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    pid = spawn_link fn ->
      await conn, &on_wake_up(&1, &2)
    end

    assert await_wake_up(pid)
  end

  test "awaits with timeout and invokes wake up callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    pid = spawn_link fn ->
      await conn, 10_000, &on_wake_up(&1, &2), &on_timeout(&1)
    end

    assert await_wake_up(pid)
  end

  test "awaits with timeout and invokes time out callback" do
    conn = conn(:GET, "/").assign(:parent, self())

    spawn_link fn ->
      await conn, 10, &on_wake_up(&1, &2), &on_timeout(&1)
    end

    assert_timeout
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

  defp on_wake_up("hello", conn) do
    conn.assigns[:parent] <- :awaken
  end

  defp on_timeout(conn) do
    conn.assigns[:parent] <- :timedout
  end
end
