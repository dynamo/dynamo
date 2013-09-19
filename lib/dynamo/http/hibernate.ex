defmodule Dynamo.HTTP.Hibernate do
  @moduledoc """
  Conveniences that allows a connection to hibernate or wait
  a given amount or an unlimited amount of time.

  Such conveniences are useful when a connection needs to be
  kept open (because of long polling, websockets or streaming)
  but you don't want to keep the current erlang process active
  all times.

  As such, waiting through small intervals or hibernating through
  long intervals is convenient.

  ## Examples

  There are two main functions defined by this module: `hibernate`
  and `await`. They can receive either 2 arguments, with the
  connection and a callback to be invoked on wake up:

    hibernate(conn, &on_wake_up(&1, &2))
    await(conn, &on_wake_up(&1, &2))

  Or 4 arguments, in which a timeout and a callback to be invoked
  on timeout must also be present:

    hibernate(conn, timeout, &on_wake_up(&1, &2), &on_timeout(&1))
    await(conn, timeout, &on_wake_up(&1, &2), &on_timeout(&1))

  Besides an positive integer or `:infinity`, this module also
  allows `:keep` as timeout value. This is useful to preserve
  a previously set timeout value.
  """

  @key :dynamo_timeref

  @doc """
  Hibernates the current process until a message is received.
  The `on_wake_up` callback is invoked with the `conn` and the
  received message on wake up.

  For more information on hibernation, check:
  http://www.erlang.org/doc/man/erlang.html#hibernate-3
  """
  def hibernate(conn, on_wake_up) when is_function(on_wake_up, 2) do
    clear_timeout(conn)
    __loop__ conn, on_wake_up, :no_timeout_callback, 0, fn ->
      :erlang.hibernate(__MODULE__, :__loop__, [conn, on_wake_up, :no_timeout_callback])
    end
  end

  @doc """
  Hibernates the current process until a message is received
  but also sets a timeout for hibernation time.

  The `on_wake_up` callback is invoked with the `conn` and the
  received message on wake up. A `on_timeout` callback is
  invoked when it times out.

  For more information on hibernation, check:
  http://www.erlang.org/doc/man/erlang.html#hibernate-3
  """
  def hibernate(conn, timeout, on_wake_up, on_timeout) when (is_integer(timeout) or timeout in [:infinity, :keep]) and
      is_function(on_wake_up, 2) and is_function(on_timeout, 1) do
    clear_timeout(conn, timeout)
    conn = set_timeout(conn, timeout)
    __loop__ conn, on_wake_up, on_timeout, 0, fn ->
      :erlang.hibernate(__MODULE__, :__loop__, [conn, on_wake_up, on_timeout])
    end
  end

  @doc """
  Sleeps the current process until a message is received.
  The `on_wake_up` callback is invoked with the `conn` and the
  received message on wake up.
  """
  def await(conn, on_wake_up) when is_function(on_wake_up, 2) do
    clear_timeout(conn)
    __loop__ conn, on_wake_up, :no_timeout_callback, 0, fn ->
      __loop__(conn, on_wake_up, :no_timeout_callback)
    end
  end

  @doc """
  Sleeps the current process until a message is received
  but also sets a timeout.

  The `on_wake_up` callback is invoked with the `conn` and the
  received message on wake up. A `on_timeout` callback is
  invoked when it times out.
  """
  def await(conn, timeout, on_wake_up, on_timeout) when (is_integer(timeout) or timeout in [:infinity, :keep]) and
      is_function(on_wake_up, 2) and is_function(on_timeout, 1) do
    clear_timeout(conn, timeout)
    conn = set_timeout(conn, timeout)
    __loop__ conn, on_wake_up, on_timeout, 0, fn ->
      __loop__(conn, on_wake_up, on_timeout)
    end
  end

  @doc false
  def __loop__(conn, on_wake_up, on_timeout) do
    __loop__(conn, on_wake_up, on_timeout, :infinity, :no_after_callback)
  end

  defp __loop__(conn, on_wake_up, on_timeout, timer, callback) do
    ref = conn.private[@key]
    receive do
      { :timeout, ^ref, __MODULE__ } when is_function(on_timeout) ->
        on_timeout.(conn)
      { :timeout, older_ref, __MODULE__ } when is_reference(older_ref) ->
        __loop__(conn, on_wake_up, on_timeout, timer, callback)
      msg ->
        on_wake_up.(msg, conn)
    after
      timer ->
        callback.()
    end
  end

  defp clear_timeout(conn, :keep), do: conn
  defp clear_timeout(conn, _),     do: clear_timeout(conn)

  defp clear_timeout(conn) do
    ref = conn.private[@key]
    ref && :erlang.cancel_timer(ref)
  end

  defp set_timeout(conn, timeout) when timeout in [:infinity, :keep], do: conn

  defp set_timeout(conn, timeout) do
    ref = :erlang.start_timer(timeout, self(), __MODULE__)
    conn.put_private(@key, ref)
  end
end
