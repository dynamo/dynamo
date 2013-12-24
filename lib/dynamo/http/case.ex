defmodule Dynamo.HTTP.Case do
  @moduledoc %S"""
  A bunch of helpers to make it easy to test Dynamos and routers.

  By default, these helpers are macros that dispatch directly
  to the registered endpoint. Here is an example:

      defmodule MyAppTest do
        use ExUnit.Case
        use Dynamo.HTTP.Case

        test :root_route do
          conn = get("/")
          assert conn.sent_body =~ %r/somevalue/
        end
      end

  The default dynamo used in tests is `Dynamo.under_Test`.
  This can be changed in a specific test case using `@endpoint`:

      defmodule CustomRouterTest do
        use ExUnit.Case
        use Dynamo.HTTP.Case

        @endpoint CustomRouter

        test :route do
          conn = get("/route")
          assert conn.sent_body =~ %r/somevalue/
        end
      end

  The connection used in such tests is the `Dynamo.Connection.Test`
  which provides some test specific function.

  ## Testing with sequential requests

  In some cases, the same test may request different endpoints:

      test :session do
        conn = get("/put_session")
        assert conn.sent_body =~ %r/somevalue/

        conn = get(conn, "/set_session")
        assert conn.sent_body =~ %r/othervalue/
      end

  The example above will automatically work, since
  `get`/`post`/`put`/`patch`/`delete`/`options` recycles the connection before
  each request.

  When recycled, all response information previously set in
  the connection is cleaned and all cookies are moved from
  the response to the request. This allows state to be passed
  in between the different requests.

  Notice though that recycling will clean up any information
  set in the connection:

      test :session do
        conn = get("/put_session")
        assert conn.sent_body =~ %r/somevalue/

        conn = conn.assign(:foo, :bar)
        conn = get(conn, "/set_session")
        assert conn.sent_body =~ %r/othervalue/
      end

  In the example above, the assign `:foo` set before the request
  won't be visible in the endpoint since it will be cleaned up.
  This can be fixed by explicitly cleaning up the request:

      conn = conn.recycle.assign(:foo, :bar)

  If the connection was already recycled, it won't be recycled once again.

  Finally, notice that all `get`/`post`/`put`/`patch`/`delete`/`options` macros
  are simply a proxy to `process/4`. So in case you want to dispatch
  to different dynamos at the same time, `process/4` may be useful.
  """

  @doc false
  defmacro __using__(_) do
    quote do
      @endpoint Dynamo.under_test
      import unquote(__MODULE__)
      import Dynamo.HTTP.Cookies
      import Dynamo.HTTP.Session
    end
  end

  @doc """
  Returns a connection built with the given method, path and body.
  """
  def conn(method, path, body // "") do
    Dynamo.Connection.Test.new(method, path, body)
  end

  @doc """
  Does a GET request to the given path:

      get("/foo")
      get(conn, "/foo")

  """
  defmacro get(arg1, arg2 // nil) do
    do_method :GET, arg1, arg2
  end

  @doc """
  Does a POST request to the given path and optionally body:

      post("/foo")
      post(conn, "/foo")
      post(conn, "/foo", "test body") # POSTs to `/foo` with `test body` body
      post(conn, "/foo", [{"foo", "bar"}]) # POSTs to `/foo` with `foo=bar` body

  """
  defmacro post(arg1, arg2 // nil) do
    if is_list(arg2) do
      do_method :POST, arg1, URI.encode_query(arg2)
    else
      do_method :POST, arg1, arg2
    end
  end

  @doc """
  Does a PUT request to the given path:

      put("/foo")
      put(conn, "/foo")

  """
  defmacro put(arg1, arg2 // nil) do
    do_method :PUT, arg1, arg2
  end

  @doc """
  Does a PATCH request to the given path:

      patch("/foo")
      patch(conn, "/foo")

  """
  defmacro patch(arg1, arg2 // nil) do
    do_method :PATCH, arg1, arg2
  end

  @doc """
  Does a DELETE request to the given path:

      delete("/foo")
      delete(conn, "/foo")

  """
  defmacro delete(arg1, arg2 // nil) do
    do_method :DELETE, arg1, arg2
  end

  @doc """
  Does a OPTIONS request to the given path:

      options("/foo")
      options(conn, "/foo")

  """
  defmacro options(arg1, arg2 // nil) do
    do_method :OPTIONS, arg1, arg2
  end

  defp do_method(method, arg1, nil) do
    quote do
      unquote(__MODULE__).process @endpoint, unquote(method), unquote(arg1)
    end
  end

  defp do_method(method, arg1, arg2) do
    quote do
      unquote(__MODULE__).process @endpoint, unquote(arg1), unquote(method), unquote(arg2)
    end
  end

  @doc """
  Writes a session cookie according to the current store to
  be used in the next request. This is the preferred way to
  set the session before a request.
  """
  def put_session_cookie(conn, session) do
    config = conn.main.config[:dynamo]
    store  = config[:session_store]
    opts   = store.setup config[:session_options]
    value  = store.put_session(nil, session, opts)
    conn.put_req_cookie(opts[:key], value)
  end

  @doc """
  Requests the given `endpoint` with the given `method` and `path`.
  And verifies if the endpoint returned a valid connection.

  ## Examples

      process MyDynamo, :get, "/foo"
      process MyDynamo, conn, :get, "/foo"

  """
  def process(endpoint, conn, method, path // nil)

  def process(endpoint, conn, method, path) when is_tuple(conn) do
    conn = if conn.sent_body, do: conn.recycle, else: conn
    do_process endpoint, conn.req(method, path)
  end

  def process(endpoint, method, path, nil) do
    do_process endpoint, Dynamo.Connection.Test.new(method, path)
  end

  def process(endpoint, path, method, body) when is_binary(path) do
    do_process endpoint, conn(method, path, body)
  end

  defp do_process(endpoint, conn) do
    conn = endpoint.service(conn)

    if not is_tuple(conn) or not function_exported?(elem(conn, 0), :state, 1) do
      raise "#{inspect endpoint}.service did not return a connection, got #{inspect conn}"
    end

    if conn.state == :unset and conn.already_sent? do
      raise "#{inspect endpoint}.service sent a response back but there was an exception and the response was lost (the exception was logged)"
    end

    conn
  end
end
