defmodule Dynamo.HTTP.Case do
  @moduledoc %B"""
  A bunch of helpers to make it easy to test Dynamo applications.

  By default, these helpers are macros that dispatch directly
  to the register application. Here is an example:

      defmodule MyAppTest do
        use ExUnit.Case
        use Dynamo.HTTP.Case
        
        test :root_route do
          conn = get("/")
          assert conn.resp_body =~ %r/somevalue/
        end
      end

  The default dynamo used in tests is `Dynamo.under_Test`.
  This can be changed in a specific test case using `@app`:
  
      defmodule CustomRouterTest do
        use ExUnit.Case
        use Dynamo.HTTP.Case
    
        @app CustomRouter
    
        test :route do
          conn = get("/route")
          assert conn.resp_body =~ %r/somevalue/
        end
      end

  The connection used in such tests is the `Dynamo.HTTP.Test`
  which provides some test specific function. Check it for
  more documentation.

  In case you want to test different apps, you can use the
  lower-level `process/3` function. All helpers provided
  in this module contain a lint function that asserts the
  endpoint returned a valid response.
  """

  @doc false
  defmacro __using__(_) do
    quote do
      @app Dynamo.under_test
      import unquote(__MODULE__)
    end
  end

  @doc """
  A simple macro that expands to:

      process(@app, :GET, path)

  """
  defmacro get(path) do
    do_method :GET, path
  end

  @doc """
  A simple macro that expands to:

      process(@app, :POST, path)

  """
  defmacro post(path) do
    do_method :POST, path
  end

  @doc """
  A simple macro that expands to:

      process(@app, :PUT, path)

  """
  defmacro put(path) do
    do_method :PUT, path
  end

  @doc """
  A simple macro that expands to:

      process(@app, :DELETE, path)

  """
  defmacro delete(path) do
    do_method :DELETE, path
  end

  defp do_method(method, path) do
    quote do
      unquote(__MODULE__).process @app, unquote(method), unquote(path)
    end
  end

  @doc """
  Access the given `app` with the given `method` and `path`.
  After access, it verifies the app returned a valid connection.
  """
  def process(app, method, path) do
    conn = app.service Dynamo.HTTP.Test.new.req(method, path)

    if not is_tuple(conn) or not function_exported?(elem(conn, 0), :state, 1) do
      raise "#{inspect app}.service did not return a connection, got #{inspect conn}"
    end

    if conn.state == :unset do
      raise "#{inspect app}.service returned a connection that did not respond yet"
    end

    conn
  end
end