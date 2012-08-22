defmodule Dynamo.Router.TestHelpers do
  @moduledoc """
  A bunch of helpers to make it easy to test Dynamo routers.

  Some of these helpers are macros that dispatch directly to
  the registered `@app`. Here is an example:

      defmodule MyAppTest do
        use ExUnit.Case, async: true
        import Dynamo.Router.TestHelpers

        @app MyApp
        
        test :root_route do
          conn = get("/")
          assert conn.resp_body =~ %r/some-value/
        end
      end

  The connection used in such tests is the
  `Dynamo.HTTP.Test`. In case you want to
  test different apps, you can use the lower-level
  `process/3` function.

  Notice that those helpers contain a lint function
  that asserts the endpoint returned a valid response.
  """

  @doc """
  A simple macro that expands to:

      process(@app, :GET, path)

  """
  defmacro get(path) do
    do_method __CALLER__, :GET, path
  end

  @doc """
  A simple macro that expands to:

      process(@app, :POST, path)

  """
  defmacro post(path) do
    do_method __CALLER__, :POST, path
  end

  @doc """
  A simple macro that expands to:

      process(@app, :PUT, path)

  """
  defmacro put(path) do
    do_method __CALLER__, :PUT, path
  end

  @doc """
  A simple macro that expands to:

      process(@app, :DELETE, path)

  """
  defmacro delete(path) do
    do_method __CALLER__, :DELETE, path
  end

  defp do_method(env, method, path) do
    unless Module.read_attribute(env.module, :app) do
      raise "In order to a #{method}, you need to set the app under test with @app"
    end

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

    if not is_tuple(conn) or not function_exported?(elem(conn, 1), :state, 1) do
      raise "#{inspect app}.service did not return a connection, got #{inspect conn}"
    end

    if conn.state == :unset do
      raise "#{inspect app}.service returned a connection that did not respond yet"
    end

    conn
  end
end