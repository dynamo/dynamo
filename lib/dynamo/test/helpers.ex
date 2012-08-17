defmodule Dynamo.Test.Helpers do
  defmacro get(path) do
    do_method __CALLER__, :GET, path
  end

  defmacro post(path) do
    do_method __CALLER__, :POST, path
  end

  defmacro put(path) do
    do_method __CALLER__, :PUT, path
  end

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

  def process(app, method, path) do
    app.service Dynamo.Test.Connection.new.req(method, path)
  end
end