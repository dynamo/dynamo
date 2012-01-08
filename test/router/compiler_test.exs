Code.require_file "../../test_helper", __FILE__

defmodule Dynamo::Router::CompilerTest do
  use ExUnit::Case

  defmodule Router::Test1 do
    Dynamo::Router.compile __MODULE__, [
      { '/foo/bar/', { :GET, 1 } },
      { 'foo/baz', { :GET, 2 } },
      { 'foo', { :GET, 3 } },
      { '', { :GET, 4 } }
    ]
  end

  def test_compiled_simple do
    { :ok, 1, [] } = Router::Test1.recognize_route(:GET, 'foo/bar', [])
    { :ok, 2, [] } = Router::Test1.recognize_route(:GET, 'foo/baz', [])
    { :ok, 3, [] } = Router::Test1.recognize_route(:GET, 'foo', [])
    { :ok, 4, [] } = Router::Test1.recognize_route(:GET, '', [])
  end

  def test_compiled_path do
    { :ok, 1, [] } = Router::Test1.recognize_route(:GET, '/foo/bar/', [])
    { :ok, 2, [] } = Router::Test1.recognize_route(:GET, '/foo/baz/', [])
    { :ok, 3, [] } = Router::Test1.recognize_route(:GET, '/foo', [])
    { :ok, 4, [] } = Router::Test1.recognize_route(:GET, '/', [])
  end
end