Code.require_file "../../test_helper", __FILE__

defmodule Dynamo::Router::ScannerTest do
  use ExUnit::Case
  refer Dynamo::Router::Scanner, as: Scanner

  def test_scan do
    [
      { :literal, 'foo' },
      { :slash, '/' },
      { :literal, 'bar' }
    ] = Scanner.scan('foo/bar')
  end
end