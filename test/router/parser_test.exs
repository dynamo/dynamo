Code.require_file "../../test_helper", __FILE__

defmodule Dynamo::Router::ParserTest do
  use ExUnit::Case
  require Dynamo::Router::Parser, as: Parser

  def test_parse do
    { :cat,
      { :terminal, 'foo' },
      { :cat,
        { :terminal, '/' },
        { :terminal, 'bar' }
      }
    } = Parser.parse('foo/bar')
  end
end