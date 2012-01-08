Code.require_file "../../test_helper", __FILE__

defmodule Dynamo::Router::GTGTest do
  use ExUnit::Case

  refer Dynamo::Router::GTG, as: GTG
  refer Dynamo::Router::Parser, as: Parser

  def test_branch do
    { 'foo', [
      {'/', [
        {'bar', [ { :endpoint, :END } ] }
      ] }
    ] } = branch('foo/bar', :END)
  end

  defp branch(string, endpoint) do
    GTG.branch Parser.parse(string), endpoint
  end
end