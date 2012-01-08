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

  def test_branch_with_literal do
    { 'foo', [ { :endpoint, :END } ] } = branch('foo', :END)
  end

  def test_merge_distinct do
    [
      { 'foo', [ {:endpoint, :A } ] },
      { 'bar', [ {:endpoint, :B } ] }
    ] = merge('foo', 'bar')
  end

  def test_merge_nested_similar do
    [
      { 'foo', [
        { '/', [
          { 'bar', [ {:endpoint, :A } ] },
          { 'baz', [ {:endpoint, :B } ] }
        ] }
      ] }
    ] = merge('foo/bar', 'foo/baz')
  end

  def test_merge_different_endpoints do
    [
      { 'foo', [
        { '/', [
          { 'bar', [
            { :endpoint, :A },
            { :endpoint, :B }
          ] }
        ] }
      ] }
    ] = merge('foo/bar', 'foo/bar')
  end

  defp branch(string, endpoint) do
    GTG.branch Parser.parse(string), endpoint
  end

  defp merge(a, b) do
    a = GTG.branch Parser.parse(a), :A
    b = GTG.branch Parser.parse(b), :B
    GTG.merge [a], b
  end
end