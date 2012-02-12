Code.require_file "../../test_helper", __FILE__

defmodule Dynamo::RouterTest do
  require Dynamo::Router, as: R
  use ExUnit::Case

  def test_split_single_segment do
    assert_equal ['foo'], R.split('/foo')
    assert_equal ['foo'], R.split('foo')
  end

  def test_split_with_more_than_one_segment do
    assert_equal ['foo', 'bar'], R.split('/foo/bar')
    assert_equal ['foo', 'bar'], R.split('foo/bar')
  end

  def test_split_removes_trailing_slash do
    assert_equal ['foo', 'bar'], R.split('/foo/bar/')
    assert_equal ['foo', 'bar'], R.split('foo/bar/')
  end
end