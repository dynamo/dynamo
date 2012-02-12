Code.require_file "../../test_helper", __FILE__

defmodule Dynamo::RouterTest::Macros do
  defmacro assert_quoted(left, right) do
    quote do
      assert_equal quote(do: unquote(left)), unquote(right)
    end
  end
end

defmodule Dynamo::RouterTest do
  import Dynamo::RouterTest::Macros
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

  def test_quote_single_segment_no_variable do
    assert_quoted ['foo'], R.generate_match('/foo')
    assert_quoted ['foo'], R.generate_match('foo')
  end

  def test_quote_with_variable do
    assert_quoted ['foo', id], R.generate_match('/foo/:id')
    assert_quoted ['foo', username], R.generate_match('foo/:username')
  end

  def test_quote_with_string_plus_variable do
    assert_quoted ['foo', 'bar-' ++ id], R.generate_match('/foo/bar-:id')
    assert_quoted ['foo', 'bar' ++ username], R.generate_match('foo/bar:username')
  end

end
