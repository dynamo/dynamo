Code.require_file "../../test_helper", __FILE__

defmodule Dynamo::RouterTest::Sample0 do
  use Dynamo::Router

  def service(_req, _res) do
    :from_sample_0
  end

  get "/nested/:arg" do
    arg
  end
end

defmodule Dynamo::RouterTest::Sample1 do
  use Dynamo::Router

  get "/1/bar" do
    1
  end

  get "/2/:bar" do
    bar
  end

  get "/3/bar-:bar" do
    bar
  end

  get "/4/*bar" do
    bar
  end

  get "/5/bar-*bar" do
    bar
  end

  get ["6", "foo"] do
    200
  end

  get "/7/:foo" when size(foo) <= 3 do
    foo
  end

  match "/8/foo" do
    8
  end

  put "/9/foo", to: Sample0

  mount Sample0, at: "/10"
  mount Sample0, at: ["11", "deep"]

  def not_found(_request, _response) do
    404
  end
end

defmodule Dynamo::RouterTest do
  use ExUnit::Case

  def test_dispatch_single_segment do
    assert_equal 1, Sample1.dispatch(:GET, ["1","bar"], {}, {})
  end

  def test_dispatch_dynamic_segment do
    assert_equal "baz", Sample1.dispatch(:GET, ["2","baz"], {}, {})
  end

  def test_dispatch_dynamic_segment_with_prefix do
    assert_equal "baz", Sample1.dispatch(:GET, ["3","bar-baz"], {}, {})
  end

  def test_dispatch_glob_segment do
    assert_equal ["baz", "baaz"], Sample1.dispatch(:GET, ["4", "baz", "baaz"], {}, {})
  end

  def test_dispatch_glob_segment_with_prefix do
    assert_equal ["bar-baz", "baaz"], Sample1.dispatch(:GET, ["5", "bar-baz", "baaz"], {}, {})
  end

  def test_dispatch_custom_route do
    assert_equal 200, Sample1.dispatch(:GET, ["6", "foo"], {}, {})
  end

  def test_dispatch_not_found do
    assert_equal 404, Sample1.dispatch(:GET, ["100", "foo"], {}, {})
  end

  def test_dispatch_with_guards do
    assert_equal "a",   Sample1.dispatch(:GET, ["7", "a"], {}, {})
    assert_equal "ab",  Sample1.dispatch(:GET, ["7", "ab"], {}, {})
    assert_equal "abc", Sample1.dispatch(:GET, ["7", "abc"], {}, {})
    assert_equal 404,   Sample1.dispatch(:GET, ["7", "abcd"], {}, {})
  end

  def test_dispatch_wrong_verb do
    assert_equal 404, Sample1.dispatch(:POST, ["1","bar"], {}, {})
  end

  def test_dispatch_any_verb do
    assert_equal 8, Sample1.dispatch(:GET,  ["8", "foo"], {}, {})
    assert_equal 8, Sample1.dispatch(:PUT,  ["8", "foo"], {}, {})
    assert_equal 8, Sample1.dispatch(:POST, ["8", "foo"], {}, {})
  end

  def test_pointing_to_another_endpoint do
    assert_equal :from_sample_0, Sample1.dispatch(:PUT, ["9", "foo"], {}, {})
  end

  def test_mounting_another_endpoint do
    assert_equal "match", Sample1.dispatch(:GET, ["10", "nested", "match"], {}, {})
  end

  def test_mounting_another_endpoint_with_explicit_path do
    assert_equal "match", Sample1.dispatch(:GET, ["11", "deep", "nested", "match"], {}, {})
  end
end