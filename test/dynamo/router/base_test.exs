Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Router.BaseTest do
  use ExUnit.Case

  defrecord Mock, forward_to: nil do
    def forward_to(at, target, conn) do
      conn.forward_to({ at, target })
    end
  end

  defmodule Sample0 do
    use Dynamo.Router

    def service(_conn) do
      :from_sample_0
    end

    get "/nested/:arg" do
      arg
    end

    get "/with_request" do
      conn
    end
  end

  defmodule Sample1 do
    use Dynamo.Router

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

    forward "/10", to: Sample0
    forward ["11", "deep"], to: Sample0

    def not_found(_conn) do
      404
    end
  end

  def test_dispatch_single_segment do
    assert Sample1.dispatch(:GET, ["1","bar"], {}) == 1
  end

  def test_dispatch_dynamic_segment do
    assert Sample1.dispatch(:GET, ["2","baz"], {}) == "baz"
  end

  def test_dispatch_dynamic_segment_with_prefix do
    assert Sample1.dispatch(:GET, ["3","bar-baz"], {}) == "baz"
  end

  def test_dispatch_glob_segment do
    assert Sample1.dispatch(:GET, ["4", "baz", "baaz"], {}) == ["baz", "baaz"]
  end

  def test_dispatch_glob_segment_with_prefix do
    assert ["bar-baz", "baaz"] == Sample1.dispatch(:GET, ["5", "bar-baz", "baaz"], {})
  end

  def test_dispatch_custom_route do
    assert Sample1.dispatch(:GET, ["6", "foo"], {}) == 200
  end

  def test_dispatch_not_found do
    assert Sample1.dispatch(:GET, ["100", "foo"], {}) == 404
  end

  def test_dispatch_with_guards do
    assert Sample1.dispatch(:GET, ["7", "a"], {}) == "a"
    assert Sample1.dispatch(:GET, ["7", "ab"], {}) == "ab"
    assert Sample1.dispatch(:GET, ["7", "abc"], {}) == "abc"
    assert Sample1.dispatch(:GET, ["7", "abcd"], {}) == 404
  end

  def test_dispatch_wrong_verb do
    assert Sample1.dispatch(:POST, ["1","bar"], {}) == 404
  end

  def test_dispatch_any_verb do
    assert Sample1.dispatch(:GET,  ["8", "foo"], {}) == 8
    assert Sample1.dispatch(:PUT,  ["8", "foo"], {}) == 8
    assert Sample1.dispatch(:POST, ["8", "foo"], {}) == 8
  end

  def test_pointing_to_another_endpoint do
    assert Sample1.dispatch(:PUT, ["9", "foo"], {}) == :from_sample_0
  end

  def test_forwarding_to_another_endpoint do
    assert Sample1.dispatch(:GET, ["10", "nested", "match"], Mock.new) == "match"
  end

  def test_forwarding_to_another_endpoint_with_explicit_path do
    assert Sample1.dispatch(:GET, ["11", "deep", "nested", "match"], Mock.new) == "match"
  end

  def test_forwarding_to_another_endpoint_annotates_the_request do
    conn = Sample1.dispatch(:GET, ["10", "with_request"], Mock.new)
    assert conn.forward_to == { ["with_request"], Dynamo.Router.BaseTest.Sample0 }
  end
end