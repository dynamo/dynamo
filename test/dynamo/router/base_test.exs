Code.require_file "../../../test_helper", __FILE__

defmodule Dynamo.Router.BaseTest do
  use ExUnit.Case, async: true

  import Dynamo.Test.Helpers

  defmodule Sample0 do
    use Dynamo.Router

    def service(_conn) do
      :from_sample_0
    end

    get "/nested/:arg" do
      arg
    end
  end

  defmodule Sample1 do
    use Dynamo.Router

    get "/" do
      :root
    end

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

    get "/with_request" do
      conn
    end

    def not_found(_conn) do
      404
    end
  end

  defmodule RootSample do
    use Dynamo.Router

    forward "/", to: Sample1
  end

  def test_dispatch_root do
    assert process(Sample1, :GET, "/") == :root
    assert process(RootSample, :GET, "/") == :root
    assert process(RootSample, :GET, "/8/foo") == 8
  end

  @app Sample1

  def test_dispatch_single_segment do
    assert get("/1/bar") == 1
  end

  def test_dispatch_dynamic_segment do
    assert get("/2/baz") == "baz"
  end

  def test_dispatch_dynamic_segment_with_prefix do
    assert get("/3/bar-baz") == "baz"
  end

  def test_dispatch_glob_segment do
    assert get("/4/baz/baaz") == ["baz", "baaz"]
  end

  def test_dispatch_glob_segment_with_prefix do
    assert get("/5/bar-baz/baaz") == ["bar-baz", "baaz"]
  end

  def test_dispatch_custom_route do
    assert get("/6/foo") == 200
  end

  def test_dispatch_not_found do
    assert get("/100/foo") == 404
  end

  def test_dispatch_with_guards do
    assert get("/7/a")    == "a"
    assert get("/7/ab")   == "ab"
    assert get("/7/abc")  == "abc"
    assert get("/7/abcd") == 404
  end

  def test_dispatch_wrong_verb do
    assert post("/1/bar") == 404
  end

  def test_dispatch_any_verb do
    assert get("/8/foo") == 8
    assert post("/8/foo") == 8
    assert put("/8/foo") == 8
    assert delete("/8/foo") == 8
  end

  def test_pointing_to_another_endpoint do
    assert put("/9/foo") == :from_sample_0
  end

  def test_forwarding_to_another_endpoint do
    assert get("/10/nested/match") == "match"
  end

  def test_forwarding_to_another_endpoint_with_explicit_path do
    assert get("/11/deep/nested/match") == "match"
  end

  def test_forwarding_on_root do
    assert get("/1/bar") == 1
  end
end