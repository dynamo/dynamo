Code.require_file "../../../test_helper.exs", __FILE__

defmodule Dynamo.Router.BaseTest do
  defmodule Sample0 do
    use Dynamo.Router

    get "/" do
      conn.resp_body("OK").assign :value, :root
    end

    get "/nested/:arg" do
      conn.resp_body("OK").assign :value, arg
    end
  end

  defmodule Sample1 do
    use Dynamo.Router

    get "/" do
      conn.resp_body("OK").assign :value, :root
    end

    get "/1/bar" do
      conn.resp_body("OK").assign :value, 1
    end

    get "/2/:bar" do
      conn.resp_body("OK").assign :value, bar
    end

    get "/3/bar-:bar" do
      conn.resp_body("OK").assign :value, bar
    end

    get "/4/*bar" do
      conn.resp_body("OK").assign :value, bar
    end

    get "/5/bar-*bar" do
      conn.resp_body("OK").assign :value, bar
    end

    get ["6", "foo"] do
      conn.resp_body("OK").assign :value, 200
    end

    get "/7/:foo" when size(foo) <= 3 do
      conn.resp_body("OK").assign :value, foo
    end

    match "/8/foo" do
      conn.resp_body("OK").assign :value, 8
    end

    put "/9/foo", to: Sample0

    forward "/10", to: Sample0
    forward ["11", "deep"], to: Sample0
    forward "/12/:var", to: Sample0

    def not_found(conn) do
      conn.status(404).resp_body("OOPS")
    end
  end

  defmodule RootSample do
    use Dynamo.Router

    prepare do: conn.fetch(:params)

    forward "/", to: Sample1
  end

  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  def test_dispatch_root do
    assert process(Sample1, :GET, "/").assigns[:value] == :root
    assert process(RootSample, :GET, "/").assigns[:value] == :root
    assert process(RootSample, :GET, "/8/foo").assigns[:value] == 8
  end

  @endpoint Sample1

  def test_dispatch_single_segment do
    assert get("/1/bar").assigns[:value] == 1
  end

  def test_dispatch_dynamic_segment do
    conn = get("/2/value")
    assert conn.assigns[:value] == "value"
    assert conn.route_params[:bar] == "value"
  end

  def test_dispatch_dynamic_segment_with_prefix do
    assert get("/3/bar-baz").assigns[:value] == "baz"
  end

  def test_dispatch_glob_segment do
    assert get("/4/baz/baaz").assigns[:value] == ["baz", "baaz"]
  end

  def test_dispatch_glob_segment_with_prefix do
    assert get("/5/bar-baz/baaz").assigns[:value] == ["bar-baz", "baaz"]
  end

  def test_dispatch_custom_route do
    assert get("/6/foo").assigns[:value] == 200
  end

  def test_dispatch_not_found do
    conn = post("/100/foo")
    assert conn.status == 404
    assert conn.sent_body == "OOPS"
  end

  def test_dispatch_with_guards do
    assert get("/7/a").assigns[:value]    == "a"
    assert get("/7/ab").assigns[:value]   == "ab"
    assert get("/7/abc").assigns[:value]  == "abc"

    conn = get("/7/abcd")
    assert conn.status == 404
    assert conn.sent_body == "OOPS"
  end

  def test_dispatch_wrong_verb do
    conn = post("/1/bar")
    assert conn.status == 404
    assert conn.sent_body == "OOPS"
  end

  def test_dispatch_any_verb do
    assert get("/8/foo").assigns[:value] == 8
    assert post("/8/foo").assigns[:value] == 8
    assert put("/8/foo").assigns[:value] == 8
    assert delete("/8/foo").assigns[:value] == 8
  end

  def test_forwarding_to_another_endpoint do
    assert get("/10").assigns[:value] == :root
    assert get("/10/nested/match").assigns[:value] == "match"
  end

  def test_forwarding_to_another_endpoint_with_explicit_path do
    assert get("/11/deep/nested/match").assigns[:value] == "match"
  end

  def test_forwarding_to_another_endpoint_with_dynamic_path do
    conn = get("/12/hello")
    assert conn.assigns[:value] == :root
    assert conn.route_params[:var] == "hello"
    assert conn.fetch(:params).params[:var] == "hello"

    conn = get("/12/hello?var=other")
    assert conn.assigns[:value] == :root
    assert conn.route_params[:var] == "hello"
    assert conn.fetch(:params).params[:var] == "hello"
  end

  @endpoint RootSample

  def test_forwarding_on_root do
    assert get("/1/bar").assigns[:value] == 1
  end

  def test_forwarding_on_root_with_dynamic_path_and_params do
    conn = get("/12/hello")
    assert conn.assigns[:value] == :root
    assert conn.route_params[:var] == "hello"
    assert conn.params[:var] == "hello"

    conn = get("/12/hello?var=other")
    assert conn.assigns[:value] == :root
    assert conn.route_params[:var] == "hello"
    assert conn.params[:var] == "hello"
  end
end
