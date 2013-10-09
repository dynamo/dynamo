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

    match ["8", "foo"] do
      conn.resp_body("OK").assign :value, 8
    end

    put "/9/foo", to: Sample0

    forward "/10", to: Sample0
    forward ["11", "deep"], to: Sample0
    forward "/12/:var", to: Sample0

    match _ do
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

  test "dispatch root" do
    assert process(Sample1, :GET, "/").assigns[:value] == :root
    assert process(RootSample, :GET, "/").assigns[:value] == :root
    assert process(RootSample, :GET, "/8/foo").assigns[:value] == 8
  end

  @endpoint Sample1

  test "dispatch single segment" do
    assert get("/1/bar").assigns[:value] == 1
  end

  test "dispatch dynamic segment" do
    conn = get("/2/value")
    assert conn.assigns[:value] == "value"
    assert conn.route_params[:bar] == "value"
  end

  test "dispatch dynamic segment with prefix" do
    assert get("/3/bar-baz").assigns[:value] == "baz"
  end

  test "dispatch glob segment" do
    assert get("/4/baz/baaz").assigns[:value] == ["baz", "baaz"]
  end

  test "dispatch glob segment with prefix" do
    assert get("/5/bar-baz/baaz").assigns[:value] == ["bar-baz", "baaz"]
  end

  test "dispatch custom route" do
    assert get("/6/foo").assigns[:value] == 200
  end

  test "dispatch not found" do
    conn = post("/100/foo")
    assert conn.status == 404
    assert conn.resp_body == "OOPS"
  end

  test "dispatch with guards" do
    assert get("/7/a").assigns[:value]    == "a"
    assert get("/7/ab").assigns[:value]   == "ab"
    assert get("/7/abc").assigns[:value]  == "abc"

    conn = get("/7/abcd")
    assert conn.status == 404
    assert conn.resp_body == "OOPS"
  end

  test "dispatch wrong verb" do
    conn = post("/1/bar")
    assert conn.status == 404
    assert conn.resp_body == "OOPS"
  end

  test "dispatch any verb" do
    assert get("/8/foo").assigns[:value] == 8
    assert post("/8/foo").assigns[:value] == 8
    assert put("/8/foo").assigns[:value] == 8
    assert patch("/8/foo").assigns[:value] == 8
    assert delete("/8/foo").assigns[:value] == 8
    assert options("/8/foo").assigns[:value] == 8
  end

  test "forwarding to another endpoint" do
    assert get("/10").assigns[:value] == :root
    assert get("/10/nested/match").assigns[:value] == "match"
  end

  test "forwarding to another endpoint with explicit path" do
    assert get("/11/deep/nested/match").assigns[:value] == "match"
  end

  test "forwarding to another endpoint with dynamic path" do
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

  test "forwarding on root" do
    assert get("/1/bar").assigns[:value] == 1
  end

  test "forwarding on root with dynamic path and params" do
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
