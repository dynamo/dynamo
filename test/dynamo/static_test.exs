defmodule Dynamo.StaticTest do
  use ExUnit.Case

  defmodule App do
    use Dynamo

    def service(conn) do
      conn.resp(200, "Ok")
    end

    config :dynamo,
      otp_app: :dynamo,
      static_root: "../../../../test/fixtures/static",
      static_route: "/static",
      cache_static: true
  end

  defmodule NoCacheApp do
    use Dynamo

    def service(conn) do
      conn.resp(200, "Ok")
    end

    config :dynamo,
      otp_app: :dynamo,
      static_root: "../../../../test/fixtures/static",
      static_route: "/static",
      cache_static: false
  end

  @asset "test/fixtures/static/file.txt"

  setup do
    File.touch!(@asset, { { 2010, 1, 1 }, { 0, 0, 0 } })
    Dynamo.Static.start_link(App)
    :ok
  end

  teardown do
    Dynamo.Static.stop(App)
    :ok
  end

  test "looks up a file" do
    assert Dynamo.Static.lookup(App, "file.txt") ==  "/static/file.txt?63429523200"
    assert Dynamo.Static.lookup(App, "/file.txt") == "/static/file.txt?63429523200"
  end

  test "uses cached version" do
    assert Dynamo.Static.lookup(App, "file.txt") ==  "/static/file.txt?63429523200"
    File.touch!(@asset, { { 2012, 1, 1 }, { 0, 0, 0 } })

    assert Dynamo.Static.lookup(App, "file.txt") == "/static/file.txt?63429523200"
    assert Dynamo.Static.lookup(App, "/file.txt") == "/static/file.txt?63429523200"
  end

  test "does not timestamp directories" do
    assert Dynamo.Static.lookup(App, "/") ==  "/static"
  end

  test "does not cache when configured as so" do
    Dynamo.Static.start_link(NoCacheApp)

    assert Dynamo.Static.lookup(NoCacheApp, "file.txt") ==  "/static/file.txt?63429523200"
    File.touch!(@asset, { { 2012, 1, 1 }, { 0, 0, 0 } })
    assert Dynamo.Static.lookup(NoCacheApp, "file.txt") == "/static/file.txt?63492595200"
  after
    Dynamo.Static.stop(NoCacheApp)
  end
end
