Code.require_file "../../test_helper.exs", __FILE__

defmodule Dynamo.AppTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  defmodule App do
    @dynamo_registration false
    use Dynamo.App
    endpoint Dynamo.AppTest

    config :dynamo,
      public_root: File.expand_path("../../fixtures/public", __FILE__),
      public_route: "/public",
      compile_on_demand: false,
      reload_modules: false
  end

  defmodule ReloadApp do
    @dynamo_registration false
    use Dynamo.App
    endpoint Dynamo.AppTest

    config :dynamo,
      public_root: false,
      compile_on_demand: true,
      reload_modules: true
  end

  @app App

  test "cascades not found accesses" do
    conn = get("/public/file.txt")
    assert conn.status == 200
    assert conn.resp_body == "HELLO"
    assert conn.resp_headers["Content-Type"] == "text/plain"
  end

  test "adds public filter" do
    file = File.expand_path("../../fixtures/public", __FILE__)
    assert Enum.first(App.filters) == Dynamo.Filters.Static.new("/public", file)
  end

  test "does not add public filter if disabled" do
    refute Enum.any? ReloadApp.filters, match?({ Dynamo.Filters.Static, _, _ }, &1)
  end

  test "adds reloader filter" do
    assert Dynamo.Filters.Reloader.new(true, true) in ReloadApp.filters
  end

  test "does not add reloader filter if disabled" do
    refute Enum.any? App.filters, match?({ Dynamo.Filters.Reloader, _, _ }, &1)
  end
end
