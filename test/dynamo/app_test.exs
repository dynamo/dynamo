Code.require_file "../../test_helper.exs", __FILE__

defmodule Dynamo.AppTest do
  use ExUnit.Case, async: true
  use Dynamo.HTTP.Case

  defmodule App do
    use Dynamo.App
    endpoint Dynamo.AppTest

    config :dynamo,
      static_root: File.expand_path("../../fixtures/public", __FILE__),
      static_route: "/public",
      compile_on_demand: false,
      reload_modules: false,
      source_paths: [File.expand_path("../../fixtures/*", __FILE__)],
      view_paths: [File.expand_path("../../fixtures/views", __FILE__)]
  end

  defmodule ReloadApp do
    use Dynamo.App
    endpoint Dynamo.AppTest

    config :dynamo,
      static_root: false,
      compile_on_demand: true,
      reload_modules: true,
      view_paths: [File.expand_path("../../fixtures/views", __FILE__)]
  end

  ## Config

  test "removes views from source paths" do
    view_path = File.expand_path("../../fixtures/views", __FILE__)
    refute view_path in App.config[:dynamo][:source_paths]
  end

  ## Filters

  test "adds public filter" do
    file = File.expand_path("../../fixtures/public", __FILE__)
    assert Enum.first(App.filters) == Dynamo.Filters.Static.new("/public", file)
  end

  test "does not add public filter if disabled" do
    refute Enum.any? ReloadApp.filters, match?({ Dynamo.Filters.Static, _, _ }, &1)
  end

  test "adds reloader filter" do
    assert Dynamo.Reloader.Filter.new(true, true) in ReloadApp.filters
  end

  test "does not add reloader filter if disabled" do
    refute Enum.any? App.filters, match?({ Dynamo.Reloader.Filter, _, _ }, &1)
  end

  ## View paths

  test "defines view paths" do
    assert App.view_paths == [Dynamo.AppTest.App.CompiledViews]
    views = File.expand_path("../../fixtures/views", __FILE__)
    assert ReloadApp.view_paths == [Dynamo.View.PathFinder.new(views)]
  end
end
