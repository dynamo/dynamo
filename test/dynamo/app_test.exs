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
      templates_paths: [File.expand_path("../../fixtures/templates", __FILE__)]
  end

  defmodule ReloadApp do
    use Dynamo.App
    endpoint Dynamo.AppTest

    config :dynamo,
      static_root: false,
      compile_on_demand: true,
      reload_modules: true,
      templates_paths: [File.expand_path("../../fixtures/templates", __FILE__)]
  end

  ## Config

  test "removes templates from source paths" do
    templates_path = File.expand_path("../../fixtures/templates", __FILE__)
    refute templates_path in App.config[:dynamo][:source_paths]
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

  test "defines templates paths" do
    assert App.templates_paths == [Dynamo.AppTest.App.CompiledTemplates]
    templates = File.expand_path("../../fixtures/templates", __FILE__)
    assert ReloadApp.templates_paths == [Dynamo.Templates.PathFinder.new(templates)]
  end
end
