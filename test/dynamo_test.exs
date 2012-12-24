Code.require_file "../test_helper.exs", __FILE__

defmodule DynamoTest do
  use ExUnit.Case, async: true

  defmodule App do
    use Dynamo

    config :dynamo,
      endpoint: DynamoTest,
      static_root: File.expand_path("../fixtures/public", __FILE__),
      static_route: "/public",
      compile_on_demand: false,
      reload_modules: false,
      source_paths: [File.expand_path("../fixtures/*", __FILE__)],
      templates_paths: [File.expand_path("../fixtures/templates", __FILE__)]

    # Test session compilation as well
    config :dynamo,
      session_store: CookieStore,
      session_options:
        [ key: "_foo_session",
          secret: "8RR6p7LJ6YN+vn8br/qZ6R0A1UXWIYRLNuvqvmJw7eLf6/ZTDXljAdNpHQhIzIRF"]
  end

  defmodule ReloadApp do
    use Dynamo

    config :dynamo,
      endpoint: DynamoTest,
      static_root: false,
      compile_on_demand: true,
      reload_modules: true,
      templates_paths: [File.expand_path("../fixtures/templates", __FILE__)]
  end

  ## Config

  test "removes templates from source paths" do
    templates_path = File.expand_path("../fixtures/templates", __FILE__)
    refute templates_path inlist App.config[:dynamo][:source_paths]
  end

  ## Filters

  test "adds public filter" do
    file = File.expand_path("../fixtures/public", __FILE__)
    assert Enum.first(App.__filters__) == Dynamo.Filters.Static.new("/public", file)
  end

  test "does not add public filter if disabled" do
    refute Enum.any? ReloadApp.__filters__, match?({ Dynamo.Filters.Static, _, _ }, &1)
  end

  test "adds reloader filter" do
    assert Dynamo.Reloader.Filter.new(true, true) inlist ReloadApp.__filters__
  end

  test "does not add reloader filter if disabled" do
    refute Enum.any? App.__filters__, match?({ Dynamo.Reloader.Filter, _, _ }, &1)
  end

  ## View paths

  test "defines templates paths" do
    assert App.templates_paths == [DynamoTest.App.CompiledTemplates]
    templates = File.expand_path("../fixtures/templates", __FILE__)
    assert ReloadApp.templates_paths == [templates]
  end
end
