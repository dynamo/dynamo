defmodule DynamoTest do
  use ExUnit.Case, async: true

  defmodule App do
    use Dynamo

    config :dynamo,
      root: Path.expand("fixtures", __DIR__),
      endpoint: DynamoTest,
      static_root: Path.expand("fixtures/public", __DIR__),
      static_route: "/public",
      compile_on_demand: false,
      reload_modules: false,
      source_paths: [Path.expand("fixtures/*", __DIR__)],
      templates_paths: [Path.expand("fixtures/templates", __DIR__)]

    # Test session compilation as well
    config :dynamo,
      session_store: Session.CookieStore,
      session_options:
        [ key: "_foo_session",
          secret: "8RR6p7LJ6YN+vn8br/qZ6R0A1UXWIYRLNuvqvmJw7eLf6/ZTDXljAdNpHQhIzIRF"]
  end

  defmodule ReloadApp do
    use Dynamo

    config :dynamo,
      otp_app: :dynamo,
      endpoint: DynamoTest,
      static_root: false,
      compile_on_demand: true,
      reload_modules: true,
      templates_paths: ["test/fixtures/templates"]
  end

  ## Config

  test "defines root based on otp app" do
    assert ReloadApp.root == String.from_char_data! :code.lib_dir(:dynamo)
  end

  test "defines root based on user config" do
    assert App.root == Path.expand("fixtures", __DIR__)
  end

  ## Filters

  test "adds public filter" do
    file = Path.expand("fixtures/public", __DIR__)
    assert List.first(App.__filters__) == Dynamo.Filters.Static.new("/public", file)
  end

  test "does not add public filter if disabled" do
    refute Enum.any? ReloadApp.__filters__, &match?({ Dynamo.Filters.Static, _, _ }, &1)
  end

  test "adds reloader filter" do
    assert Dynamo.Filters.Loader.new(true, true) in ReloadApp.__filters__
  end

  test "does not add reloader filter if disabled" do
    refute Enum.any? App.__filters__, &match?({ Dynamo.Filters.Loader, _, _ }, &1)
  end

  ## View paths

  test "defines templates paths" do
    assert App.templates_paths == [DynamoTest.App.CompiledTemplates]
    templates = Path.expand("fixtures/templates", __DIR__)
    assert ReloadApp.templates_paths == [templates]
  end
end
